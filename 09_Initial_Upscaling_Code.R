#Initial Upscaling for Kendall Grassland Site
#Cleaning up flux data-----------------

#Downloading MODIS data----------------
library(MODISTools)

#just Kendall Grassland - subset by date first
GetDates(Product="MOD13Q1", Lat=31.736527, Long=-109.94188)
#This file would be much longer with multiple sites
modis.subset <- data.frame(lat=31.736527, long=-109.94188)
modis.subset$start.date <- 2004 
modis.subset$end.date <- 2014 

#This takes a bit - 1 full minute for 1 site 
MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1",
               Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"),
               Size = c(1,1))

subset.string <- read.csv(list.files(pattern= ".asc")[1], 
                          header=FALSE, as.is=TRUE)
subset.string[1,]

MODISSummaries(LoadDat = modis.subset, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability",
               QualityThreshold = 0)
















#Machine Learning in R from the following tutorial: 
#https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.1qOXxOY
#Using caret - does KNN method (k-nearest neighbors) but we can change easily to "RF"
library(ggvis)
library(class)
library(gmodels)
library(caret)
library(e1071)
library(randomForest)
#1. Get your data----------------
#Built_in_IRIS_dataset
#From UC-Irvine Machine learning repository
kendall_or <- read.csv("C:/Users/Mallory/Google Drive/Upscaling_Initial/Kendall_Upscaling_2.csv") 
#Print first lines
head(kendall_or)
#add column names
str(kendall_or)
kendall_or$Month <- as.factor(kendall_or$Month)
#2. Know your data----------------------
#Iris scatter plots using ggvis
kendall_or %>% ggvis(~GPP, ~ET, fill=~Month) %>% layer_points()
kendall_or %>% ggvis(~GPP, ~VPD, fill=~Month) %>% layer_points()
kendall_or %>% ggvis(~GPP, ~Precip, fill=~Month) %>% layer_points()
kendall_or %>% ggvis(~GPP, ~Tair, fill=~Month) %>% layer_points()


#Test overall correlation between 'Petal.Length' and 'Petal.Width'
cor(kendall_or$GPP,kendall_or$ET, use="complete.obs")

#3. Where to go now?---------------
#This example: Species is the target variable
#We should also try one of the numerical classess as a target variable


#4. Prepare your workspace--------------
#Get package "class"

#5. Prepare your data----------------------
#Normalization makes data easier for the KNN algorithm to learn
#Two types of normalization: 
#example normalization (normalize each case individually)
#feature normalization (adjust each feature in the same way across all cases)
#Normalization a good idea when one attribute has a wide range of values compared to others
#User-defined normalization function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

#Normalize data 
kendall <- as.data.frame(lapply(kendall_or[4:9], normalize))
summary(kendall)
kendall$month <- as.factor(kendall_or$Month)
str(kendall)


#8. Machine Learning with "caret"
#Split data into training and test set ( alittle different) - ratio 75/25
#Create index to split based on labels
index <- createDataPartition(kendall$GPP, p=0.75, list=FALSE)
index
#Subset training set
kendall.training <- kendall[index,]
kendall.test <- kendall[-index,]

#Overview of algorithms supported by caret function
names(getModelInfo())
#Train a model (trying both KNN and random forest)
model_knn <- train(kendall.training[,2:6], kendall.training[,1], method='knn')
model_reg <-train(kendall.training[,2:6], kendall.training[,1], method='cubist')
model_rf <- train(kendall.training[,2:6], kendall.training[,1], method='rf')
model_ann <- train(kendall.training[,2:6], kendall.training[,1], method='neuralnet')

#Predict based on model
predictions <- predict(object=model_knn, kendall.test[,2:6])
table(predictions)
pred1 <- as.numeric(predictions)
cor(pred1, kendall.test[,1])
model_knn
lb1 <- paste("R^2 == ", "0.825")
RMSE1 <- paste("RMSE==", "0.105")

qplot(pred1, kendall.test[,1]) + 
  geom_point(shape=19, colour="tomato2", size=4)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 0.1, y = 0.7, size = 5, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 0.1, y = 0.6, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Machine Learning - Nearest Neighbor")  
  


#Try out preprocessing methods
index_cat <- createDataPartition(kendall$month, p=0.75, list=FALSE)
index_cat
#Subset training set
kendall.training_cat <- kendall[index_cat,]
kendall.test_cat <- kendall[-index_cat,]
kendall.training_cat
kendall.test_cat
model_knn_cat <- train(kendall.training_cat[,1:6], kendall.training_cat[,7], method='rf',
                   preProcess=c("center", "scale"))
predictions <- predict(object=model_knn_cat, kendall.test_cat[,1:6], type="raw")
confusionMatrix(predictions, kendall.test_cat[,7])
str(predictions)
str(kendall.test_cat[,7])
