#From UC-Irvine Machine learning repository
All_sites <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_7_12_2017.csv") 
#Print first lines
head(All_sites)
#add column names
str(All_sites)
All_sites$month <- as.factor(All_sites$month)
#2. Know your data----------------------
#Iris scatter plots using ggvis
All_sites %>% ggvis(~GPP, ~ET, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~VPD, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~Precip, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~Tair, fill=~site) %>% layer_points()


#Test overall correlation between 'Petal.Length' and 'Petal.Width'
cor(All_sites$GPP, All_sites$ET, use="complete.obs")

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
return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

#Normalize data 
str(All_sites)
#All_sites <- subset(All_sites, select = -c(X))
All_sites <- All_sites[c("X", "date", "site", "IGBP", "month", "GPP", "Precip", "Tair", "VPD", "daylength",
                         "precip", "srad", "swe", "tmax", "tmin", "vp", "LST", "NDVI")]
All_sites[complete.cases(All_sites),]
All_normalized <- as.data.frame(lapply(All_sites[6:18], normalize))
str(All_normalized)
str(All_normalized)
All <- cbind(All_sites[1:5], All_normalized)
All_normalized <- All[complete.cases(All_normalized),]

#8. Machine Learning with "caret"
#Split data into training and test set ( alittle different) - ratio 75/25
#Create index to split based on labels
index <- createDataPartition(All_normalized$GPP, p=0.75, list=FALSE)
index
#Subset training set
All_sites.training <- All_normalized[index,]
All_sites.test <- All_normalized[-index,]
str(All_normalized)
#Overview of algorithms supported by caret function
names(getModelInfo())
head(All_normalized)
cols1 <- c(5, 7:18)
cols2<- c(3, 5, 7:18)
cols3<- c(5, 7:8, 10:11, 14:18)
cols4<- c(5, 10:18)
head(All_sites.training)
#Train a model (trying both KNN and random forest)
model_rf1 <- train(All_sites.training[, cols], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf2 <- train(All_sites.training[,cols2], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf3 <-train(All_sites.training[,cols3], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf4 <- train(All_sites.training[,cols4], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)

#Predict based on model
predictions <- predict(object=model_rf4, All_sites.test[,cols4])
table(predictions)
pred1 <- as.numeric(predictions)
cor(pred1, All_sites.test[,6])
model_rf4
RF4 <- model_rf4$finalModel
varImp(RF4)
varImpPlot(RF4, type=2)
lb1 <- paste("R^2 == ", "0.77")
RMSE1 <- paste("RMSE==", "0.085")

qplot(pred1, All_sites.test[,6]) + 
  geom_point(shape=19, colour="tomato2", size=4)+
  geom_abline(intercept = 0, slope = 1)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 0.1, y = 0.7, size = 5, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 0.1, y = 0.6, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - 4 (RS/Daymet Only)")  



#Try out preprocessing methods
index_cat <- createDataPartition(All_sites$month, p=0.75, list=FALSE)
index_cat
#Subset training set
All_sites.training_cat <- All_sites[index_cat,]
All_sites.test_cat <- All_sites[-index_cat,]
All_sites.training_cat
All_sites.test_cat
model_knn_cat <- train(All_sites.training_cat[,1:6], All_sites.training_cat[,7], method='rf',
                       preProcess=c("center", "scale"))
predictions <- predict(object=model_knn_cat, All_sites.test_cat[,1:6], type="raw")
confusionMatrix(predictions, All_sites.test_cat[,7])
str(predictions)
str(All_sites.test_cat[,7])
