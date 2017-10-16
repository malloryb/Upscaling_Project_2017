library(dplyr)
library(ggvis)
library(caret)
library(randomForest)

#From UC-Irvine Machine learning repository
All_sites <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_7_12_2017.csv") 
#Print first lines
head(All_sites)
#add column names
str(All_sites)
All_sites$month <- as.factor(All_sites$month)
#2. Know your data----------------------
#Iris scatter plots using ggvis and some correlations
All_sites %>% ggvis(~GPP, ~ET, fill=~site) %>% layer_points()
All_sites %>% ggvis(~Precip, ~precip, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~VPD, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~Precip, fill=~site) %>% layer_points()
All_sites %>% ggvis(~GPP, ~Tair, fill=~site) %>% layer_points()

cor(All_sites$Precip, All_sites$precip, use="complete.obs")
cor(All_sites$Tair, All_sites$tmax, use="complete.obs")
cor(All_sites$VPD, All_sites$vp, use="complete.obs")

#3. Prepare your data----------------------
#Normalization makes data easier for the RF algorithm to learn
#Two types of normalization: 
#example normalization (normalize each case individually)
#feature normalization (adjust each feature in the same way across all cases)
#Normalization a good idea when one attribute has a wide range of values compared to others
#User-defined normalization function
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

#Normalize quantitative data 
str(All_sites)
All_sites <- All_sites[c("X", "date", "site", "IGBP", "month", "GPP", "Precip", "Tair", "VPD", "daylength",
                         "precip", "srad", "swe", "tmax", "tmin", "vp", "LST", "NDVI")]
All_sites[complete.cases(All_sites),]
All_normalized <- as.data.frame(lapply(All_sites[6:18], normalize))
str(All_normalized)
str(All_normalized)
All <- cbind(All_sites[1:5], All_normalized)
All_normalized <- All[complete.cases(All_normalized),]

#Machine Learning with "caret"
#Split data into training and test set (a little different) - ratio 75/25
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
#Model using: month, precip, Tair, VPD, daylength, precip, srad, swe, tmax, tmin, vp, LST, NDVI
cols1 <- c(5, 7:18)
#Model using: same as cols1 + site component
cols2<- c(3, 5, 7:18)
#Model using: same as as cols1 except: VPD, srad, swe 
cols3<- c(5, 7:8, 10:11, 14:18)
#Model using: same as cols1 except: Precip, Tair, VPD
cols4<- c(5, 10:18)
#Model using: ONLY LST and NDVI
cols5 <- c(5, 11, 14:16, 18)
head(All_sites.training)
#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
model_rf1 <- train(All_sites.training[, cols1], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf2 <- train(All_sites.training[,cols2], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf3 <-train(All_sites.training[,cols3], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf4 <- train(All_sites.training[,cols4], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf5 <- train(All_sites.training[,cols5], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
model_rf5preproc <- train(All_sites.training[,cols5], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE,  preProcess=c("center", "scale"))
#Predict based on model
predictions <- predict(object=model_rf5, All_sites.test[,cols5])

table(predictions)
pred1 <- as.numeric(predictions)
cor(pred1, All_sites.test[,6])
model_rf5preproc
RF5 <- model_rf5$finalModel
varImp(RF5)
varImpPlot(RF5, type=2)
lb1 <- paste("R^2 == ", "0.73")
RMSE1 <- paste("RMSE==", "0.088")

#Plot predicted vs. measured
qplot(pred1, All_sites.test[,6]) + 
  geom_point(shape=19, colour="tomato2", size=4)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,1)+
  ylim(0,1)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 0.1, y = 0.7, size = 5, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 0.1, y = 0.6, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - 5 (RS/Daymet Only)")  


