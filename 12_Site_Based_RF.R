library(dplyr)
library(ggvis)
library(caret)
library(randomForest)

#From UC-Irvine Machine learning repository
All_sites <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_10_23_2017.csv") 
#Print first lines
head(All_sites)
#add column names
str(All_sites)
All_sites$month <- as.factor(All_sites$month)
All_sites$IGBP_no <- as.factor(All_sites$IGBP_no)
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

#Function to see if any cols have NAs. Now that I'm not standardizing GPP anymore having issues w/ NAs
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

#Normalize quantitative data 
str(All_sites)
All_sites <- All_sites[c("date", "site", "IGBP_no", "month", "Latitude", "Longitude", "GPP", "daylength",
                         "precip", "srad", "swe", "tmax", "tmin", "vp", "LST", "NDVI", "SPEI_1", "SPEI_3", 
                         "SPEI_6", "SPEI_9", "SPEI_12", "SPEI_IGBP", "CCP")]
All_sites <- All_sites[complete.cases(All_sites),]
nacols(All_sites)
head(All_sites)

All_normalized <- as.data.frame(lapply(All_sites[8:16], normalize))
str(All_normalized)
All <- cbind(All_sites[1:7], All_normalized)
All <- cbind(All, All_sites[17:23])
str(All)
All_normalized <- All[complete.cases(All_normalized),]
head(All_normalized)

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
#cols1 <- c(5, 7:18)
#Model using: same as cols1 + site component
#cols2<- c(3, 5, 7:18)
#Model using: same as as cols1 except: VPD, srad, swe 
#cols3<- c(5, 7:8, 10:11, 14:18)
#Model using: same as cols1 except: Precip, Tair, VPD
#cols4<- c(5, 10:18)
#Model with all:
cols1 <- c(3:4, 5:6, 9, 12:23)
head(All_sites.training)
head(All_sites.training[,cols1])
head(All_sites.training[,7:8])

#Model using: precip, NDVI, tmax, tmin,month
cols5 <- c(3:4, 9, 12:13, 16)
head(All_sites.training)
head(All_sites.training[,cols5])
head(All_sites.training[,7:8])
#Model using: Precip, NDVI, tmax, tmin, month, LST, SPEI_1 - SPEI_12
cols6 <- c(4, 9, 12:13, 15:21)
head(All_sites.training[,cols6])

cols7 <- c(4,9,12:13, 15:18, 20)
head(All_sites.training[,cols7])

cols8 <- c(4,9, 12:13, 15:17, 19, 23)
head(All_sites.training[,cols8])

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
#model_rf1 <- train(All_sites.training[, cols1], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf2 <- train(All_sites.training[,cols2], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf3 <-train(All_sites.training[,cols3], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf4 <- train(All_sites.training[,cols4], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)

model_rf1 <- train(All_sites.training[,cols1], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf5 <- train(All_sites.training[,cols5], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf6<- train(All_sites.training[,cols6], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf7<- train(All_sites.training[,cols7], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf8<- train(All_sites.training[,cols8], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)

#model_rf5preproc <- train(All_sites.training[,cols5], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE,  preProcess=c("center", "scale"))
#Predict based on model
predictions <- predict(object=model_rf1, All_sites.test[,cols1])
predictions <- predict(object=model_rf5, All_sites.test[,cols5])
predictions <- predict(object=model_rf6, All_sites.test[,cols6])
predictions <- predict(object=model_rf7, All_sites.test[,cols7])
predictions <- predict(object=model_rf8, All_sites.test[,cols8])

table(predictions)
pred1 <- as.numeric(predictions)
cor(pred1, All_sites.test[,7])

model_rf5
RF1 <- model_rf1$finalModel
RF5 <- model_rf5$finalModel
RF6 <- model_rf6$finalModel
RF7 <- model_rf7$finalModel

varImp(RF1)
varImpPlot(RF1)
varImp(RF5)
varImp(RF6)
varImp(RF7)
RF5
varImpPlot(RF5, type=2)
varImpPlot(RF6, type=2)
varImpPlot(RF7, type=2)
lb1 <- paste("R^2 == ", "0.73")
RMSE1 <- paste("RMSE==", "0.65")
#Plot predicted vs. measured
qplot(pred1, All_sites.test[,7]) + 
  geom_point(shape=19, colour="tomato2", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,7.5)+
  ylim(0,7.5)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 0.5, y = 6, size = 5, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 0.5, y = 5.5, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - 5 (RS/Daymet Only)")  

#Predict based on test raster

#For January 
Jan_2001 <- stack("D:/Upscaling_Project/Gridded_inputs/Jan_2001.tif")

names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
sw <- extent(Jan_2001)
Jan_2001
#This step takes awhile: 11:23am - on personal laptop
Jan_2001_GPP <- predict(Jan_2001, RF5, ext=sw)
plot(Jan_2001_GPP, main="Jan 2001 upscaled GPP", zlim=c(0,7))

#For June
Jun_2001 <- stack("D:/Upscaling_Project/Gridded_Inputs/Jun_2001.tif")
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
sw <- extent(Jun_2001)
Jun_2001_GPP <- predict(Jun_2001, RF5, ext=sw)
Jun_2001_GPP <-raster("F:/Upscaling_Project/Gridded_Inputs/Jun_2001.tif")
plot(Jun_2001_GPP, main="June 2001 upscaled GPP", zlim=c(0,7))

saveRDS(RF5, "F:/Upscaling_Project/Upscaling_Project_2017/RF5_11_8.rds")
readRDS("D:/Upscaling_Project/Upscaling_Project_2017/RF5_10_18.rds")
