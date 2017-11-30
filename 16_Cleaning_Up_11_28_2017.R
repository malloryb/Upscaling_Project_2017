#Cleaning things up for AGU presentation analysis
#Created 11/28/2017
#To-do's
#1) calculate water balance using 'spei' package for site-based RF  (X)
#2) create MAP and MAT rasters from Daymet data and put in gridded input datasets
#3) Add Water Balance rasters to gridded input datasets 
#4) Run site-based RF with proper input variables
#5) Run upscaling for 2001 
#6) Compare seasonal cycles from output to existing datasets

#7) Add all sites and figure out what was that weird site in older versions of the All_Sites.csv file

#1 Calculate water balance using 'spei' package for site-based RF------

library(SPEI)
library(caret)
library(randomForest)

Allsites <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_11_20.csv")
str(Allsites)
#probably should do 'split-apply-combine'

#split 
X <- split(Allsites, Allsites$site)

A  <- X[[1]]
B  <- X[[2]]
C  <- X[[3]]
D  <- X[[4]]
E  <- X[[5]]
FF  <- X[[6]]
G  <- X[[7]]
H  <- X[[8]]
I  <- X[[9]]
J  <- X[[10]]
K  <- X[[11]]
L  <- X[[12]]
M  <- X[[13]]
N  <- X[[14]]
O  <- X[[15]]
P  <- X[[16]]
Q  <- X[[17]]
R  <- X[[18]]
S  <- X[[19]]
TT  <- X[[20]]
U  <- X[[21]]


A$PET <- thornthwaite(A$tmed, A$Latitude[1], na.rm=TRUE)
B$PET <- thornthwaite(B$tmed, B$Latitude[1], na.rm=TRUE)
C$PET <- thornthwaite(C$tmed, C$Latitude[1], na.rm=TRUE)
D$PET <- thornthwaite(D$tmed, D$Latitude[1], na.rm=TRUE)
E$PET <- thornthwaite(E$tmed, E$Latitude[1], na.rm=TRUE)
FF$PET <- thornthwaite(FF$tmed, FF$Latitude[1], na.rm=TRUE)
G$PET <- thornthwaite(G$tmed, G$Latitude[1], na.rm=TRUE)
H$PET <- thornthwaite(H$tmed, H$Latitude[1], na.rm=TRUE)
I$PET <- thornthwaite(I$tmed, I$Latitude[1], na.rm=TRUE)
J$PET <- thornthwaite(J$tmed, J$Latitude[1], na.rm=TRUE)
K$PET <- thornthwaite(K$tmed, K$Latitude[1], na.rm=TRUE)
L$PET <- thornthwaite(L$tmed, L$Latitude[1], na.rm=TRUE)
M$PET <- thornthwaite(M$tmed, M$Latitude[1], na.rm=TRUE)
N$PET <- thornthwaite(N$tmed, N$Latitude[1], na.rm=TRUE)
O$PET <- thornthwaite(O$tmed, O$Latitude[1], na.rm=TRUE)
P$PET <- thornthwaite(P$tmed, P$Latitude[1], na.rm=TRUE)
Q$PET <- thornthwaite(Q$tmed, Q$Latitude[1], na.rm=TRUE)
R$PET <- thornthwaite(R$tmed, R$Latitude[1], na.rm=TRUE)
S$PET <- thornthwaite(S$tmed, S$Latitude[1], na.rm=TRUE)
TT$PET <- thornthwaite(TT$tmed, TT$Latitude[1], na.rm=TRUE)
U$PET <- thornthwaite(U$tmed, U$Latitude[1], na.rm=TRUE)

All_sites2 <- rbind(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,TT,U)
All_sites2$PET <- as.numeric(All_sites2$PET)
All_sites2$wb <- All_sites2$precip - All_sites2$PET  
str(All_sites2)

write.csv(All_sites2, "F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_11_28.csv")

#2 Create MAP and MAT rasters from daymet----------------------------------


Precip <- stack("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Tmax <- stack("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
Tmin <- stack("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")

Tmax[Tmax==-9999] <-NA
Tmin[Tmin==-9999] <-NA
Precip[Precip==-9999] <-NA

pr_sum <- sum(Precip, na.rm=TRUE)
tmax_sum <- sum(Tmax. na.rm=TRUE)
tmin_sum <- sum(Tmin, na.rm=TRUE)

MAP <- pr_sum/204

MAT <- tmax_sum + tmin_sum
MAT <- MAT/(408)

plot(MAP)
plot(MAT)

#3) Add Water Balance rasters to gridded input datasets 
#Read water balance rasters, resample to extent of stack, then add them to the stack
#Existing raster stacks: 


dem <- raster("F:/Upscaling_Project/Gridded_Inputs/Hydro1k/SW_dem.tif")
Jan_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2001.tif"), dem, Jan_2001_wbresample)
names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Feb_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2001.tif"), dem, Feb_2001_wbresample)
names(Feb_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Mar_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2001.tif"), dem, Mar_2001_wbresample)
names(Mar_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Apr_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2001.tif"), dem, Apr_2001_wbresample)
names(Apr_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
May_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2001.tif"), dem, May_2001_wbresample)
names(May_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Jun_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2001.tif"), dem, Jun_2001_wbresample)
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Jul_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2001.tif"), dem, Jul_2001_wbresample)
names(Jul_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Aug_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2001.tif"), dem, Aug_2001_wbresample)
names(Aug_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Sep_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2001.tif"), dem, Sep_2001_wbresample)
names(Sep_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Oct_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2001.tif"), dem, Oct_2001_wbresample)
names(Oct_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Nov_2001<-stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2001.tif"), dem, Nov_2001_wbresample)
names(Nov_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))
Dec_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2001.tif"), dem, Dec_2001_wbresample)
names(Dec_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb"))

writeRaster(Jan_2001,"Jan2001.tif", format="raster")
writeRaster(Feb_2001,"Feb2001.tif", format="raster", overwrite=TRUE)
writeRaster(Mar_2001,"Mar2001.tif", format="raster")
writeRaster(Apr_2001,"Apr2001.tif", format="raster")
writeRaster(May_2001,"May2001.tif", format="raster")
writeRaster(Jun_2001,"Jun2001.tif", format="raster")
writeRaster(Jul_2001,"Jul2001.tif", format="raster")
writeRaster(Aug_2001,"Aug2001.tif", format="raster")
writeRaster(Sep_2001,"Sep2001.tif", format="raster")
writeRaster(Oct_2001,"Oct2001.tif", format="raster")
writeRaster(Nov_2001,"Nov2001.tif", format="raster")
writeRaster(Dec_2001,"Dec2001.tif", format="raster")

Jan_2001_extent <- raster("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2001.tif")

Jan_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_1.tif")
Jan_2001_wbresample <- resample(Jan_2001_wb, Jan_2001_extent, method="bilinear")

Feb_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_2.tif")
Feb_2001_wbresample <- resample(Feb_2001_wb, Jan_2001_extent, method="bilinear")

Mar_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_3.tif")
Mar_2001_wbresample <- resample(Mar_2001_wb, Jan_2001_extent, method="bilinear")

Apr_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_4.tif")
Apr_2001_wbresample <- resample(Apr_2001_wb, Jan_2001_extent, method="bilinear")

May_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_5.tif")
May_2001_wbresample <- resample(May_2001_wb, Jan_2001_extent, method="bilinear")

Jun_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_6.tif")
Jun_2001_wbresample <- resample(Jun_2001_wb, Jan_2001_extent, method="bilinear")

Jul_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_7.tif")
Jul_2001_wbresample <- resample(Jul_2001_wb, Jan_2001_extent, method="bilinear")

Aug_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_8.tif")
Aug_2001_wbresample <- resample(Aug_2001_wb, Jan_2001_extent, method="bilinear")

Sep_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_9.tif")
Sep_2001_wbresample <- resample(Sep_2001_wb, Jan_2001_extent, method="bilinear")

Oct_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_10.tif")
Oct_2001_wbresample <- resample(Oct_2001_wb, Jan_2001_extent, method="bilinear")

Nov_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_11.tif")
Nov_2001_wbresample <- resample(Nov_2001_wb, Jan_2001_extent, method="bilinear")

Dec_2001_wb <- raster("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/wb_2001_12.tif")
Dec_2001_wbresample <- resample(Dec_2001_wb, Jan_2001_extent, method="bilinear")

#Resample MAT and MAP
MAT <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/MAT.tif")
MAT_resample <- resample(MAT, Jan_2001_extent, method="bilinear")
MAP <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/MAP.tif")
MAP_resample <- resample(MAP, Jan_2001_extent, method="bilinear")
#Load grid files

Jan_2001 <- stack("D:/Upscaling_Project/Gridded_Inputs/Idaho_MET/Jan2001.grd")
Jan_2001 <- dropLayer(Jan_2001, 7)
#4) Run site-based RF with proper variables --------

#From UC-Irvine Machine learning repository
All_sites <- read.csv("D:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_11_28.csv") 
#Print first lines
head(All_sites)
#add column names
str(All_sites)
All_sites$month <- as.factor(All_sites$month)
All_sites$IGBP_no <- as.factor(All_sites$IGBP_no)
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
All_sites <- All_sites[c("date", "site", "elev", "month", "Latitude", "Longitude", "GPP", "daylength",
                         "precip", "srad", "swe", "tmax", "tmin", "vp", "LST", "NDVI", "SPEI_1", "SPEI_3", 
                         "SPEI_6", "SPEI_9", "SPEI_12", "SPEI_IGBP", "CCP",  "MAP", "MAT", "PET", "wb")]
All_sites <- All_sites[complete.cases(All_sites),]
nacols(All_sites)
head(All_sites)

All_normalized <- as.data.frame(lapply(All_sites[8:16], normalize))
str(All_normalized)
All <- cbind(All_sites[1:7], All_normalized)
All <- cbind(All, All_sites[17:27])
str(All)
All_normalized <- All[complete.cases(All_normalized),]
head(All_normalized)
#Here's where we can split
#All_normalized <- subset(All_normalized, site="us-fuf")
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
cols1 <- c(3:4, 5:6, 9, 12:27)
head(All_sites.training)
head(All_sites.training[,cols1])
head(All_sites.training[,7:8])

#Model using: precip, NDVI, tmax, tmin,month, elev, and wb
cols2 <- c(3:4, 9, 12:13, 16, 27)
head(All_sites.training)
head(All_sites.training[,cols2])
head(All_sites.training[,7:8])


#Model using: precip, NDVI, tmax, tmin,month, and elev
cols3 <- c(3:4, 9, 12:13, 16)
head(All_sites.training)
head(All_sites.training[,cols3])
head(All_sites.training[,7:8])

#Model using: precip, NDVI, tmax, tmin, MAP, MAT, month, and elev
cols4 <- c(3:4, 9, 12:13, 16, 24:25)
head(All_sites.training)
head(All_sites.training[,cols4])
head(All_sites.training[,7:8])

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
#model_rf1 <- train(All_sites.training[, cols1], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf2 <- train(All_sites.training[,cols2], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf3 <-train(All_sites.training[,cols3], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)
#model_rf4 <- train(All_sites.training[,cols4], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE)

model_rf1 <- train(All_sites.training[,cols1], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf2 <- train(All_sites.training[,cols2], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf3<- train(All_sites.training[,cols3], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)
model_rf4<- train(All_sites.training[,cols4], All_sites.training[,7], method='rf', importance=TRUE, do.trace=TRUE)

#model_rf5preproc <- train(All_sites.training[,cols5], All_sites.training[,6], method='rf', importance=TRUE, do.trace=TRUE,  preProcess=c("center", "scale"))
#Predict based on model

predictions <- predict(object=model_rf1, All_sites.test[,cols1])
predictions <- predict(object=model_rf2, All_sites.test[,cols2])
predictions <- predict(object=model_rf3, All_sites.test[,cols3])

table(predictions)
pred1 <- as.numeric(predictions)
cor(pred1, All_sites.test[,7])

model_rf1
RF1 <- model_rf1$finalModel
RF2 <- model_rf2$finalModel
RF3 <- model_rf3$finalModel
RF4 <- model_rf4$finalModel

varImpPlot(RF1)
varImpPlot(RF2)
varImpPlot(RF3)
varImpPlot(RF4)
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


#Predict based on rasters
library(dismo)
library(raster)
library(randomForest)
library(caret)

sw <- extent(Jan_2001)
Jan_2001

model_rf2
varImpPlot(model_rf2)
#For January 
#This step takes awhile: 11:23am - on personal laptop
Jan_2001_GPP <- predict(Jan_2001, RF3, ext=sw)
plot(Jan_2001_GPP, main="Jan 2001 upscaled GPP", zlim=c(0,7))

#For June
#Jun_2001 <- stack("D:/Upscaling_Project/Gridded_Inputs/Jun_2001.tif")
#names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#sw <- extent(Jun_2001)
#Jun_2001_GPP <- predict(Jun_2001, RF5, ext=sw)
#Jun_2001_GPP <-raster("F:/Upscaling_Project/Gridded_Inputs/Jun_2001.tif")
#plot(Jun_2001_GPP, main="June 2001 upscaled GPP", zlim=c(0,7))

saveRDS(RF4, "D:/Upscaling_Project/Upscaling_Project_2017/RF4_11_29.rds")
library(caret)
library(randomForest)

#Ok next step: Adding WB rasters 
RF4 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF4_11_29.rds")


MAP_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
MAT_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")

plot(MAP_resample)
plot(MAT_resample)

sw <- extent(Jun_2001)
test <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2001.tif")
Jun_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/Jun2001.grd")
Jun_2001 <- stack(Jun_2001, MAP_resample, MAT_resample)
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "wb", "MAP", "MAT"))
plot(Jun_2001)
Jun_2001_GPP <- predict(Jun_2001, RF4, ext=sw)

Jul_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/Jul2001.grd")
Jul_2001 <- dropLayer(Jul_2001, 7)
Jul_2001 <- stack(Jul_2001, MAP_resample, MAT_resample)
names(Jul_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
plot(Jul_2001)
Jul_2001_GPP <- predict(Jul_2001, RF4, ext=sw)

Aug_2001 <- stack("Upscaling_Project/Gridded_Inputs/Idaho_MET/Aug2001.grd")
Aug_2001 <- dropLayer(Aug_2001, 7)
Aug_2001 <- stack(Aug_2001, MAP_resample, MAT_resample)
names(Aug_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Aug_2001_GPP <- predict(Aug_2001, RF4, ext=sw)

Sep_2001 <- stack("Upscaling_Project/Gridded_Inputs/Idaho_MET/Sep2001.grd")
Sep_2001 <- dropLayer(Sep_2001, 7)
Sep_2001 <- stack(Sep_2001, MAP_resample, MAT_resample)
names(Sep_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Sep_2001_GPP <- predict(Sep_2001, RF4, ext=sw)

Oct_2001 <- stack("Upscaling_Project/Gridded_Inputs/Idaho_MET/Oct2001.grd")
Oct_2001 <- dropLayer(Oct_2001, 7)
Oct_2001 <- stack(Oct_2001, MAP_resample, MAT_resample)
names(Oct_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Oct_2001_GPP <- predict(Oct_2001, RF4, ext=sw)

Nov_2001 <- stack("Upscaling_Project/Gridded_Inputs/Idaho_MET/Nov2001.grd")
Nov_2001 <- dropLayer(Nov_2001, 7)
Nov_2001 <- stack(Nov_2001, MAP_resample, MAT_resample)
names(Nov_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Nov_2001_GPP <- predict(Nov_2001, RF4, ext=sw)

plot(Sep_2001_GPP)
plot(Oct_2001_GPP)
plot(Nov_2001_GPP)

writeRaster(Nov_2001_GPP, "F:/Upscaling_Project/Nov_2001_GPP.tif")
writeRaster(Oct_2001_GPP, "F:/Upscaling_Project/Oct_2001_GPP.tif")
writeRaster(Jun_2001_GPP, "F:/Upscaling_Project/Jun_2001_GPP.tif")
writeRaster(Jul_2001_GPP, "F:/Upscaling_Project/Jul_2001_GPP.tif")            
writeRaster(Aug_2001_GPP, "F:/Upscaling_Project/Aug_2001_GPP.tif")
writeRaster(Sep_2001_GPP, "F:/Upscaling_Project/Sep_2001_GPP.tif")


Mar_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/Mar2001.grd")
Mar_2001 <- dropLayer(Mar_2001, 7)
Mar_2001 <- stack(Mar_2001, MAP_resample, MAT_resample)
names(Mar_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Mar_2001_GPP <- predict(Mar_2001, RF4, ext=sw)
writeRaster(Mar_2001_GPP, "F:/Upscaling_Project/Mar_2001_GPP.tif")

Apr_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/Apr2001.grd")
Apr_2001 <- dropLayer(Apr_2001, 7)
Apr_2001 <- stack(Apr_2001, MAP_resample, MAT_resample)
names(Apr_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
Apr_2001_GPP <- predict(Apr_2001, RF4, ext=sw)
writeRaster(Apr_2001_GPP, "F:/Upscaling_Project/Apr_2001_GPP.tif")

May_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/May2001.grd")
May_2001 <- dropLayer(May_2001, 7)
May_2001 <- stack(May_2001, MAP_resample, MAT_resample)
names(May_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev", "MAP", "MAT"))
May_2001_GPP <- predict(May_2001, RF4, ext=sw)
writeRaster(May_2001_GPP, "F:/Upscaling_Project/May_2001_GPP.tif")

#Stack them all up
Jan_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Jan_2001_GPP.tif")
Feb_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Feb_2001_GPP.tif")
Mar_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Mar_2001_GPP.tif")
Apr_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Apr_2001_GPP.tif")
May_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/May_2001_GPP.tif")
Jun_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Jun_2001_GPP.tif")
Jul_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Jul_2001_GPP.tif")
Aug_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Aug_2001_GPP.tif")
Sep_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Sep_2001_GPP.tif")
Oct_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Oct_2001_GPP.tif")
Nov_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Nov_2001_GPP.tif")
Dec_2001 <- raster("F:/Upscaling_Project/Upscaling_11_29/Dec_2001_GPP.tif")

stack_2001_GPP <- stack(Jan_2001, Feb_2001, Mar_2001, Apr_2001, May_2001, Jun_2001, Jul_2001, Aug_2001, Sep_2001, Oct_2001, Nov_2001, Dec_2001)
names(stack_2001_GPP) <- paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#write(Stack_2001_GPP, "F:/Uspcaling_Project/Upscaling_11_29/Stack_2001_GPP.tif")
#Compare to other things...
#Stack_2001_GPP <- stack("F:/Upscaling_Project/Upscaling_11_29/Stack_2001_GPP.tif")
plot(Stack_2001_GPP, zlim=c(0,6))

Stack_2001_GPP

#US-Srm
SRMpoint <- cbind(-110.86, 31.82)
SRMresult <- extract(Stack_2001_GPP, SRMpoint)
plot(SRMresult[1:12])

#US-fuf
#FUFpoint <- cbind(-111.76, 35.12)
FUFpoint <- cbind(-111, 35)
FUFresult <-extract(Stack_2001_GPP, FUFpoint)
plot(FUFresult[1:12])

#US-CA
CApoint <- cbind(-119.76, 37.12)
CAresult <-extract(Stack_2001_GPP, CApoint)
plot(CAresult[1:12])

#US-Wkg
#WKGpoint <- cbind(-109.96, 31.74)
WKGpoint <- cbind(-110, 32)
WKGresult <-extract(Stack_2001_GPP, WKGpoint)
plot(WKGresult[1:12])


file <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-fuf_merged.csv")
file$date <- as.Date(paste("01", file$monthyear, sep="_"), format="%d_%b_%Y")
str(file)
file[10:20,]
library(lubridate)
library(plyr)
#get mean seasonal cycle
file$month <- month(file$date)
file$year <- year(file$date)
seasonal <- ddply(file, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
seasonal$BarnesWKG <- (WKGresult[1:12])
seasonal$BarnesFUF <- (FUFresult[1:12])
seasonal$BarnesSRM <- (SRMresult[1:12])
str(seasonal)

WKG <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonal, aes(x=month, y=BarnesWKG, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Wkg")+
  theme_bw()

SRM <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonal, aes(x=month, y=BarnesSRM, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Srm")+
  theme_bw()

FUF <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonal, aes(x=month, y=BarnesWKG, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Fuf")+
  theme_bw()

