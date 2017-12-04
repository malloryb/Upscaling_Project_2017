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
All_sites <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_11_28.csv") 
#Print first lines
head(All_sites)
#numcols <- c(3:16, 18, 23, 25:33, 34:35)
#Allsites_sum <- All_sites[,numcols]
#head(Allsites_sum)
descrCor <- cor(Allsites_sum, use="pairwise.complete.obs")
highCorr <- sum(abs(descrCor[upper.tri(descrCor)] > .999))
highCorDescr <- findCorrelation(descrCor, cutoff=0.75)
#descrCor[,-highCorDescr]
#add column names
library(lubridate)
str(All_sites)

All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%d/%m/%Y")))
All_sites$month <- as.factor(All_sites$month)
#All_sites$IGBP_no <- as.factor(All_sites$IGBP_no)
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
All_sites <- All_sites[c("date", "site", "elev", "month", "GPP", "year",
                         "precip", "srad", "swe", "tmax", "tmin", "vp", "MAP", "MAT", "wb","NDVI", "tmed")]
All_sites <- All_sites[complete.cases(All_sites),]
nacols(All_sites)
head(All_sites)
All_sites_num  <- All_sites[c("elev", "GPP", "precip", "srad", "swe", "tmax", "tmin", "vp", "MAP", "MAT", "wb", "NDVI", "tmed")]
descrCor <- cor(All_sites_num, use="pairwise.complete.obs")
highCorr <- sum(abs(descrCor[upper.tri(descrCor)] > .999))
highCorDescr <- findCorrelation(descrCor, cutoff=0.75)
descrCor[,-highCorDescr]


#No longer going to normalize variables as per here: https://stats.stackexchange.com/questions/57010/is-it-essential-to-do-normalization-for-svm-and-random-forest
#Here's where we can split
#All_normalized <- subset(All_normalized, site="us-fuf")
#Machine Learning with "caret"
#Split into training and testing data
#Create index to split based on year
index <- createDataPartition(All_sites$GPP, p=0.80, list=FALSE)
index
#Subset training set
All_sites.training <- All_sites[index,]
All_sites.test <- All_sites[-index,]
str(All_sites)
#Overview of algorithms supported by caret function
names(getModelInfo())
head(All_sites)
#Model using: month, precip, Tair, VPD, daylength, precip, srad, swe, tmax, tmin, vp, LST, NDVI
#cols1 <- c(5, 7:18)
#Model using: same as cols1 + site component
#cols2<- c(3, 5, 7:18)
#Model using: same as as cols1 except: VPD, srad, swe 
#cols3<- c(5, 7:8, 10:11, 14:18)
#Model using: same as cols1 except: Precip, Tair, VPD
#cols4<- c(5, 10:18)
#Model with all:
colsA1 <- c(3:4, 7:14, 16)
head(All_sites.training)
head(All_sites.training[,colsA1])
head(All_sites.training[,5:6])

#Model using: precip, NDVI, tmax, tmin,month, elev, and wb
colsA2 <- c(3:4, 7:16)
head(All_sites.training)
head(All_sites.training[,colsA2])
head(All_sites.training[,5:6])

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="cv", number=5), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_rfA2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="cv", number=5), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

predictionsA1 <- predict(object=model_rfA1, All_sites.test[,colsA1])
predictionsA2 <- predict(object=model_rfA2, All_sites.test[,colsA2])
table(predictionsA1)
table(predictionsA2)
predA1 <- as.numeric(predictionsA1)
cor(predA1, All_sites.test[,5])

predA2 <- as.numeric(predictionsA2)
cor(predA2, All_sites.test[,5])

RFA1 <- model_rfA1$finalModel
RFA2 <- model_rfA2$finalModel

varImpPlot(RFA1)
varImpPlot(RFA2)

RFA1
RFA2
lb1 <- paste("R^2 == ", "0.78")
RMSE1 <- paste("RMSE==", "0.37")
lb2 <- paste("R^2 == ", "0.78")
RMSE2 <- paste("RMSE==", "0.37")

#Plot predicted vs. measured
#Model A1
qplot(predA1, All_sites.test[,5]) + 
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
  ggtitle("Random forest - A1")  

#Model A2 
qplot(predA2, All_sites.test[,5]) + 
  geom_point(shape=19, colour="tomato2", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,7.5)+
  ylim(0,7.5)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb2, parse=TRUE, x = 0.5, y = 6, size = 5, colour = "Black")+
  annotate("text", label = RMSE2, parse=TRUE, x = 0.5, y = 5.5, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - A2 (A1 + water balance)")  

saveRDS(RFA1, "F:/Upscaling_Project/Upscaling_Project_2017/RFA1_12_2.rds")
saveRDS(RFA2, "F:/Upscaling_Project/Upscaling_Project_2017/RFA2_12_2.rds")

#Predict based on rasters
library(dismo)
library(raster)
library(randomForest)
library(caret)
vp <- stack("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_vp_2000_2016_AOI.tif")
srad <- stack("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_srad_2000_2016_AOI.tif")
plot(srad)
plot(vp, zlim=0,1000)
#Doing everything for 2007
#Function where x is a list of filenames
RF_Agu_Analysis <- function(band1, month, monthno, year){
  #Read in files
  filename <- paste0("F:/Upscaling_Project/Gridded_Inputs/RF_Input/",month,"_",year, ".tif")
  filenameSrad <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameSwe <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_swe_2000_2016_AOI.tif"
  filenameVP <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameWB <- paste0("F:/Upscaling_Project/Gridded_Inputs/Idaho_MET/","wb_",year,"_",monthno,".tif")
  MAP_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  srad <- raster(filenameSrad, band = band1)
  swe <- raster(filenameSwe, band = band1)
  wb <- raster(filenameWB)
  vp <- raster(filenameVP)
  print("filesloaded")

  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  swe[swe==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  Sweresample <- resample(swe, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  wbresample <- resample(wb, MAT_resample)
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, Sweresample, wbresample, vpresample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin","MAP", "MAT","srad", "swe", "wb", "vp"))
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  
  #Predict and write out model A2 
  RFA1<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFA1_12_2.rds")
  RFA2<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFA1_12_2.rds")
  
  #PredictA1
  rast_stackA1 <- dropLayer(rast_stack, 11)
  print("PredictA1")
  RFA1_predicted <- predict(rast_stackA1, RFA1, ext=sw)
  print("PredictA2")
  RFA2_predicted <- predict(rast_stack, RFA2, ext=sw)

  outputfilenameA1 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/",month,"_",year,".tif", sep="")
  outputfilenameA2 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/",month,"_",year,".tif", sep="")
  
  print(paste("writing out", outputfilenameA1))
  writeRaster(RFA1_predicted, outputfilenameA1, overwrite=TRUE)
  
  print(paste("writing out", outputfilenameA2))
  writeRaster(RFA2_predicted, outputfilenameA2, overwrite=TRUE)
  
  gc()
}


RF_Agu_Analysis(band1=85, month="Jan", monthno=1, year=2007)
RF_Agu_Analysis(band1=86, month="Feb", monthno=2, year=2007)
RF_Agu_Analysis(band1=87, month="Mar", monthno=3, year=2007)
RF_Agu_Analysis(band1=88, month="Apr", monthno=4, year=2007)
RF_Agu_Analysis(band1=89, month="May", monthno=5, year=2007)
RF_Agu_Analysis(band1=90, month="Jun", monthno=6, year=2007)
RF_Agu_Analysis(band1=91, month="Jul", monthno=7, year=2007)
RF_Agu_Analysis(band1=92, month="Aug", monthno=8, year=2007)
RF_Agu_Analysis(band1=93, month="Sep", monthno=9, year=2007)
RF_Agu_Analysis(band1=94, month="Oct", monthno=10, year=2007)
RF_Agu_Analysis(band1=95, month="Nov", monthno=11, year=2007)
RF_Agu_Analysis(band1=96, month="Dec", monthno=12, year=2007)

#Read and stack them all up----------------
Jan_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Jan_2007.tif")
Feb_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Feb_2007.tif")
Mar_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Mar_2007.tif")
Apr_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Apr_2007.tif")
May_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/May_2007.tif")
Jun_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Jun_2007.tif")
Jul_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Jul_2007.tif")
Aug_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Aug_2007.tif")
Sep_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Sep_2007.tif")
Oct_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Oct_2007.tif")
Nov_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Nov_2007.tif")
Dec_2007A1 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Dec_2007.tif")

stack_2007A1 <- stack(Jan_2007A1, Feb_2007A1, Mar_2007A1, Apr_2007A1, May_2007A1, Jun_2007A1, Jul_2007A1, Aug_2007A1, Sep_2007A1, Oct_2007A1, Nov_2007A1, Dec_2007A1)
names(stack_2007A1) <- paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
plot(stack_2007A1, zlim=c(0,5))

writeRaster(stack_2007A1, "F:/Upscaling_Project/Upscaled_GPP/Stack_2007A1.tif")


#For model A2
Jan_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Jan_2007.tif")
Feb_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Feb_2007.tif")
Mar_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Mar_2007.tif")
Apr_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Apr_2007.tif")
May_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/May_2007.tif")
Jun_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Jun_2007.tif")
Jul_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Jul_2007.tif")
Aug_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Aug_2007.tif")
Sep_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Sep_2007.tif")
Oct_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Oct_2007.tif")
Nov_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Nov_2007.tif")
Dec_2007A2 <- raster("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Dec_2007.tif")

stack_2007A2 <- stack(Jan_2007A2, Feb_2007A2, Mar_2007A2, Apr_2007A2, May_2007A2, Jun_2007A2, Jul_2007A2, Aug_2007A2, Sep_2007A2, Oct_2007A2, Nov_2007A2, Dec_2007A2)
names(stack_2007A2) <- paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
plot(stack_2007A2, zlim=c(0,5))

#Compare to other things...
#Stack_2001_GPP <- stack("F:/Upscaling_Project/Upscaling_11_29/Stack_2001_GPP.tif")
library(lubridate)
library(plyr)
#US-Srm
SRMpoint <- cbind(-110.86, 31.82)
SRMresultA1 <- extract(stack_2007A1, SRMpoint)
SRMresultA2 <- extract(stack_2007A2, SRMpoint)
plot(SRMresultA1[1:12])
plot(SRMresultA2[1:12])

#US-fuf
FUFpoint <- cbind(-111.76, 35.0890)
FUFresultA1 <-extract(stack_2007A1, FUFpoint)
FUFresultA2 <-extract(stack_2007A2, FUFpoint)
plot(FUFresultA1[1:12])
plot(FUFresultA2[1:12])


#US-Wkg
WKGpoint <- cbind(-109.94, 31.7365)
WKGresult <-extract(stack_2007A1, WKGpoint)
plot(WKGresult[1:12])

VCMpoint <- cbind(-106.5, 35.888447)
VCMresult <-extract(stack_2001_GPP, WKGpoint)
plot(VCMresult[1:12])

VCPpoint <- cbind(-106.59, 35.864)
VCPresult <-extract(stack_2001_GPP, WKGpoint)
plot(VCPresult[1:12])

fuf <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-fuf_merged.csv")
fuf$date <- as.Date(paste("01", fuf$monthyear, sep="_"), format="%d_%b_%Y")
str(fuf)
fuf[10:20,]
#get seasonal cycle for only 
fuf$month <- month(fuf$date)
fuf$year <- year(fuf$date)
fuf_2007 <- subset(fuf, year==2007)
#seasonalfuf <- ddply(fuf, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
#seasonalfuf$BarnesFUF <- (FUFresult[1:12])
fuf_2007_monthly <- ddply(fuf_2007, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
fuf_2007_monthly$BarnesFUF <- FUFresultA1[1:12]


vcm <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-vcp_merged.csv")
vcm$date <- as.Date(paste("01", vcm$monthyear, sep="_"), format="%d_%b_%Y")
str(vcm)
vcm[10:20,]
#get mean seasonal cycle
vcm$month <- month(vcm$date)
vcm$year <- year(vcm$date)
subset(vcm, year==2007)
seasonalvcm <- ddply(vcm, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
seasonalvcm$BarnesVCM <- (VCMresult[1:12])

srm <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-srm_merged.csv")
srm$date <- as.Date(paste("01", srm$monthyear, sep="_"), format="%d_%b_%Y")
str(srm)
srm[10:20,]
#get mean seasonal cycle
srm$month <- month(srm$date)
srm$year <- year(srm$date)
subset(srm, year==2007)
seasonalsrm <- ddply(srm, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
seasonalsrm$BarnesSRM <- (SRMresult[1:12])

wkg <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-wkg_merged.csv")
wkg$date <- as.Date(paste("01", wkg$monthyear, sep="_"), format="%d_%b_%Y")
str(wkg)
wkg[10:20,]
#get mean seasonal cycle
wkg$month <- month(wkg$date)
wkg$year <- year(wkg$date)
wkg=subset(wkg, year==2007)
seasonalwkg <- ddply(wkg, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
seasonalwkg$BarnesWKG <- (WKGresult[1:12])
str(seasonal)

WKG <- ggplot() +
  geom_line(data = seasonalwkg, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonalwkg, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonalwkg, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonalwkg, aes(x=month, y=BarnesWKG, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Wkg")+
  theme_bw()

SRM <- ggplot() +
  geom_line(data = seasonalsrm, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonalsrm, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonalsrm, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonalsrm, aes(x=month, y=BarnesSRM, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Srm")+
  theme_bw()

FUF <- ggplot() +
  geom_line(data = fuf_2007_monthly, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = fuf_2007_monthly, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = fuf_2007_monthly, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= fuf_2007_monthly, aes(x=month, y=BarnesFUF, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Fuf")+
  theme_bw()


VCP <- ggplot() +
  geom_line(data = seasonalvcm, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonalvcm, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonalvcm, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonalvcm, aes(x=month, y=BarnesVCM, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Vcp")+
  theme_bw()
