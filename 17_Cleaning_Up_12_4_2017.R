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
#Create irregular time slices from: https://r-norberg.blogspot.com/2016/08/data-splitting-time-slices-with.html
createIrregularTimeSlices <- function(y, initialWindow, horizon = 1, unit = c("sec", "min", "hour", "day", "week", "month", "year", "quarter"), fixedWindow = TRUE, skip = 0) {
  if(inherits(y, 'Date')) y <- as.POSIXct(y)
  stopifnot(inherits(y, 'POSIXt'))
  
  # generate the sequence of date/time values over which to split. These will always be in ascending order, with no missing date/times.
  yvals <- seq(from = lubridate::floor_date(min(y), unit), 
               to = lubridate::ceiling_date(max(y), unit), 
               by = unit)
  
  # determine the start and stop date/times for each time slice
  stops <- seq_along(yvals)[initialWindow:(length(yvals) - horizon)]
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  }else {
    starts <- rep(1, length(stops))
  }
  
  # function that returns the indices of y that are between the start and stop date/time for a slice 
  ind <- function(start, stop, y, yvals) {
    which(y > yvals[start] & y <= yvals[stop])
  }
  train <- mapply(ind, start = starts, stop = stops, MoreArgs = list(y = y, yvals = yvals), SIMPLIFY = FALSE)
  test <- mapply(ind, start = stops, stop = (stops + horizon), MoreArgs = list(y = y, yvals = yvals), SIMPLIFY = FALSE)
  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")
  
  # reduce the number of slices returned if skip > 0
  if (skip > 0) {
    thin <- function(x, skip = 2) {
      n <- length(x)
      x[seq(1, n, by = skip)]
    }
    train <- thin(train, skip = skip + 1)
    test <- thin(test, skip = skip + 1)
  }
  
  # eliminate any slices that have no observations in either the training set or the validation set
  empty <- c(which(sapply(train, function(x) length(x) == 0)),
             which(sapply(test, function(x) length(x) == 0)))
  if(length(empty) > 0){
    train <- train[-empty]
    test <- test[-empty]
  }
  
  out <- list(train = train, test = test)
  out
}
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
A$BAL <- A$precip - A$PET
A$spei1 <-spei(A[,'BAL'],1, na.rm=TRUE) 

B$PET <- thornthwaite(B$tmed, B$Latitude[1], na.rm=TRUE)
B$BAL <- B$precip - B$PET
B$spei1 <spei(B[,'BAL'],1)

C$PET <- thornthwaite(C$tmed, C$Latitude[1], na.rm=TRUE)
C$BAL <- C$precip - C$PET
C$spei1 <-spei(C[,'BAL'],1) 

D$PET <- thornthwaite(D$tmed, D$Latitude[1], na.rm=TRUE)
D$BAL <- D$precip - D$PET
D$spei1 <-spei(D[,'BAL'],1) 

E$PET <- thornthwaite(E$tmed, E$Latitude[1], na.rm=TRUE)
E$BAL <- E$precip - E$PET
E$spei1 <-spei(E[,'BAL'],1) 

FF$PET <- thornthwaite(FF$tmed, FF$Latitude[1], na.rm=TRUE)
FF$BAL <- FF$precip - FF$PET
FF$spei1 <-spei(FF[,'BAL'],1) 

G$PET <- thornthwaite(G$tmed, G$Latitude[1], na.rm=TRUE)
GBAL <- G$precip - G$PET
G$spei1 <-spei(GG[,'BAL'],1) 

H$PET <- thornthwaite(H$tmed, H$Latitude[1], na.rm=TRUE)
H$BAL <- H$precip - H$PET
H$spei1 <-spei(H[,'BAL'],1) 

I$PET <- thornthwaite(H$tmed, H$Latitude[1], na.rm=TRUE)
I$BAL <- I$precip - I$PET
I$spei1 <-spei(I[,'BAL'],1) 

J$PET <- thornthwaite(J$tmed, J$Latitude[1], na.rm=TRUE)
J$BAL <- J$precip - J$PET
J$spei1 <-spei(J[,'BAL'],1) 

K$PET <- thornthwaite(K$tmed, K$Latitude[1], na.rm=TRUE)
K$BAL <- K$precip - K$PET
K$spei1 <-spei(K[,'BAL'],1) 

L$PET <- thornthwaite(L$tmed, L$Latitude[1], na.rm=TRUE)
L$BAL <- L$precip - L$PET
L$spei1 <-spei(L[,'BAL'],1) 

M$PET <- thornthwaite(M$tmed, M$Latitude[1], na.rm=TRUE)
M$BAL <- M$precip - M$PET
M$spei1 <-spei(M[,'BAL'],1) 

N$PET <- thornthwaite(N$tmed, N$Latitude[1], na.rm=TRUE)
N$BAL <- N$precip - N$PET
N$spei1 <-spei(N[,'BAL'],1) 

O$PET <- thornthwaite(O$tmed, O$Latitude[1], na.rm=TRUE)
O$BAL <- O$precip - O$PET
O$spei1 <-spei(O[,'BAL'],1) 

P$PET <- thornthwaite(P$tmed, P$Latitude[1], na.rm=TRUE)
P$BAL <- P$precip - P$PET
P$spei1 <-spei(P[,'BAL'],1) 

Q$PET <- thornthwaite(Q$tmed, Q$Latitude[1], na.rm=TRUE)
Q$BAL <- Q$precip - Q$PET
Q$spei1 <-spei(Q[,'BAL'],1) 

R$PET <- thornthwaite(R$tmed, R$Latitude[1], na.rm=TRUE)
R$BAL <- R$precip - R$PET
R$spei1 <-spei(R[,'BAL'],1) 

S$PET <- thornthwaite(S$tmed, S$Latitude[1], na.rm=TRUE)
S$BAL <- S$precip - S$PET
S$spei1 <-spei(S[,'BAL'],1) 

TT$PET <- thornthwaite(TT$tmed, TT$Latitude[1], na.rm=TRUE)
TT$BAL <- TT$precip - TT$PET
TT$spei1 <-spei(TT[,'BAL'],1) 

U$PET <- thornthwaite(U$tmed, U$Latitude[1], na.rm=TRUE)
U$BAL <- U$precip - U$PET
U$spei1 <-spei(U[,'BAL'],1) 


All_sites2 <- rbind(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,TT,U)
str(All_sites2)
write.csv(All_sites2, "F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_12_4.csv")

#4) Run site-based RF with proper variables --------
library(lubridate)
library(caret)
library(randomForest)
#From UC-Irvine Machine learning repository
#Now Doing 3 different models: one for spring ("Mar-May), summer("Jun-Sep"), Inactive("Oct-"feb")
All_sites <- read.csv("D:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_12_5.csv") 
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

#All_sites <- subset(All_sites, month== 4 | month== 5| month== 6| month==7 | month==8 | month==9)
All_sites <- All_sites[c("date", "site", "elev", "month", "GPP", "year",
                         "precip", "srad", "vp", "MAP", "MAT", "NDVI", "tmax", "SPEI_1")]
All_sites <- All_sites[complete.cases(All_sites),]
All_sites$date <- as.Date(All_sites$date)
nacols(All_sites)
head(All_sites)

?createTimeSlices


#No longer going to normalize variables as per here: https://stats.stackexchange.com/questions/57010/is-it-essential-to-do-normalization-for-svm-and-random-forest
#Here's where we can split
#Timesilces
#mypartition <- createIrregularTimeSlices(All_sites$date, initialWindow=48, horizon=12, unit="month", fixedWindow=T)
#ctrl <- trainControl(index=mypartition$train, indexOut=mypartition$test)
#tsmod <- train(All_sites.training[colsA1], All_sites.training[,5], method="rf", trControl=ctrl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
#Subset by season
Spring_sites <- subset(All_sites, month==3 | month==4 | month==5)
Summer_sites <- subset(All_sites, month==6 | month==7 | month==8)
Winter_sites <- subset(All_sites, month==9 | month==10| month==11 | month==12 | month==1 | month==2)
#Split into training and testing data

#Create index to split based on year
index <- createDataPartition(Winter_sites$GPP, p=0.80, list=FALSE)
index

#Resample data to overrepresent high GPP observations
#Subset training set
All_sites.training <- Winter_sites[index,]
All_sites.test <- Winter_sites[-index,]
str(All_sites)
#Overview of algorithms supported by caret function
names(getModelInfo())
head(All_sites)
#Model with all:
colsA1 <- c(3:4, 7:13)
head(All_sites.training)
head(All_sites.training[,colsA1])
head(All_sites.training[,5:6])

#Model wtih all + SPEI_1
colsA2 <- c(3:4, 7:14)
head(All_sites.training)
head(All_sites.training[,colsA2])
head(All_sites.training[,5:6])

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
#model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="cv", number=5, classProbs=TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
#model_tsA1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
#model_rfA2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="cv", number=5), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
#model_tsA2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

springmodel_A1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_spring <- predict(object=springmodel_A1, All_sites.test[,colsA1])

springmodel_A2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_spring <- predict(object=springmodel_A2, All_sites.test[,colsA2])

summermodel_A1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_summer <- predict(object=summermodel_A1, All_sites.test[,colsA1])

summermodel_A2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_summer <- predict(object=summermodel_A2, All_sites.test[,colsA2])

wintermodel_A1 <- train(All_sites.training[,colsA1], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_winter <- predict(object=wintermodel_A1, All_sites.test[,colsA1])

wintermodel_A2 <- train(All_sites.training[,colsA2], All_sites.training[,5], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
predictions_winter <- predict(object=wintermodel_A2, All_sites.test[,colsA2])

#predictionstsA2 <- predict(object=model_tsA2, All_sites.test[,colsA2])
table(predictions_spring)
table(predictions_summer)
table(predictions_winter)
#table(predictionstsA2)

predspring <- as.numeric(predictions_spring)
predsummer <- as.numeric(predictions_summer)
predwinter <- as.numeric(predictions_winter)
cor(predspring, All_sites.test[,5])
cor(predsummer, All_sites.test[,5])
#predA2 <- as.numeric(predictionsA2)
cor(predwinter, All_sites.test[,5])
#predtsA2 <- as.numeric(predictionstsA2)
#cor(predtsA2, All_sites.test[,5])

RFspring <- springmodel_A2$finalModel
RFsummer <- summermodel_A2$finalModel
RFwinter <- wintermodel_A2$finalModel
#RFtsA2 <- model_tsA2$finalModel

varImpPlot(RFspring)
varImpPlot(RFsummer)
varImpPlot(RFwinter)

#varImpPlot(RFA1)
#varImpPlot(RFA2)
#varImpPlot(RFtsA1)
#varImpPlot(RFtsA2)

saveRDS(RFspring, "D:/Upscaling_Project/Upscaling_Project_2017/RFspringA2_12_5.rds")
saveRDS(RFsummer, "D:/Upscaling_Project/Upscaling_Project_2017/RFsummerA2_12_5.rds")
saveRDS(RFwinter, "D:/Upscaling_Project/Upscaling_Project_2017/RFwinterA2_12_5.rds")
#saveRDS(RFtsA2, "F:/Upscaling_Project/Upscaling_Project_2017/RFtsA2_12_5.rds")


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


#Predict based on rasters
library(dismo)
library(raster)
library(randomForest)
library(caret)
#Doing everything for 2007
#Function where x is a list of filenames
RF_SPEI_Analysis <- function(band1, month, monthno, year){
  #Read in files
  filename <- paste0("F:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameSrad <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "F:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  print(filename)
  MAP_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("F:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","MAP", "MAT","srad", "vp", "SPEI_1"))
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  
  #Read models
  RFA1<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFA1_12_5.rds")
  RFA2<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFA2_12_5.rds")
  RFtsA1<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFtsA1_12_5.rds")
  RFtsA2<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RFtsA2_12_5.rds")
  
  #Predict and write out model A1 
  #PredictA1
  rast_stackA1 <- dropLayer(rast_stack, 10)
  print("PredictA1")
  RFA1_predicted <- predict(rast_stackA1, RFA1, ext=sw)
  print("PredictA2")
  RFA2_predicted <- predict(rast_stack, RFA2, ext=sw)
  RFtsA1_predicted <- predict(rast_stackA1, RFtsA1, ext=sw)
  print("PredictA2")
  RFtsA2_predicted <- predict(rast_stack, RFtsA2, ext=sw)
  
  outputfilenameA1 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/",month,"_",year,".tif", sep="")
  outputfilenameA2 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/",month,"_",year,".tif", sep="")
  outputfilenametsA1 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_ts_A1/",month,"_",year,".tif", sep="")
  outputfilenametsA2 <- paste("F:/Upscaling_Project/Upscaled_GPP/AGU_ts_A2/",month,"_",year,".tif", sep="")
  
  print(paste("writing out", outputfilenameA1))
  writeRaster(RFA1_predicted, outputfilenameA1, overwrite=TRUE)
  
  print(paste("writing out", outputfilenameA2))
  writeRaster(RFA2_predicted, outputfilenameA2, overwrite=TRUE)
  
  print(paste("writing out", outputfilenametsA1))
  writeRaster(RFtsA1_predicted, outputfilenametsA1, overwrite=TRUE)
  
  print(paste("writing out", outputfilenametsA2))
  writeRaster(RFtsA2_predicted, outputfilenametsA2, overwrite=TRUE)
  
  gc()
}

RF_spring_Analysis <- function(band1, month, monthno, year){
  #Read in files
  filename <- paste0("D:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameSrad <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "D:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  print(filename)
  MAP_resample <- raster("D:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("D:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","MAP", "MAT","srad", "vp", "SPEI_1"))
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  
  #Read models
  RFA2<- readRDS("D:/Upscaling_Project/Upscaling_Project_2017/RFspringA2_12_5.rds")
    #Predict and write out model A1 
  #PredictA1
  RFA2_predicted <- predict(rast_stack, RFA2, ext=sw)
  
  outputfilenameA2 <- paste("D:/Upscaling_Project/Upscaled_GPP/AGU_Model_Seasonal/",month,"_",year,".tif", sep="")
  
  print(paste("writing out", outputfilenameA2))
  writeRaster(RFA2_predicted, outputfilenameA2, overwrite=TRUE)
  
  gc()
}
RF_summer_Analysis <- function(band1, month, monthno, year){
  #Read in files
  filename <- paste0("D:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameSrad <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "D:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  print(filename)
  MAP_resample <- raster("D:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("D:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","MAP", "MAT","srad", "vp", "SPEI_1"))
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  
  #Read models
  RFA2<- readRDS("D:/Upscaling_Project/Upscaling_Project_2017/RFsummerA2_12_5.rds")
  #Predict and write out model A1 
  #PredictA1
  RFA2_predicted <- predict(rast_stack, RFA2, ext=sw)
  
  outputfilenameA2 <- paste("D:/Upscaling_Project/Upscaled_GPP/AGU_Model_Seasonal/",month,"_",year,".tif", sep="")
  
  print(paste("writing out", outputfilenameA2))
  writeRaster(RFA2_predicted, outputfilenameA2, overwrite=TRUE)
  
  gc()
}
RF_winter_Analysis <- function(band1, month, monthno, year){
  #Read in files
  filename <- paste0("D:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameSrad <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "D:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "D:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  print(filename)
  MAP_resample <- raster("D:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("D:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","MAP", "MAT","srad", "vp", "SPEI_1"))
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  
  #Read models
  RFA2<- readRDS("D:/Upscaling_Project/Upscaling_Project_2017/RFwinterA2_12_5.rds")
  #Predict and write out model A1 
  #PredictA1
  RFA2_predicted <- predict(rast_stack, RFA2, ext=sw)
  
  outputfilenameA2 <- paste("D:/Upscaling_Project/Upscaled_GPP/AGU_Model_Seasonal/",month,"_",year,".tif", sep="")
  
  print(paste("writing out", outputfilenameA2))
  writeRaster(RFA2_predicted, outputfilenameA2, overwrite=TRUE)
  
  gc()
}



RF_winter_Analysis(band1=85, month="Jan", monthno=1, year=2007)
RF_winter_Analysis(band1=86, month="Feb", monthno=2, year=2007)
RF_spring_Analysis(band1=87, month="Mar", monthno=3, year=2007)
RF_spring_Analysis(band1=88, month="Apr", monthno=4, year=2007)
RF_spring_Analysis(band1=89, month="May", monthno=5, year=2007)
RF_summer_Analysis(band1=90, month="Jun", monthno=6, year=2007)
RF_summer_Analysis(band1=91, month="Jul", monthno=7, year=2007)
RF_summer_Analysis(band1=92, month="Aug", monthno=8, year=2007)
RF_winter_Analysis(band1=93, month="Sep", monthno=9, year=2007)
RF_winter_Analysis(band1=94, month="Oct", monthno=10, year=2007)
RF_winter_Analysis(band1=95, month="Nov", monthno=11, year=2007)
RF_winter_Analysis(band1=96, month="Dec", monthno=12, year=2007)


RF_winter_Analysis(band1=156, month="Jan", monthno=1, year=2013)
RF_winter_Analysis(band1=157, month="Feb", monthno=2, year=2013)
RF_spring_Analysis(band1=158, month="Mar", monthno=3, year=2013)
RF_spring_Analysis(band1=159, month="Apr", monthno=4, year=2013)
RF_spring_Analysis(band1=160, month="May", monthno=5, year=2013)
RF_summer_Analysis(band1=161, month="Jun", monthno=6, year=2013)
RF_summer_Analysis(band1=162, month="Jul", monthno=7, year=2013)
RF_summer_Analysis(band1=163, month="Aug", monthno=8, year=2013)
RF_winter_Analysis(band1=164, month="Sep", monthno=9, year=2013)
RF_winter_Analysis(band1=165, month="Oct", monthno=10, year=2013)
RF_winter_Analysis(band1=167, month="Nov", monthno=11, year=2013)
RF_winter_Analysis(band1=168, month="Dec", monthno=12, year=2013)

Jun_2007A1 <- raster("D:/Upscaling_Project/Upscaled_GPP/AGU_Model_A1/Jun_2007.tif")
Jun_2007A2 <- raster("D:/Upscaling_Project/Upscaled_GPP/AGU_Model_A2/Jun_2007.tif")
Jun_2007tsA1 <- raster("D:/Upscaling_Project/Upscaled_GPP/AGU_ts_A1/Jun_2007.tif")
Jun_2007tsA2 <- raster("D:/Upscaling_Project/Upscaled_GPP/AGU_ts_A2/Jun_2007.tif")
plot(Jun_2007A1)
plot(Jun_2007A2)
plot(Jun_2007tsA1)
plot(Jun_2007tsA2)
Jun_2007 <- stack(Jun_2007A1, Jun_2007tsA1, Jun_2007A2, Jun_2007tsA2)
plot(Jun_2007)
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
