#Fixing the offset issue (water year starting on Nov 1st vs. Oct 1st) and re-running the site-level analysis
#Created 2/2/2018
#To-do's
#1) Merge daymet files with fixed flux files
#2) Merge item #1) with EVI data and LST data
#3) Merge item #2) with Hydro 1k data
#4) Calculate water balance using 'spei' package for site-based RF (X)
#5) Calculate spei for site-based RF
#6) Run site-based RF with proper input variables
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(SPEI)
#1) Merge daymet files with fixed flux files----------------------
#Read all three big .csv files and merge by date

#Flux
Flux_merge <- read.csv("D:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_3_2_2018.csv")
#Daymet
Daymet_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Daymet_monthly_all.csv") 
#MODIS
MODIS_merge <-  read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")

#Check files, delete "X", and format date col for merge 
str(Flux_merge)
Flux_merge$date <- as.Date(Flux_merge$date, format="%Y-%m-%d")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soy", "us-so3")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-sob", "us-so2")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-son", "us-so4")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soo", "us-so2")
Flux_merge$sitedate <- with(Flux_merge, paste(site,date, sep="-"))


#Replace sites "us-ray" and "us-tex" with their proper name (they are from mexiflux not ameriflux)
Daymet_merge<- subset(Daymet_merge, select = -c(X))
Daymet_merge$tmed <- ((Daymet_merge$tmax + Daymet_merge$tmin)/2)
Daymet_merge$date <- as.Date(Daymet_merge$date, format="%Y-%m-%d")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-ray", "mx-ray")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-tes", "mx-tes")
Daymet_merge$sitedate <- with(Daymet_merge, paste(site,date, sep="-"))
unique(Daymet_merge$site)

#Set latitudes by site
str(Daymet_merge)
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
IGBP_lookup <- IGBP_lookup[,c("site", "lat")]
Lat_merge <- plyr::rename(IGBP_lookup, c("site"="site", "lat"="Latitude"))
Daymet_for_SPEI <- merge(Daymet_merge, Lat_merge, by="site")
str(Daymet_for_SPEI)
unique(Daymet_for_SPEI$site)
#split 
#More sites in here than in the lookup file - so should be lots of blanks re: Latitude 
X <- split(Daymet_for_SPEI, Daymet_for_SPEI$site)
df1  <- X[[1]]
df2  <- X[[2]]
df3  <- X[[3]]
df4  <- X[[4]]
df5  <- X[[5]]
df6  <- X[[6]]
df7  <- X[[7]]
df8  <- X[[8]]
df9  <- X[[9]]
df10 <- X[[10]]
df11 <- X[[11]]
df12 <- X[[12]]
df13 <- X[[13]]
df14 <- X[[14]]
df15 <- X[[15]]
df16 <- X[[16]]
df17 <- X[[17]]
df18 <- X[[18]]
df19 <- X[[19]]
df20 <- X[[20]]
df21 <- X[[21]]
df22 <- X[[22]]
df23 <- X[[23]]
df24 <- X[[24]]

#SPEI_calc_function
SPEI_calc <- function(A){
  A <- A[order(A$date),]
  print(A$site[1])
  print("calculate water balance")
  A$PET <- thornthwaite(A$tmed, A$Latitude[1], na.rm=TRUE)
  A$BAL <- A$precip - A$PET
  print("calculate spei")
  spei1 <- (spei(A[,'BAL'], 1)$fitted)
  spei1 <- data.frame(spei1 = c(spei1), time=c(time(spei1))) 
  spei3 <- (spei(A[,'BAL'], 3)$fitted)
  spei3 <- data.frame(spei3 = c(spei3), time = c(time(spei3))) 
  spei6 <- (spei(A[,'BAL'], 6)$fitted)
  spei6 <- data.frame(spei6 = c(spei6), time = c(time(spei6))) 
  spei9 <- (spei(A[,'BAL'], 9)$fitted)
  spei9 <- data.frame(spei9 = c(spei9), time = c(time(spei9))) 
  spei12 <-(spei(A[,'BAL'], 12)$fitted)
  spei12 <-data.frame(spei12 = c(spei12), time = c(time(spei12))) 
  print("bind everything together")
  A$PET <- as.numeric(A$PET)
  A$BAL <- as.numeric(A$BAL)
  spei_all <- cbind(spei1, spei3, spei6, spei9, spei12)
  spei_all$date <- format(date_decimal(spei_all$time), "%m-%d-%Y")
  spei_all$date <- as.Date(spei_all$date, format="%m-%d-%Y")
  spei_all$date <- floor_date((spei_all$date +1), "month")
  spei_all$month <- month(spei_all$date)
  #columns to get rid of in the final thing: 16, 18, 20, 22, 24 (all caled "time") and the second "date (postion=25)
  final <- cbind(A, spei_all)
  final <- final[-c(16,18,20,22,24,25)]
  print(head(final))
  print(nrow(A))
  print(nrow(final))
  return(final)
}

#Get list of dataframes using pattern (thank you stack overflow!: https://stackoverflow.com/questions/14954399/put-multiple-data-frames-into-list-smart-way)
l.df <- lapply(ls(pattern="df[0-9]+"), function(x) get(x))
str(l.df)
#Apply & combine in one
Daymet_merge <- do.call("rbind", lapply(l.df, SPEI_calc))

MODIS_merge<- subset(MODIS_merge, select = -c(X))
MODIS_merge$date <- as.Date(MODIS_merge$date, format="%Y-%m-%d")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-ray", "mx-ray")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-tes", "mx-tes")
MODIS_merge$sitedate <- with(MODIS_merge, paste(site,date, sep="-"))
str(MODIS_merge)

levels(unlist(as.factor(Daymet_merge$site)))
levels(unlist(as.factor(Flux_merge$site)))
levels(unlist(as.factor(MODIS_merge$site)))

head(Daymet_merge)
head(Flux_merge)
head(MODIS_merge)
#Merge! And clean up merged mess. Final merged file should have same number
#of observations as the "flux_merge" file since we should have complete records
#For both daymet and MODIS - 2197 obs for all
Flux_daymet <- merge(Flux_merge, Daymet_merge, by="sitedate", all.x=T)
Merged_all <- merge(Flux_daymet, MODIS_merge, by="sitedate", all.x=T)
str(Flux_daymet)
str(Merged_all)

#Cleanup
#Delete extraneous columns
Merged_all <- subset(Merged_all, select = -c(sitedate, date.x, site.x, date.y, site.y))
#Format_Date
Merged_all$date <- as.Date(Merged_all$date)
Merged_all$IGBP <- NA
str(Merged_all)
#Add IGBP column based on lookup table------------------ not working yet 
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
str(IGBP_lookup)
IGBP_lookup
All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGPB <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGPB)

#Write out .csv file
#write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv")

#Get random forest models (this code could be cleaned up significantly) 
#Run site-based RF with proper variables --------
library(lubridate)
library(caret)
library(randomForest)
library(dplyr)
library(plyr)

#From UC-Irvine Machine learning repository
#Now Doing 3 different models: one for spring ("Mar-May), summer("Jun-Sep"), Inactive("Oct-"feb")
All_sites <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv") 
str(All_sites)
head(All_sites)
#Checking on SPEI
SPEI_Check <- All_sites[c("date", "site", "spei1", "spei12")]
SPEI_Check$date <- as.Date(SPEI_Check$date, format="%Y-%m-%d")
SPEI_Check$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
SPEI_Check$site_year <- do.call(paste, c(SPEI_Check[c("site", "year")], sep = "_")) 
str(SPEI_Check)

SPEI_cors <- ddply(SPEI_Check, .(site_year), summarize,
        spei1mean = mean(spei1,na.rm=TRUE), spei1max= spei1[which.max( abs(spei1))], spei1min=spei1[which.min( abs(spei1))], 
        spei1med=median_hilow(spei1, na.rm=TRUE), spei12 = first(spei12))
cor(SPEI_cors$spei12, SPEI_cors$spei1mean, use="complete.obs")
cor(SPEI_cors$spei12, SPEI_cors$spei1max, use="complete.obs")
cor(SPEI_cors$spei12, SPEI_cors$spei1min, use="complete.obs")
cor(SPEI_cors$spei12, SPEI_cors$spei1med, use="complete.obs")

#Print first lines
head(All_sites)
#Fix column names and add numeric columns
str(All_sites)
All_sites$elev <- as.numeric(All_sites$elev)
All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
All_sites$month <- as.factor(All_sites$month)
All_sites$precip <- as.numeric(All_sites$precip)
All_sites$swe <- as.numeric(All_sites$swe)

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

str(All_sites)
#All_sites <- subset(All_sites, month== 4 | month== 5| month== 6| month==7 | month==8 | month==9)
All_sites <- All_sites[c("GPP", "date", "daylength", "site", "elev", "month", "srad", "swe", "tmed", "tmax", "tmin", "BAL", "PET", 
                         "precip", "vp", "MAP", "MAT", "NDVI", "spei1", "spei3", "spei6", "spei9", "spei12")]

#408th (row 409) value for All_Sites$spei3 is "Inf" for some reason, and so is the 1495th value. Going to replace it with the average of the two surrounding values: 0.495
#Why are they in different spots? at 266 and 1448 this time
which(sapply(All_sites$spei3, is.infinite))

All_sites$spei3[265:267]
All_sites$spei3[266] <- 0.908
All_sites$spei3[1447:1449]
All_sites$spei3[1448] <- -1.0609


summary(All_sites)
All_sites <- All_sites[complete.cases(All_sites),]
summary(All_sites)
str(All_sites)

#No longer going to normalize variables as per here: https://stats.stackexchange.com/questions/57010/is-it-essential-to-do-normalization-for-svm-and-random-forest
#Here's where we can split
#Timesilces
#mypartition <- createIrregularTimeSlices(All_sites$date, initialWindow=48, horizon=12, unit="month", fixedWindow=T)
#ctrl <- trainControl(index=mypartition$train, indexOut=mypartition$test)
#tsmod <- train(All_sites.training[colsA1], All_sites.training[,5], method="rf", trControl=ctrl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

#Split into training and testing data

#Create index to split based on year
index <- createDataPartition(All_sites$GPP, p=0.80, list=FALSE)
index


#Resample data to overrepresent high GPP observations
#Subset training set
All_sites.training <- All_sites[index,]
All_sites.test <- All_sites[-index,]
str(All_sites)

#All_sites.trianing <- preProcess(All_sites.training, method = c("center", "scale"))

str(All_sites.training)
str(All_sites.test)

#Overview of algorithms supported by caret function
names(getModelInfo())

head(All_sites)
#Model with all:
colsA1 <- c(3, 5:23)
head(All_sites.training)
head(All_sites.training[,colsA1])
str(All_sites.training[,colsA1])
head(All_sites.training[,1:2])

str(All_sites.training)
#Model with: NDVI, daylength, MAP, MAT, vp, month, srad, spei12, PET, tmax, elev, tmin, precip, spei1
colsA2 <- c(3, 5:7, 10:11, 13:19, 23)
head(All_sites.training)
head(All_sites.training[,colsA2])
head(All_sites.training[,5:6])


#Model with: NDVI, daylength, MAP, MAT, vp, month, srad, tmax, elev, tmin, precip)
colsA3 <- c(3, 5:7, 10:11, 14:18)
head(All_sites.training)
head(All_sites.training[,colsA3])
head(All_sites.training[,5:6])

#Model same as A2 but without PET 
colsA4 <- c(3, 5:7, 10:11, 14:19, 23)
head(All_sites.training)
head(All_sites.training[,colsA4])
head(All_sites.training[,5:6])

#this doesn't seem to be working
All_sites.training[!complete.cases(All_sites.training),]

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
myControl <- trainControl(method="repeatedcv", repeats=5, number=10)

model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA3 <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA3 <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA4 <- train(All_sites.training[,colsA4], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA4 <- train(All_sites.training[,colsA4], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

pred_rfA1 <- as.numeric(predict(object=model_rfA1, All_sites.test[,colsA1]))
pred_tsA1 <- as.numeric(predict(object=model_tsA1, All_sites.test[,colsA1]))

pred_rfA2 <- as.numeric(predict(object=model_rfA2, All_sites.test[,colsA2]))
pred_tsA2 <- as.numeric(predict(object=model_tsA2, All_sites.test[,colsA2]))

pred_rfA3 <- as.numeric(predict(object=model_rfA3, All_sites.test[,colsA3]))
pred_tsA3 <- as.numeric(predict(object=model_tsA3, All_sites.test[,colsA3]))

pred_rfA4 <- as.numeric(predict(object=model_rfA4, All_sites.test[,colsA4]))
pred_tsA4 <- as.numeric(predict(object=model_tsA4, All_sites.test[,colsA4]))


cor(pred_rfA1, All_sites.test[,1])
cor(pred_tsA1, All_sites.test[,1])
cor(pred_rfA2, All_sites.test[,1])
cor(pred_tsA2, All_sites.test[,1])
cor(pred_rfA3, All_sites.test[,1])
cor(pred_tsA3, All_sites.test[,1])
cor(pred_rfA4, All_sites.test[,1])
cor(pred_tsA4, All_sites.test[,1])


postResample(pred=pred_rfA1, obs=All_sites.test[,1])
postResample(pred=pred_tsA1, obs=All_sites.test[,1])
postResample(pred=pred_rfA2, obs=All_sites.test[,1])
postResample(pred=pred_tsA2, obs=All_sites.test[,1])
postResample(pred=pred_rfA3, obs=All_sites.test[,1])
postResample(pred=pred_tsA3, obs=All_sites.test[,1])

postResample(pred=pred_rfA4, obs=All_sites.test[,1])
postResample(pred=pred_tsA4, obs=All_sites.test[,1])


RF_F1 <- model_rfA1$finalModel
RF_T1 <- model_tsA1$finalModel
RF_F2 <- model_rfA2$finalModel
RF_T2 <- model_tsA2$finalModel
RF_F3 <- model_rfA3$finalModel
RF_T3 <- model_tsA3$finalModel
RF_F4 <- model_rfA4$finalModel
RF_T4 <- model_tsA4$finalModel


varImpPlot(RF_F1)
varImpPlot(RF_T1)
varImpPlot(RF_F2)
varImpPlot(RF_T2)
varImpPlot(RF_F3)
varImpPlot(RF_T3)

varImpPlot(RF_F4)
varImpPlot(RF_T4)

saveRDS(RF_F1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F1_2_16.rds")
saveRDS(RF_T1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T1_2_16.rds")
saveRDS(RF_F2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F2_2_16.rds")
saveRDS(RF_T2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T2_2_16.rds")
saveRDS(RF_F3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F3_3_3.rds")
saveRDS(RF_T3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T3_3_3.rds")
saveRDS(RF_F4, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F4_3_3.rds")
saveRDS(RF_T4, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T4_3_3.rds")


RFA1
RFA2
lb1 <- paste("R^2 == ", "0.70")
RMSE1 <- paste("RMSE==", "0.76")
lb2 <- paste("R^2 == ", "0.78")
RMSE2 <- paste("RMSE==", "0.37")

#Plot predicted vs. measured
#Model A1
qplot(predsummer, All_sites.test[,5]) + 
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



##Running RF models on gridded data subsets --------------------------
library(caret)
library(randomForest)
raster("F:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif", band=93)
#Create extents 
RF_Val_Analysis <- function(band1, bandsp, month, monthno, year){
  #Read in files
  filename <- paste0("F:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameDayl <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_dayl_2000_2016_AOI.tif"
  filenameSrad <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "F:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  filenamespei12 <- "F:/SPEIBase/spei12.nc"
  print(filename)
  MAP_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("F:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  #inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP, band= band1)
  dayl <- raster(filenameDayl)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  spei12 <- brick(filenamespei12)
  spei12 <- spei12[[bandsp]]
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  spei12resample <- resample(spei12, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  daylresample <- resample(dayl, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample, daylresample, spei12resample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "spei1", "daylength", "spei12"))
  print("raster stacked")
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  print(sw)
  
  create_extent <- function(x){
    radius <- 0.5 # radius in kilometers
    yPlus <- x[1,2]+ radius
    xPlus <- x[1,1]+ radius
    yMinus <- x[1,2] - radius
    xMinus <- x[1,1] - radius
    e <- extent(xMinus, xPlus, yMinus, yPlus)
    return(e)
     }
  
  #Trying to just do a radius around a point
  AUDpoint <- cbind(-110.509, 31.591)
  COPpoint <- cbind(-109.66, 38.162)
  FUFpoint <- cbind(-111.762,	35.089)
  LPApoint <- cbind(-110.438, 24.1293)
  MPJpoint <- cbind(-106.239,	34.43828)
  #MX site
  RAYpoint <- cbind(-110.537,	29.741)
  SCCpoint<- cbind(-116.45,	33.61)
  SCFpoint<- cbind(-116.45,	33.808)
  SCWpoint<- cbind(-116.455,	33.605)
  SEGpoint<- cbind(-106.7,	34.360)
  SENpoint<- cbind(-106.68,	34.358)
  SESpoint <- cbind(-106.745,	34.335)
  SO4point<- cbind(-116.6406,	33.385)
  SO2point<- cbind(-116.6228,	33.374)
  SO3point<- cbind(-116.6226,	33.377)
  SRCpoint<- cbind(-110.8395,	31.908)
  SRGpoint <- cbind(-110.828,	31.789)
  SRMpoint <- cbind(-110.866, 31.821)
  #MX Site
  TESpoint<- cbind(-109.298,	27.8446)
  VCMpoint<- cbind(-106.532,	35.888)
  VCPpoint<- cbind(-106.597,	35.864)
  WHSpoint<- cbind(-110.052,	31.744)
  WJSpoint<- cbind(-105.862,	34.426)
  WKGpoint <- cbind(-109.942,	31.737)
  
  audex <- create_extent(AUDpoint)
  aud <- crop(rast_stack,audex)
  aud <- extent(aud)
  
  copex <- create_extent(COPpoint)
  cop <- crop(rast_stack,copex)
  cop <- extent(cop)
  
  fufex <- create_extent(FUFpoint)
  fuf <- crop(rast_stack,fufex)
  fuf <- extent(fuf)
  
  lpaex <- create_extent(LPApoint)
  lpa <- crop(rast_stack,lpaex)
  lpa <- extent(lpa)
  
  mpjex <- create_extent(MPJpoint)
  mpj <- crop(rast_stack,mpjex)
  mpj <- extent(mpj)
  
  rayex <- create_extent(RAYpoint)
  ray <- crop(rast_stack,rayex)
  ray <- extent(ray)
  
  
  sccex <- create_extent(SCCpoint)
  scc <- crop(rast_stack,sccex)
  scc <- extent(scc)
  
  scfex <- create_extent(SCFpoint)
  scf <- crop(rast_stack,scfex)
  scf <- extent(scf)
  
  scwex <- create_extent(SCWpoint)
  scw <- crop(rast_stack,scwex)
  scw <- extent(scw)
  
  segex <- create_extent(SEGpoint)
  seg <- crop(rast_stack,segex)
  seg <- extent(seg)
  
  senex <- create_extent(SENpoint)
  sen <- crop(rast_stack,senex)
  sen <- extent(sen)
  
  sesex <- create_extent(SESpoint)
  ses <- crop(rast_stack,sesex)
  ses <- extent(ses)
  
  so4ex <- create_extent(SO4point)
  so4 <- crop(rast_stack,so4ex)
  so4 <- extent(so4)
  
  so2ex <- create_extent(SO2point)
  so2 <- crop(rast_stack,so2ex)
  so2 <- extent(so2)
  
  so3ex <- create_extent(SO3point)
  so3 <- crop(rast_stack,so3ex)
  so3 <- extent(so3)
  
  srcex <- create_extent(SRCpoint)
  src <- crop(rast_stack,srcex)
  src <- extent(src)
  
  srgex <- create_extent(SRGpoint)
  srg <- crop(rast_stack,srgex)
  srg <- extent(srg)
  
  srmex <- create_extent(SRMpoint)
  srm <- crop(rast_stack,srmex)
  srm <- extent(srm)
  
  tesex <- create_extent(TESpoint)
  tes <- crop(rast_stack,tesex)
  tes <- extent(tes)
  
  vcmex <- create_extent(VCMpoint)
  vcm <- crop(rast_stack,vcmex)
  vcm <- extent(vcm)

  vcpex <- create_extent(VCPpoint)
  vcp <- crop(rast_stack,vcpex)
  vcp <- extent(vcp)
  
  whsex <- create_extent(WHSpoint)
  whs <- crop(rast_stack,whsex)
  whs <- extent(whs)
  
  wjsex <- create_extent(WJSpoint)
  wjs <- crop(rast_stack,wjsex)
  wjs <- extent(wjs)
  
  wkgex <- create_extent(WKGpoint)
  wkg <- crop(rast_stack,wkgex)
  wkg <- extent(wkg)
  
  print("subset points")
  
  #Read models
  RFF3<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_F3_2_16.rds")
  RFT3<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_T3_2_16.rds")
  RFF4<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_F4_2_27.rds")
  RFT4 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_T4_2_27.rds")
  
  
  #For each site: 4 .tifs written out - write function and lapply over list of extents? Or at least function to apply models and write them out
  Apply_RF <- function(site, sname, env= parent.frame()){
    print(ls())
    print(month)
    print(year)
    #Function needs to apply to a site and 
    RFF3_predicted <- predict(rast_stack, RFF3, ext=site)
    plot(RFF3_predicted)
    RFT3_predicted <- predict(rast_stack, RFT3, ext=site)
    RFF4_predicted <- predict(rast_stack, RFF4, ext=site)
    RFT4_predicted <- predict(rast_stack, RFT4, ext=site)
    
    print(month)
    print(year)
    print(site)
    
    outputfilenameF3 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_F3/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameT3 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_T3/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameF4 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_F4/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameT4 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_T4/",month,"_",year,"_", sname, ".tif", sep="")
    
    print(paste("writing out", outputfilenameF3))
    writeRaster(RFF3_predicted, outputfilenameF3, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameT3))
    writeRaster(RFT3_predicted, outputfilenameT3, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameF4))
    writeRaster(RFF4_predicted, outputfilenameF4, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameT4))
    writeRaster(RFT4_predicted, outputfilenameT4, overwrite=TRUE)
    
  }

Apply_RF(aud, "us_aud")
Apply_RF(cop, "us_cop")
Apply_RF(fuf, "us_fuf")
Apply_RF(lpa, "us_lpa")
Apply_RF(mpj, "us_mpj")  
Apply_RF(ray, "us_ray")  
Apply_RF(scc, "us_scc")
Apply_RF(scf, "us_scf")
Apply_RF(scw, "us_scw")
Apply_RF(seg, "us_seg")
Apply_RF(sen, "us_sen")
Apply_RF(ses, "us_ses")  
Apply_RF(so4, "us_so4")  
Apply_RF(so2, "us_so2")
Apply_RF(so3, "us_so3")
Apply_RF(src, "us_src")
Apply_RF(srg, "us_srg")
Apply_RF(srm, "us_srm")
Apply_RF(tes, "us_tes")  
Apply_RF(vcm, "us_vcm")  
Apply_RF(vcp, "us_vcp")
Apply_RF(whs, "us_whs")
Apply_RF(wks, "us_wks")
Apply_RF(wkg, "us_wkg")

  gc()
}






RF_Val_Analysis(band1=85, bandsp=1273, month="Jan", monthno=1, year=2007)
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

library(raster)


#Reading SPEIbase netcdf file
library(ncdf4)
library(raster)
library(reshape2)
library(dplyr)
library(chron)
library(lattice)
library(RColorBrewer)

spei12 <- brick("F:/SPEIBase/spei12.nc")
spei12[[1273]] 

#Need to merge flux and input files-------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(plyr)
library(stringr)
#Plan: Merge new flux files (sums) with Jung 2017 files (extracted yesterday)
Jung_files <- list.files("F:/Upscaling_Project/Jung_Comps/", pattern="*_*.csv$")

#Function to read all files in list and apply necessary corrections
myDb <- lapply(Jung_files, function(x){
  dat <- read.csv(x, skip=2)
  names(dat) <- paste(c("monthyear", "Jung_GPP"))
  dat$monthyear <- as.character(dat$monthyear)
  dat$site <- tools::file_path_sans_ext(basename(x))
  return(dat)
})

str(myDb)

#Rbind dataframes to gether
Jung_2017 <- do.call(rbind,myDb)

Flux_files <- read.csv("F:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_3_2_2018.csv")
#Merge by "site_date" column
str(Flux_files)
Flux_files$site <- str_replace_all(Flux_files$site, "us-soy", "us-so3")
Flux_files$site <- str_replace_all(Flux_files$site, "us-sob", "us-so2")
Flux_files$site <- str_replace_all(Flux_files$site, "us-son", "us-so4")
Flux_files$site <- str_replace_all(Flux_files$site, "us-soo", "us-so2")
Flux_files$monthyear <- (gsub('([[:punct:]])|\\-','_',Flux_files$monthyear))
Flux_files$site <- (gsub('([[:punct:]])|\\-','_',Flux_files$site))
Flux_files$site <- as.factor(Flux_files$site)
Flux_files$sitedate <- paste(Flux_files$monthyear, Flux_files$site, sep="_")

str(Jung_2017)
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_soy", "us_so3")
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_sob", "us_so2")
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_son", "us_so4")
Jung_2017$site <- as.factor(Jung_2017$site)
Jung_2017$sitedate <- paste(Jung_2017$monthyear, Jung_2017$site, sep="_")


levels(Jung_2017$site)
levels(Flux_files$site)
#Merged file
Merged_Jung_Comp <- merge(Flux_files, Jung_2017, by="sitedate", all.x=T)
#Delete columns: site.y, X, X1, monthyear.y
drops <- c("site.y", "X", "X1", "monthyear.y")
Merged_Jung_Comp <- Merged_Jung_Comp[, !(names(Merged_Jung_Comp) %in% drops)]
write.csv(Merged_Jung_Comp, "F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")

#Graphing comps----------------------------------------
#Need to create some graphs now:
library(ggthemes)

Merged_Jung_Comp <- read.csv("D:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
str(Merged_Jung_Comp)
Merged_Jung_Comp <- Merged_Jung_Comp[!(is.na(Merged_Jung_Comp$year)),] 
summary(Merged_Jung_Comp)
Merged_Jung_Comp$date <- as.Date(Merged_Jung_Comp$date, format="%Y-%m-%d")
Merged_Jung_Comp$month <- month(Merged_Jung_Comp$date)
Merged_Jung_Comp$year <- year(Merged_Jung_Comp$date)

#Delete all files after 2013 (Fluxcom only goes through 2013)
Merged_Jung_Comp<-Merged_Jung_Comp[!(Merged_Jung_Comp$year > 2013 | Merged_Jung_Comp$year < 1999),]
new_DF <- Merged_Jung_Comp[is.na(Merged_Jung_Comp$Jung_GPP),]
new_DF
summary(Merged_Jung_Comp)

sd(Merged_Jung_Comp$Jung_GPP)
#Need both interannual and seasonal graphs and correlations
#First let's make the graphs comparing seasonal cycles 
#Split - apply - combine 
out <- split(Merged_Jung_Comp, Merged_Jung_Comp$site.x)
str(out)
seasonal_func <- function(x){
  df <- ddply(x, .(month, site.x), summarize, Jung_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])) , Jung_GPP=mean(Jung_GPP, na.rm=TRUE), GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])) , GPP=mean(GPP, na.rm=TRUE))
  return(df)
}

lapply(out, seasonal_func)
seasonal_to_plot <- do.call(rbind, lapply(out, seasonal_func))
str(seasonal_to_plot)
seasonal_to_plot
names(seasonal_to_plot)[names(seasonal_to_plot) == 'site.x'] <- 'site'

#going to have to create a plotting function to plot all of these separately (and write out)
#write out plot
#write out correlation?
#plot graph 

plot_seasonal_cycle <- function(x){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  r <- as.character(round(cor(x$GPP, x$Jung_GPP), 3))
  lbl <- paste("r =", r)
  q <- ggplot() +
    ggtitle(x$site)+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("blue")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    annotate("text", label = lbl, parse=FALSE, x = 1, y = 4, size = 5, colour = "Black")+
    geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP-Jung_se,ymax=Jung_GPP+Jung_se),colour="blue")+
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  plot(q)
}

list_seasons <- split(seasonal_to_plot, seasonal_to_plot$site)
lapply(list_seasons, plot_seasonal_cycle)

#IAV correlations
Merged_Jung_Comp <- read.csv("D:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
str(Merged_Jung_Comp)
Merged_Jung_Comp <- Merged_Jung_Comp[!(is.na(Merged_Jung_Comp$year)),] 
summary(Merged_Jung_Comp)
Merged_Jung_Comp$date <- as.Date(Merged_Jung_Comp$date, format="%Y-%m-%d")
Merged_Jung_Comp$month <- month(Merged_Jung_Comp$date)
Merged_Jung_Comp$year <- year(Merged_Jung_Comp$date)

#Delete all files after 2013 (Fluxcom only goes through 2013)
Merged_Jung_Comp<-Merged_Jung_Comp[!(Merged_Jung_Comp$year > 2013 | Merged_Jung_Comp$year < 1999),]
new_DF <- Merged_Jung_Comp[is.na(Merged_Jung_Comp$Jung_GPP),]
new_DF
summary(Merged_Jung_Comp)
#get IAV (by site...)
#ddply to get annual sums of GPP (IAV)
out <- split(Merged_Jung_Comp, Merged_Jung_Comp$site.x)
str(out)
summer <- subset(Merged_Jung_Comp, month==6 | month==7 | month==8)
summer_out <- split(summer, summer$site.x)
spring <- subset(Merged_Jung_Comp, month==3 | month==4 | month==5)
spring_out <- split(spring, spring$site.x)
fall <- subset(Merged_Jung_Comp, month==9 | month==10 | month==11)
fall_out <- split(fall, fall$site.x)
winter <- subset(Merged_Jung_Comp, month==12 | month==1 | month==2)
winter_out <- split(winter, winter$site.x)

IAV_func <- function(x){
  df <- ddply(x, .(year, site.x), summarize, Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
  return(df)
}

require(psych)
rmssdfunc <- function(xx)
{
  return(data.frame(RMSSDGPP = rmssd(xx$GPP), RMSSDJungGPP= rmssd(xx$Jung_GPP)))
}

do.call(rbind, lapply(out, rmssdfunc))
IAV_to_plot <- do.call(rbind, lapply(out, IAV_func))
str(IAV_to_plot)
summer <- do.call(rbind, lapply(summer_out, IAV_func))
spring <- do.call(rbind, lapply(spring_out, IAV_func))
winter <- do.call(rbind, lapply(winter_out, IAV_func))
fall <- do.call(rbind, lapply(fall_out, IAV_func))

#ddply to get seasonal sums of GPP (Summer vs. spring vs. winter vs. fall)


require(plyr)
corfunc <- function(xx)
{
  return(data.frame(COR = cor(xx$GPP, xx$Jung_GPP)))
}



ddply(IAV_to_plot, .(site.x), corfunc)
ddply(spring, .(site.x), corfunc)
ddply(winter, .(site.x), corfunc)
ddply(fall, .(site.x), corfunc)
ddply(summer, .(site.x), corfunc)



