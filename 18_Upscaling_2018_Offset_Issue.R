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
#1) Merge daymet files with fixed flux files----------------------
#Read all three big .csv files and merge by date

#Flux
Flux_merge <- read.csv("D:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_2_2_2018.csv")
#Daymet
Daymet_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Daymet_monthly_all.csv") 
#MODIS
MODIS_merge <-  read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")

#Check files, delete "X", and format date col for merge 
str(Flux_merge)
Flux_merge$date <- as.Date(Flux_merge$date, format="%m/%d/%Y")
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
write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_15_2018.csv")

#Get random forest models (this code could be cleaned up significantly) 
#Run site-based RF with proper variables --------
library(lubridate)
library(caret)
library(randomForest)

#From UC-Irvine Machine learning repository
#Now Doing 3 different models: one for spring ("Mar-May), summer("Jun-Sep"), Inactive("Oct-"feb")
All_sites <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_15_2018.csv") 
#Print first lines
head(All_sites)
#Fix column names and add numeric columns
str(All_sites)
All_sites$elev <- as.numeric(All_sites$elev)
All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
All_sites$month <- as.numeric(All_sites$month)
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
str(All_sites)
nacols(All_sites)
All_sites
#All_sites <- subset(All_sites, month== 4 | month== 5| month== 6| month==7 | month==8 | month==9)
All_sites <- All_sites[c("GPP", "date", "daylength", "site", "elev", "month", "srad", "swe", "tmed", "tmax", "tmin", "BAL", "PET", 
                         "precip", "vp", "MAP", "MAT", "NDVI", "spei1", "spei3", "spei6", "spei9", "spei12")]

str(All_sites)
All_sites <- All_sites[complete.cases(All_sites),]
str(All_sites)
apply(All_sites, 2, function(x) any(is.nan(x)))
apply(All_sites, 2, function(x) any(is.na(x)))
apply(All_sites, 2, function(x) any(is.infinite(x)))

All_sites$date <- as.Date(All_sites$date)
nacols(All_sites)
head(All_sites)
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

apply(All_sites.training, 2, function(x) any(is.nan(x)))
apply(All_sites.training, 2, function(x) any(is.na(x)))
apply(All_sites.training, 2, function(x) any(is.infinite(x)))

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
#Model wtih all + SPEI_1
colsA2 <- c(3:4, 7:14)
head(All_sites.training)
head(All_sites.training[,colsA2])
head(All_sites.training[,5:6])

All_sites.training[!complete.cases(All_sites.training),]

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
myControl <- trainControl(method="repeatedcv", repeats=5, number=10)


model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="cv", number=5), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="cv", number=5, classProbs = TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)


model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="cv", number=5), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA2 <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA2 <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

predict_rfA1 <- as.numeric(predict(object=model_rfA1, All_sites.test[,colsA1]))
predict_tsA1 <- as.numeric(predict(object=model_rfA1, All_sites.test[,colsA1]))

predict_rfA2 <- as.numeric(predict(object=model_rfA2, All_sites.test[,colsA2]))
predict_tsA2 <- as.numeric(predict(object=model_rfA2, All_sites.test[,colsA2]))

predict_rfA3 <- as.numeric(predict(object=model_rfA3, All_sites.test[,colsA3]))
predict_rfA3 <- as.numeric(predict(object=model_rfA3, All_sites.test[,colsA3]))


cor(pred_rfA1, All_sites.test[,1])
cor(pred_tsA1, All_sites.test[,1])
cor(pred_rfA2, All_sites.test[,1])
cor(pred_tsA2, All_sites.test[,1])
cor(pred_rfA3, All_sites.test[,1])
cor(pred_tsA3, All_sites.test[,1])

RF_F1 <- model_rfA1$finalModel
RF_T1 <- model_tsA1$finalModel
RF_F2 <- model_rfA2$finalModel
RF_T2 <- model_tsA2$finalModel
RF_F3 <- model_rfA3$finalModel
RF_T3 <- model_tsA3$finalModel

varImpPlot(RF_F1)
varImpPlot(RF_T1)
varImpPlot(RF_F2)
varImpPlot(RF_T2)
varImpPlot(RF_F3)
varImpPlot(RF_T3)


saveRDS(RF_F1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F1_2_16.rds")
saveRDS(RF_T1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T1_2_16.rds")
saveRDS(RF_F2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F2_2_16.rds")
saveRDS(RF_T2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T2_2_16.rds")
saveRDS(RF_F3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F3_2_16.rds")
saveRDS(RF_T3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T3_2_16.rds")


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



