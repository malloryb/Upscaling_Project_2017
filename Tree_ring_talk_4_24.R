#3/9/2018 - Model is not looking great. Need to get seasonal variance better. 
#Here's what I did: 1) Oversampled observations with 'high"GPP- which was GPP above 1.5
#I doubled the observations with high GPP and added them to the training (80%) dataset
#I am also testing out some different resampling methods. The timeslice won't work anymore
#Because I have extra values of high GPP. Doing a LOO cross validation and some k-folds.
#Also worked on parallel processing to try and speed things up. Still slow though. 
#Maybe run on storm? 

#Run site-based RF with proper variables ------------------
#devtools::install_github('topepo/caret/pkg/caret', dependencies = c("Depends", "Imports", "Suggests"))
library(lubridate)
library(caret)
library(randomForest)
library(plyr)
library(dplyr)
library(reshape2)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Add NDVI in addition to EVI to the Allsites file
setwd("F:/Upscaling_Project/MODIS_NDVI/")

modis_ndvi <- function(file){
  filename <- tools::file_path_sans_ext(basename(file))
  print(filename)
  tstndvi <- read.csv(file)
  tstndvi <- dplyr::rename(tstndvi, date=system.time_start, NDVI=NDVI_1)
  tstndvi$month <- substr(as.character(tstndvi$date), 1,3)
  tstndvi$year <- substrRight(as.character(tstndvi$date),4)
  tstndvi$monthyear <- paste(tstndvi$month, tstndvi$year, sep="-")
  tstndvi$date <- paste(tstndvi$monthyear, "01", sep="-")
  mergendvi <- ddply(tstndvi, ~date, summarize, NDVI=mean(NDVI, na.rm=TRUE))
  mergendvi$site <- paste("us", substr(filename, 1,3), sep="-")
  return(mergendvi)
  }

ndvilist <- list.files("F:/Upscaling_Project/MODIS_NDVI/")
tst <- do.call("rbind", lapply(ndvilist, modis_ndvi))

#Load data
All_sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv") 
str(All_sites)
All_sites <- rename(All_sites, EVI=NDVI)
cutoff_index <- ddply(All_sites, .(site), summarize, cutoff=quantile(GPP, 0.75, na.rm=TRUE))
cutoff_index <- cutoff_index[-c(23),]
All_sites <- merge(All_sites, cutoff_index, by="site")
summary(All_sites$GPP)
#Fix column names and add numeric columns
str(All_sites)
#Values above 3rd quartile
All_sites$period <- as.factor(ifelse(All_sites$GPP <1.6, "Inactive", "Active"))
#Values above 4th quartile
All_sites$period2 <-as.factor(ifelse(All_sites$GPP <2.5, "Inactive", "Active"))
All_sites$period3 <- as.factor(ifelse(All_sites$month >10 | All_sites$month <3, "0", "1"))
All_sites$period4 <- as.factor(ifelse(All_sites$GPP < All_sites$cutoff, "Low", "High"))
All_sites$elev <- as.numeric(All_sites$elev)
All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
All_sites$precip <- as.numeric(All_sites$precip)
All_sites$swe <- as.numeric(All_sites$swe)
All_sites$GPPsquared <- (All_sites$GPP*All_sites$GPP)
index <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
values <- c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall", "winter")
All_sites$season <- values[match(All_sites$month, index)]
All_sites$month <- as.factor(All_sites$month)
All_sites$season <- as.factor(All_sites$season)

str(All_sites)
All_sites <- All_sites[c("GPP", "date", "daylength", "site", "elev", "month", "srad", "swe", "tmed", "tmax", "tmin", "BAL", "PET", 
                         "precip", "vp", "MAP", "MAT", "NDVI", "spei1", "spei3", "spei6", "spei9", "spei12","period", "period2","season", "period3", "period4")]
#Trying to plot
#Rows are -Inf for SPEI3 for some reason. Fixing 266 and 1448 by just using the mean of the surrounding two values.
which(sapply(All_sites$spei3, is.infinite))

All_sites$spei3[265:267]
All_sites$spei3[266] <- 0.908
All_sites$spei3[1447:1449]
All_sites$spei3[1448] <- -1.0609

#Get rid of non-complete cases and double check
summary(All_sites)
All_sites <- All_sites[complete.cases(All_sites),]
summary(All_sites)
#Split into training and testing data
set.seed(455)
index <- createDataPartition(All_sites$GPP, p=0.80, list=FALSE)
index
#Subset training set
All_sites.training <- All_sites[index,]
All_sites.training[!complete.cases(All_sites.training),]
All_sites.test <- All_sites[-index,]

str(All_sites.training)
#Here's where I double the number of 'high gpp observations' - two levels: high and very high
hist(All_sites.training[,"GPP"])
All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
hist(All_sites.training[,"GPP"])
All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
hist(All_sites.training[,"GPP"])
All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
hist(All_sites.training[,"GPP"])
#All_sites.training[,"GPP"] <- (All_sites.training[,"GPP"])*(All_sites.training[,"GPP"])
#did this 2 times

#Now going to try the cost function instead and skipping the above lines (70-77)
#snagged from: https://github.com/cran/AppliedPredictiveModeling/blob/master/inst/chapters/17_Job_Scheduling.R
cost <- function(pred, obs)
{
  isNA <- is.na(pred)
  if(!all(isNA))
  {
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    
    cost <- ifelse(pred == obs, 0, 1)
    if(any(pred == "VF" & obs == "L")) cost[pred == "L" & obs == "VF"] <- 10
    if(any(pred == "F" & obs == "L")) cost[pred == "F" & obs == "L"] <- 5
    if(any(pred == "F" & obs == "M")) cost[pred == "F" & obs == "M"] <- 5
    if(any(pred == "VF" & obs == "M")) cost[pred == "VF" & obs == "M"] <- 5
    out <- mean(cost)
  } else out <- NA
  out
}

costSummary <- function (data, lev = NULL, model = NULL)
{
  cost <- function(pred, obs)
  {
    isNA <- is.na(pred)
    if(!all(isNA))
    {
      pred <- pred[!isNA]
      obs <- obs[!isNA]
      
      cost <- ifelse(pred == obs, 0, 1)
      if(any(pred == "VF" & obs == "L")) cost[pred == "L" & obs == "VF"] <- 10
      if(any(pred == "F" & obs == "L")) cost[pred == "F" & obs == "L"] <- 5
      if(any(pred == "F" & obs == "M")) cost[pred == "F" & obs == "M"] <- 5
      if(any(pred == "VF" & obs == "M")) cost[pred == "VF" & obs == "M"] <- 5
      out <- mean(cost)
    } else out <- NA
    out
  }
  
  if (is.character(data$obs))  data$obs <- factor(data$obs, levels = lev)
  c(postResample(data[, "pred"], data[, "obs"]),
    Cost = cost(data[, "pred"], data[, "obs"]))
}


#Don't mess with the test data though
summary(All_sites.training[,"GPP"])
#Gonna try squaring GPP
#All_sites.training[,"GPP"] <- (All_sites.training[,"GPP"])*(All_sites.training[,"GPP"])
#All_sites.training[,"GPP"] <- log(All_sites.training[,"GPP"])


str(All_sites.test)

#Check all values are finite

is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

is.finite.data.frame(All_sites.training)

#Overview of algorithms supported by caret function
names(getModelInfo())
head(All_sites)

#Model with the following columns:
colsA1 <- c("daylength", "elev", "month", "srad", "tmax", "tmin", "precip", "vp", "MAP", "MAT", "NDVI", "spei6",
            "spei12", "spei1")

head(All_sites.training[,colsA1])
head(All_sites.training[,1:2])
str(All_sites.training[,colsA1])
str(All_sites.training[,"GPP"])

madSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out <- mad(data$obs - data$pred,
             na.rm = TRUE)
  names(out) <- "MAD"
  out
}

#Initializing parallel processing
library(parallel)
library(doParallel)

cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)
#ctrl1 <- trainControl(method = "LOOCV", allowParallel = TRUE)
ctrl2 <- trainControl(method = "repeatedcv", number=5, repeats=3, allowParallel = TRUE)
ctrl3 <- trainControl(method = "repeatedcv", number=5, repeats=3, summaryFunction=costSummary, allowParallel = TRUE)

#model_tsC2 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=ctrl1, importance=TRUE, do.trace=TRUE)
model_tsC3 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=ctrl2, importance=TRUE, do.trace=TRUE)
model_tsC4 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='pcaNNet', trControl=ctrl2, importance=TRUE, do.trace=TRUE)

#Stop cluster
stopCluster(cluster)
registerDoSEQ()

#Look at variable importance of models
varImp(model_tsC2)
varImp(model_tsC3)
varImp(model_tsc4)
pred_tsC2 <- as.numeric(predict(object=model_tsC2, All_sites.test[,colsA1]))
pred_tsC3 <- as.numeric(predict(object=model_tsC3, All_sites.test[,colsA1]))
pred_tsC4 <- as.numeric(predict(object=model_tsC4, All_sites.test[,colsA1]))

cor(pred_tsC2, All_sites.test[,1])
cor(pred_tsC3, All_sites.test[,1])
cor(pred_tsC4, All_sites.test[,1])

postResample(pred=pred_tsC2, obs=All_sites.test[,1])
postResample(pred=pred_tsC3, obs=All_sites.test[,1])

RF_C2 <- model_tsC2$finalModel
RF_C3 <- model_tsC3$finalModel
RF_C4 <- model_tsC4

varImpPlot(RF_C2)
varImpPlot(RF_C3)

saveRDS(RF_C2, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C2_3_10.rds")
saveRDS(RF_C3, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_4_22.rds")


