#3/9/2018 - Model is not looking great. Need to get seasonal variance better. 
#Here's what I did: 1) Oversampled observations with 'high"GPP- which was GPP above 1.5
#I doubled the observations with high GPP and added them to the training (80%) dataset
#I am also testing out some different resampling methods. The timeslice won't work anymore
#Because I have extra values of high GPP. Doing a LOO cross validation and some k-folds.
#Also worked on parallel processing to try and speed things up. Still slow though. 
#Maybe run on storm? 

#To do 12/3/2018: 
#Figure out how to do training/cross validtion with 80% of the SITES not 80% of the observations
#This may actually reduce the need for oversampling
#Look into randomforest explainer package too

#Run site-based RF with proper variables ------------------
#devtools::install_github('topepo/caret/pkg/caret', dependencies = c("Depends", "Imports", "Suggests"))
sessionInfo()
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
setwd("/Users/mallory/Dropbox (Dissertation Dropbox)/Upscaling_Projct/MODIS_NDVI/")
modis_ndvi <- function(file){
  filename <- tools::file_path_sans_ext(basename(file))
  print(filename)
  tstndvi <- read.csv(file)
  tstndvi <- dplyr::rename(tstndvi, date=system.time_start, NDVI=NDVI_1)
  tstndvi$month <- substr(as.character(tstndvi$date), 1,3)
  tstndvi$year <- substrRight(as.character(tstndvi$date),4)
  tstndvi$monthyear <- paste(tstndvi$month, tstndvi$year, sep="-")
  tstndvi$date <- paste(tstndvi$monthyear, "01", sep="-")
  mergendvi <- ddply(tstndvi, ~monthyear, summarize, NDVI=mean(NDVI, na.rm=TRUE))
  mergendvi$site <- paste("us", substr(filename, 1,3), sep="-")
  mergendvi$sitedate <- paste(mergendvi$site, mergendvi$monthyear, sep="-")
  return(mergendvi)
  }

ndvilist <- list.files("/Users/mallory/Dropbox (Dissertation Dropbox)/Upscaling_Projct/MODIS_NDVI/")
ndvi <- do.call("rbind", lapply(ndvilist, modis_ndvi))

#Load data
All_sites1 <- read.csv("/Users/mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv") 
str(All_sites1)
All_sites1$sitedate <- paste(All_sites1$site, All_sites1$monthyear, sep="-")
All_sites1 <- rename(All_sites1, EVI=NDVI)
All_sites1 <- merge(All_sites1, ndvi, by="sitedate")
#now clean it up
All_sites <- dplyr::select(All_sites1, -c(X.1,X, X1, monthyear.y, site.y))
All_sites <- rename(All_sites, monthyear=monthyear.x, site=site.x)
All_sites$diff <- All_sites$EVI - All_sites$NDVI
subsites <- subset(All_sites, diff >0.2)
levels(subsites$site)
cor(All_sites$NDVI, All_sites$EVI)
qplot(All_sites$EVI, All_sites$NDVI)

ggplot(data=All_sites, aes(x=date, y=diff, colour=site, group=1)) +
  geom_line(size=1.2)


#cutoff_index <- ddply(All_sites, .(site), summarize, cutoff=quantile(GPP, 0.75, na.rm=TRUE))
#cutoff_index <- cutoff_index[-c(23),]
#All_sites <- merge(All_sites, cutoff_index, by="site")
summary(All_sites$GPP)
#Fix column names and add numeric columns
str(All_sites)
#Values above 3rd quartile
#All_sites$period <- as.factor(ifelse(All_sites$GPP <1.6, "Inactive", "Active"))
#Values above 4th quartile
#All_sites$period2 <-as.factor(ifelse(All_sites$GPP <2.5, "Inactive", "Active"))
#All_sites$period3 <- as.factor(ifelse(All_sites$month >10 | All_sites$month <3, "0", "1"))
#All_sites$period4 <- as.factor(ifelse(All_sites$GPP < All_sites$cutoff, "Low", "High"))
All_sites$elev <- as.numeric(All_sites$elev)
All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
All_sites$precip <- as.numeric(All_sites$precip)
All_sites$swe <- as.numeric(All_sites$swe)
#All_sites$GPPsquared <- (All_sites$GPP*All_sites$GPP)
#index <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#values <- c("winter", "winter", "spring", "spring", "spring", "summer", "summer", "summer", "fall", "fall", "fall", "winter")
#All_sites$season <- values[match(All_sites$month, index)]
All_sites$month <- as.factor(All_sites$month)
#All_sites$season <- as.factor(All_sites$season)

str(All_sites)
All_sites <- All_sites[c("GPP", "date", "daylength", "site", "elev", "month", "srad", "swe", "tmed", "tmax", "tmin", "BAL", "PET", 
                         "precip", "vp", "MAP", "MAT", "NDVI", "EVI", "spei1", "spei3", "spei6", "spei9", "spei12","period", "period2","season", "period3", "period4")]
#Trying to plot
#Rows are -Inf for SPEI3 for some reason. Fixing 236 and 1461 by just using the mean of the surrounding two values.
which(sapply(All_sites$spei3, is.infinite))

All_sites$spei3[235:237]
All_sites$spei3[236] <- 0.908
All_sites$spei3[1460:1462]
All_sites$spei3[1461] <- -1.0609

#Get rid of non-complete cases and double check
summary(All_sites)
All_sites <- All_sites[complete.cases(All_sites),]
summary(All_sites)
#Split into training and testing data
set.seed(455)
index <- createDataPartition(All_sites$site, p=0.80, list=FALSE)
index
#Subset training set
All_sites.training <- All_sites[index,]
All_sites.training[!complete.cases(All_sites.training),]
All_sites.test <- All_sites[-index,]

str(All_sites.training)
#Here's where I double the number of 'high gpp observations' - two levels: high and very high
#hist(All_sites.training[,"GPP"])
#All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
#hist(All_sites.training[,"GPP"])
#All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
#hist(All_sites.training[,"GPP"])
#All_sites.training <- rbind(All_sites.training, subset(All_sites.training, period4=="High"))
#hist(All_sites.training[,"GPP"])
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

colsA2 <- c("daylength", "elev", "month", "srad", "tmax", "tmin", "precip", "vp", "MAP", "MAT", "EVI", "spei6",
            "spei12", "spei1")

head(All_sites.training[,colsA2])
head(All_sites.training[,1:2])
str(All_sites.training[,colsA2])
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
#ctrl3 <- trainControl(method = "repeatedcv", number=5, repeats=3, summaryFunction=costSummary, allowParallel = TRUE)

#model_tsC2 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=ctrl1, importance=TRUE, do.trace=TRUE)
model_tsC3 <- train(All_sites.training[,colsA1], All_sites.training[,"GPP"], method='rf', trControl=ctrl2, importance=TRUE, do.trace=TRUE)
model_tsC4 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=ctrl2, importance=TRUE, do.trace=TRUE)

#Stop cluster
stopCluster(cluster)
registerDoSEQ()

#Look at variable importance of models
varImp(model_tsC3)
varImp(model_tsC4)
pred_tsC3 <- as.numeric(predict(object=model_tsC3, All_sites.test[,colsA1]))
pred_tsC4 <- as.numeric(predict(object=model_tsC4, All_sites.test[,colsA2]))

cor(pred_tsC3, All_sites.test[,"GPP"])
cor(pred_tsC4, All_sites.test[,1])

postResample(pred=pred_tsC3, obs=All_sites.test[,"GPP"])

RF_C3 <- model_tsC3$finalModel
RF_C4 <- model_tsC4$finalModel

varImpPlot(RF_C3)
varImpPlot(RF_C4)

saveRDS(RF_C3, "/Users/mallory/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_NDVI_12_6.rds")
saveRDS(RF_C4, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_EVI_4_24.rds")

#Testing seasonal cycle------------------------------

RF_Val_Input_Created_Subset <- function(month, monthno, year, bandsp){
  #This function is to apply a single model to the input data - and write out the results in a table
  #Read in files
  filename <- paste0("Upscaling_Project/Gridded_Inputs/",month,"_",year, "_test2.tif")
  filenamespei6 <- "Upscaling_Project/SPEIBase/spei06.nc"
  rast_stack <- stack(filename)
  rast_ext <- extent(rast_stack[[1]])
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "spei1", "daylength", "spei12"))
  period3 = raster (ext=rast_ext, res=0.002081004)
  monthno2 <- as.numeric(monthno)
  print(monthno2)
  values(period3) <- as.numeric(ifelse(monthno2 >10 | monthno2 <3, "0", "1"))
  spei6 <- raster::brick(filenamespei6)
  spei6 <- spei6[[bandsp]]
  spei6 <- crop(spei6, rast_ext)
  print("Resampling SPEI...")
  spei6resample <- resample(spei6, rast_stack[[1]], method="bilinear")
  print("Resampling finished...")
  rast_stack <- addLayer(rast_stack, spei6resample, period3)
  
  print(filename)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "spei1", "daylength", "spei12", "spei6", "period3"))
  print("raster stacked")
  
  #Predict and write out model P3
  sw <- extent(rast_stack)
  print(sw)
  
  create_extent <- function(x){
    radius <- 0.02 # radius in kilometers
    yPlus <- x[1,2]+ radius
    xPlus <- x[1,1]+ radius
    yMinus <- x[1,2] - radius
    xMinus <- x[1,1] - radius
    e <- extent(xMinus, xPlus, yMinus, yPlus)
    return(e)
  }
  
  #Read models
  RFP3<- readRDS("/Users/mallory/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_NDVI_12_6.rds")
  
  #For each site- write function and lapply over list of extents? Or at least function to apply models and write them out
  FUFpoint <- cbind(-111.762,	35.089)
  SEGpoint<- cbind(-106.7,	34.360)
  SRMpoint <- cbind(-110.866, 31.821)
  VCMpoint<- cbind(-106.532,	35.888)
  VCPpoint<- cbind(-106.597,	35.864)
  WHSpoint<- cbind(-110.052,	31.744)
  WKGpoint <- cbind(-109.942,	31.737)
  
  fufex <- create_extent(FUFpoint)
  fuf <- crop(rast_stack,fufex)
  fuf <- extent(fuf)
  
  segex <- create_extent(SEGpoint)
  seg <- crop(rast_stack,segex)
  seg <- extent(seg)
  
  srmex <- create_extent(SRMpoint)
  srm <- crop(rast_stack,srmex)
  srm <- extent(srm)
  
  vcmex <- create_extent(VCMpoint)
  vcm <- crop(rast_stack,vcmex)
  vcm <- extent(vcm)
  
  vcpex <- create_extent(VCPpoint)
  vcp <- crop(rast_stack,vcpex)
  vcp <- extent(vcp)
  
  whsex <- create_extent(WHSpoint)
  whs <- crop(rast_stack,whsex)
  whs <- extent(whs)
  
  wkgex <- create_extent(WKGpoint)
  wkg <- crop(rast_stack,wkgex)
  wkg <- extent(wkg)
  
  print("extents created")
  
  RFP1_predictedfuf <- predict(rast_stack, RFP3, ext=eval(fuf))
  RFP1_predictedseg <- predict(rast_stack, RFP3, ext=eval(seg))
  RFP1_predictedsrm <- predict(rast_stack, RFP3, ext=eval(srm))
  RFP1_predictedvcm <- predict(rast_stack, RFP3, ext=eval(vcm))
  RFP1_predictedvcp <- predict(rast_stack, RFP3, ext=eval(vcp))
  RFP1_predictedwhs <- predict(rast_stack, RFP3, ext=eval(whs))
  RFP1_predictedwkg <- predict(rast_stack, RFP3, ext=eval(wkg))
  
  P1fuf <-extract(RFP1_predictedfuf, FUFpoint) 
  P1fuf[is.na(P1fuf[])] <- cellStats(RFP1_predictedfuf,stat='mean', na.rm=TRUE)
  P1seg <-extract(RFP1_predictedseg, SEGpoint) 
  P1seg[is.na(P1seg[])] <- cellStats(RFP1_predictedseg,stat='mean', na.rm=TRUE)
  P1srm <-extract(RFP1_predictedsrm, SRMpoint) 
  P1srm[is.na(P1srm[])] <- cellStats(RFP1_predictedsrm,stat='mean', na.rm=TRUE)
  P1vcm<-extract(RFP1_predictedvcm, VCMpoint) 
  P1vcm[is.na(P1vcm[])] <- cellStats(RFP1_predictedvcm,stat='mean', na.rm=TRUE)
  P1vcp <-extract(RFP1_predictedvcp, VCPpoint) 
  P1vcp[is.na(P1vcp[])] <- cellStats(RFP1_predictedvcp,stat='mean', na.rm=TRUE)
  P1whs <-extract(RFP1_predictedwhs, WHSpoint) 
  P1whs[is.na(P1whs[])] <- cellStats(RFP1_predictedwhs,stat='mean', na.rm=TRUE)
  P1wkg <-extract(RFP1_predictedwkg, WKGpoint) 
  P1whs[is.na(P1wkg[])] <- cellStats(RFP1_predictedwkg,stat='mean', na.rm=TRUE)
  
  output <- cbind(P1fuf, P1seg, P1srm, P1vcm, P1vcp, P1whs, P1wkg)
  print(output)
  return(output)
  gc()
}


#Hokay giving up for now, format GPP now----------------
setwd("F:/Upscaling_Project/MODIS_GPP/")

modis_GPP <- function(file){
  filename <- tools::file_path_sans_ext(basename(file))
  print(filename)
  tstgpp <- read.csv(file)
  tstgpp <- dplyr::rename(tstgpp, date=system.time_start, gpp=Gpp_1)
  tstgpp$month <- substr(as.character(tstgpp$date), 1,3)
  tstgpp$year <- substrRight(as.character(tstgpp$date),4)
  tstgpp$monthyear <- paste(tstgpp$month, tstgpp$year, sep="_")
  tstgpp$date <- paste(tstgpp$monthyear, "01", sep="_")
  mergegpp <- ddply(tstgpp, ~monthyear, summarize, modisgpp=mean(gpp, na.rm=TRUE))
  mergegpp$site <- paste("us", substr(filename, 10,12), sep="_")
  mergegpp$sitedate <- paste(mergegpp$monthyear, mergegpp$site, sep="_")
  keeps <- c("modisgpp", "sitedate")
  merged <- mergegpp[keeps]
  return(merged)
}

gpplist <- list.files("F:/Upscaling_Project/MODIS_GPP/")
gpp <- do.call("rbind", lapply(gpplist, modis_GPP))
str(gpp)
#merge with gpp and re-plot key figs--------------------
#Fig 1: 
setwd("F:/Upscaling_Project/Upscaled_GPP/RF_C3/")
lst <- list.files("F:/Upscaling_Project/Upscaled_GPP/RF_C3/", pattern=".csv")
#Format output from RFC3 flux file 
format_Barnesoutput <- function(xx){
  print("read file")
  filename <- tools::file_path_sans_ext(basename(xx))
  xx <- read.csv(xx)
  require(stringr)
  require(lubridate)
  #get function filename
  #read in site 
  print(filename)
  xx$year <- substr(filename,10,13)
  #some sort of lookup table is needed 
  xx$monthyear <- paste(xx$month, xx$year, sep="_")
  xx$date <- as.Date(paste("01", xx$monthyear, sep="_"), format="%d_%m_%Y")
  xx$monthyear <- format(xx$date,"%b_%Y")
  return(xx)
}

RF_C3 <- do.call("rbind", lapply(lst, format_Barnesoutput))
RF_C3$X <- NULL
#Name sites properly
names(RF_C3) <- c("us_aud", "us_cop", "us_fuf", "us_mpj", "us_scc", "us_scf", 
                  "us_scw", "us_seg", "us_sen", "us_ses", "us_so2", "us_so3", 
                  "us_so4", "us_srg", "us_srm", "us_vcm", "us_vcp", "us_whs", 
                  "us_wjs", "us_wkg", "month", "year", "monthyear", "date")
str(RF_C3)
#Put in long format for plotting
RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_aud:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
str(RFC3_long)
str(gpp)
RFC3_long <- merge(RFC3_long, gpp, by="sitedate")
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
levels(FluxJung$site.x)
levels(RFC3_long$site)
merged <- merge(RFC3_long, FluxJung, all.x=T)
str(merged)
#Merge with SPEI Going to use site-based SPEI 12 for now
getspei <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv")
str(getspei)
getspei$site <- (gsub('([[:punct:]])|\\-','_', getspei$site))
getspei$monthyear <- (gsub('([[:punct:]])|\\-','_', getspei$monthyear))
getspei$sitedate <- as.factor(paste(getspei$monthyear, getspei$site, sep="_"))
spei <- getspei[,c("sitedate","spei12","spei1", "spei6", "spei3")]
str(spei)
levels(getspei$sitedate)
levels(merged$sitedate)
#Function to calculate correlation between spei and monthly GPP 
corfunc <- function(gg){
  require(plyr)
  return(data.frame(CORFlux12 = cor(gg$GPP, gg$spei1, use="complete.obs"), pvalFlux12=cor.test(gg$GPP, gg$spei1)$p.value, 
                    CORBarnes12= cor(gg$Barnes_GPP, gg$spei1, use="complete.obs"), pvalBarnes12=cor.test(gg$Barnes_GPP, gg$spei1)$p.value,
                    CORModis12= cor(gg$modisgpp, gg$spei1, use="complete.obs"), pvalModis12=cor.test(gg$modisgpp, gg$spei1)$p.value,
                    CORJung12= cor(gg$Jung_GPP, gg$spei1, use="complete.obs"), pvalJung12=cor.test(gg$Jung_GPP, gg$spei1)$p.value))
}

#Merge SPEI with Flux, Barnes, and Jung
for_fig_1 <- merge(merged, spei, all.x=T)
for_fig_1$X <- NULL
for_fig_1$site.x <- NULL
for_fig_1

#Testing out the corfunc
corfunc(for_fig_1)
#Checking out IAV--------------
for_fig_1<-for_fig_1[!(for_fig_1$year > 2013 | for_fig_1$year < 1999),]
#Subset values where GPP is a value
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
summary(for_fig_1)
for_fig_1[100:150,]
library(plyr)
#Get IAV for IAV plots
IAV_to_plot <-ddply(for_fig_1, .(year, site), summarize, Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Modis_GPP = sum(modisgpp, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
#Get figure 1 formatting data ready (corrs with SPEI)
Fig1_to_plot <-ddply(Subs1, .(year, site), summarize, SPEI_12max=max(abs(spei12)), SPEI_12mean=mean(spei12, na.rm=TRUE),
                     SPEI_6max=max(abs(spei6)), SPEI_6mean=mean(spei6, na.rm=TRUE),
                     SPEI_1max=max(abs(spei1)), SPEI_1mean=mean(spei1, na.rm=TRUE),
                     Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Modis_GPP=sum(modisgpp, na.rm=TRUE), 
                     Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))

#Plot sensitivity 
library(ggplot2)
library(ggpubr)
library(ggthemes)
#Colors: 
#Barnes: #BD2031
#Flux: #5F20BD
#Jung #7FBD20
Fig1_to_plot
#write.csv(Fig1_to_plot, "F:/Upscaling_Project/fig1_to_plot.csv")
Fig1_to_plot <- read.csv("F:/Upscaling_Project/fig1_to_plot.csv")
pp1 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#BD2031") +  ylim(0,55)+
  xlab("12 month SPEI")+ ylab("Tower GPP")+
  ggtitle("Observed GPP")+ theme_few(base_size=12)

pp2 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = Barnes_GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#5F20BD")+ ylim(0,55)+
  xlab("12 month SPEI")+ ylab("DryFlux GPP")+
  ggtitle("DryFlux GPP")+ theme_few(base_size=12)

pp3 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = Jung_GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#7FBD20")+ylim(0,55)+
  xlab("12 month SPEI")+ ylab("Fluxcom GPP")+
  ggtitle("Fluxcom GPP")+ theme_few(base_size=12)

pp4 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = Modis_GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="cyan2")+ylim(0,55)+
  xlab("12 month SPEI")+ ylab("MODIS GPP")+
  ggtitle("MODIS GPP")+ theme_few(base_size=12)


ggarrange(pp1, pp3, pp2, pp4)

#Figure 1b---------------------------------------------------------

For1b <- read.csv("F:/Upscaling_Project/For_Fig_1_4_24.csv")
modeled_GPP <- subset(For1b, GPP=="BarnesGPP" | GPP=="JungGPP" | GPP=="modisgpp")
tower_GPP <- subset(For1b, GPP=="FluxGPP")
#Getting correlation for ...
tower_GPP <- subset(For1b, GPP=="FluxGPP")
Fluxcom_GPP <- subset(For1b, GPP=="JungGPP")
DryFlux_GPP<- subset(For1b, GPP=="BarnesGPP")
MODIS_GPP<- subset(For1b, GPP=="modisgpp")

mean(tower_GPP$cor)
mean(Fluxcom_GPP$cor)
mean(DryFlux_GPP$cor)
mean(MODIS_GPP$cor)

ggplot(modeled_GPP, aes(x = reorder(site,order_site), y = cor, fill= GPP)) +
  geom_bar(stat="identity", position="dodge")+
  geom_point(data=tower_GPP, aes(site, cor), shape=95, size=20, color="#BD2031")+
  scale_fill_manual("legend", values =c("#5F20BD", "#BD2031", "#7FBD20"))+
  xlab("Flux Sites")+ ylab("Correlation between GPP and 12-month SPEI")+
  theme_few(base_size=13)+
  theme(axis.text.x = element_text(angle = -90))+
  theme(legend.position="none")

#Figure 2-----------------------------
Subs1<-subset(For1b, (!is.na(For1b[,9])))
str(Subs1)
#Create data frame for seasonal cycle plotting
plot_seasonal_cycle <- ddply(Subs1, .(month, site), summarize, Barnes_GPP_se=sd(Barnes_GPP, na.rm=TRUE)/sqrt(length(Barnes_GPP[!is.na(Barnes_GPP)])), Barnes_GPP=mean(Barnes_GPP, na.rm=TRUE), 
                             Modis_GPP_se=sd(modisgpp, na.rm=TRUE)/sqrt(length(modisgpp[!is.na(modisgpp)])), Modis_GPP=mean(modisgpp, na.rm=TRUE),Jung_GPP_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])), 
                             Jung_GPP=mean(Jung_GPP, na.rm=TRUE), 
                             GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])), GPP=mean(GPP, na.rm=TRUE))
str(Subs1)
plot_seasonal_cycle
setwd("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Final Dissertation/")

list_seasons <- split(plot_seasonal_cycle, plot_seasonal_cycle$site)
str(list_seasons)
plot_seasonal_cycle_1 <- function(x){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  require("ggpubr")
  print(str(x))
  droplevels(x)
  #toscale <- sapply(x, is.numeric)
  #x[toscale] <- lapply(x[toscale], scale)
  
  head(x)
  
  B<- as.character(round(cor(x$GPP, x$Barnes_GPP, use="complete.obs"), 2))
  J<- as.character(round(cor(x$GPP, x$Jung_GPP, use="complete.obs"), 2))
  M<- as.character(round(cor(x$GPP, x$Modis_GPP, use="complete.obs"), 2))
  
  print("calculated cors")
  rmssdGPP <- as.character(round(rmssd(x$GPP), 2))
  rmssdB <- as.character(round(rmssd(x$Barnes_GPP), 2))
  rmssdJ <- as.character(round(rmssd(x$Jung_GPP), 2))
  rmssdM <- as.character(round(rmssd(x$Modis_GPP), 2))
  
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseModis =round(sqrt( mean((x$Modis_GPP-x$GPP)^2 , na.rm = TRUE )), 2)

  print("got RMSSD")
  lblGPP <- paste("RMSSDObserved =", rmssdGPP)
  lblB <- paste("rmseDryFlux =", rmseBarnes, ",r=", B, ",RMSSD=",rmssdB)
  lblJ <- paste("rmseFluxcom =", rmseJung, ",r=", J, ",RMSSD=",rmssdJ)
  lblM <- paste("rmseModis =", rmseModis, ",r=", M, ",RMSSD=",rmssdM)
  
  filename <- paste(x$site[1], "seasonal_comparison_4_24.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(paste(x$site, "RFC3", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("#BD2031")), size=2) +
    geom_line(data = x, aes(x = month, y = Barnes_GPP, color = I("#5F20BD")), size=2) +
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("#7FBD20")), size=2) +
    geom_line(data = x, aes(x = month, y = Modis_GPP, color = I("cyan2")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=Barnes_GPP- Barnes_GPP_se, ymax=Barnes_GPP+Barnes_GPP_se),colour="#5F20BD")+
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#BD2031")+
    geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    geom_errorbar(data=x,aes(x=month, ymin=Modis_GPP- Modis_GPP_se, ymax=Modis_GPP+Modis_GPP_se),colour="cyan2")+
    annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    annotate("text", label = lblM, parse=FALSE, x = 6, y = 9, size = 7, colour = "cyan2")+
        scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =12)+
    theme(legend.position = c(0, 0))
  plot(q)
  filename <- paste(x$site[1], "seasonalRF_comparison_4_24.png", sep="_")
  ggsave(filename, device='png', width=16, height=16, dpi = 300, units = "cm")
}

#-----------------------------------------------------------------
fuf <- list_seasons[[22]]
plot_seasonal_cycle_1(list_seasons[[12]])
lapply(list_seasons, plot_seasonal_cycle_1)
getRMSE <- function(x){
  str(x)
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2) 
  df <- data.frame(rmseBarnes, rmseJung, x$site, x$month)
  colnames(df) <- c("Barnes","Jung", "site", "month")
  return(df)
  
}
list_seasons

RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_auF:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
#Figure 2B
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
merged2 <- merge(RFC3_long, FluxJung, all=T)
Jung <- dplyr::select(merged2, "sitedate", "GPP", "Jung_GPP")
Barnes <- dplyr::select(merged2, "sitedate", "GPP", "Barnes_GPP")
Barnes$model <- "Barnes"
Jung$model <- "Jung"
Jung <- Jung[complete.cases(Jung), ] 
Barnes <- Barnes[complete.cases(Barnes), ]
str(Jung)
colnames(Jung)[3] <- "model_GPP"
colnames(Barnes)[3] <- "model_GPP"
str(Barnes)
str(Jung)
fig_2b_plot <- rbind(Barnes, Jung)
fig_2b_plot$model <- as.factor(fig_2b_plot$model)
levels(fig_2b_plot$model) <- gsub("Barnes", "DryFlux", levels(fig_2b_plot$model))
levels(fig_2b_plot$model) <- gsub("Jung", "Fluxcom", levels(fig_2b_plot$model))

lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 2));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}


group.colors <- c(Fluxcom = "#7FBD20", DryFlux = "#5F20BD")
fig_2b_plot
ggplot(fig_2b_plot, aes(x=GPP, y=model_GPP, group=model, colour=model))+
  xlim(0,9)+ylim(0,9.5)+
  annotate("text", x = 3.0, y = 9.2, colour = "#5F20BD", size = 5,
           label = lm_eqn(lm(model_GPP ~ GPP, Barnes)), parse = TRUE)+
  annotate("text", x = 3, y = 9.5, colour = "#7FBD20", size = 5,
           label = lm_eqn(lm(model_GPP ~ GPP, Jung)), parse = TRUE)+
  geom_smooth(method="lm",se=FALSE) +geom_point(alpha=0.5, size=1)+geom_abline(intercept=0,slope=1)+
  scale_color_manual(values=group.colors)+
  xlab("observed GPP")+
  ylab("modeled GPP")+
  theme_few(base_size=14)


head(fig_2b_plot)
str(fig_2b_plot)
version

