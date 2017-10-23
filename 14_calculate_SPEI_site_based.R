#File to calculate SPEI on site basis
#Current goal: SPEI (back from September) - 1, 3, 6, 9, and 12-month SPEI for all sites/all months. 
#Re-run RF models then
library(SPEI)
library(dplyr)
library(plyr)
library(raster)
library(data.table)
library(lubridate)

site_based <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_10_23_2017.csv")
str(site_based)
#first: compare thornthwaite and hargreaves methods of calculating SPEI 
#For thornthwaite -> will need to calculate mean monthly temp (or from MODIS_LST?)
#site_based$tmed <- ((site_based$tmax + site_based$tmin)/2)
#remove rows with NA for daymet data 

completeFun <- function(data, desiredCols){
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

setwd("C:/Users/Mallory/odrive/Dissertation Dropbox/Daymet/SPEI_calc/")

test_file <- read.csv("us-aud.csv")

#Function needs to: calculate hargreaves PET, calculate "BAL", then calculate: 1, 3, 6, 9 month SPEI
str(dat)
calc_spei <- function(x){
  header <- read.table(x, nrows = 1, header = FALSE, sep =';', stringsAsFactors = FALSE)
  header <- as.character(header$V1)
  latitude = substr(header, 10, 17)
  dat <- read.table(x, skip = 7, header = TRUE, sep =',')  
  setnames(dat, c("year", "yday", "daylength", "precip", "srad", "swe", "tmax", "tmin", "vp"))
  str(dat)
  dat$Date <- format(strptime(dat$yday, format="%j"), format="%m-%d")
  dat$Date <-paste(dat$year, dat$Date, sep = "-")
  str(dat$Date)
  dat$Date <- as.Date(dat$Date)
  str(dat$Date)
  dat$Date <- floor_date(dat$Date, "month")
  monthly <- ddply(dat, .(Date), summarise, precip=sum(precip, na.rm=TRUE), tmax=mean(tmax, na.rm=TRUE),
                  tmin=mean(tmin, na.rm=TRUE), tmed= mean(tmin+tmax, na.rm=TRUE)) 
  str(monthly)
  latitude <- as.numeric(latitude)
  monthly$PET_harg <- hargreaves(monthly$tmin, monthly$tmax, lat=latitude, na.rm=TRUE)
  monthly$BAL <- monthly$precip - monthly$PET_harg 
  spei_12 <- spei(monthly[,'BAL'], 1)
  return(spei_12)
  }
  
calc_spei("us-aud.csv")

for_spei <- completeFun(site_based, "tmed")

#PET functions in SPEI won't take a list of latitudes for calculating PET -> we'll have to do "split, apply, combine"
calc_thorn <- function(x){
  latitude <- (x$Latitude[1])
  PET_thorn <- thornthwaite(x$tmed, latitude, na.rm=TRUE)
  return(PET_thorn)
}
#split by site
split <- (split(for_spei, for_spei$site))
#lapply thornthwaite; note -> SPEI labels for "Jan", "Feb", etc. are incorrect and assueme we started in Jan, when we started in May
t1 <- lapply(split, calc_thorn)
#Recombine - seems to have worked properly (PET lines up)
for_spei$PET_thorn <- do.call(rbind, t1)

#Now to do the same for hargreaves--------------
calc_hargreaves <- function(x){
  latitude <- (x$Latitude[1])
  PET_harg <- hargreaves(x$tmin, x$tmax, lat=latitude, na.rm=TRUE)
  return(PET_harg)
}
#split
split2 <- split(for_spei, for_spei$site)
#apply
t2 <- lapply(split2, calc_hargreaves)
#combine
for_spei$PET_harg <- do.call(rbind, t2)
#How well do hargreaves and thornthwaite correlate? 
cor(for_spei$PET_harg, for_spei$PET_thorn)

write.csv(for_spei, "F:/Upscaling_Project/Site_based_RF/for_spei.csv")

#Calculate SPEI------------------
for_spei <- read.csv("D:/Upscaling_Project/Site_based_RF/for_spei.csv")

head(for_spei)
str(for_spei)

#Need to split apply combine by site this time - each site has different timeseries for which SPEI will need to be calculated
for_spei$BAL <- for_spei$precip - for_spei$PET_thorn 
#The longer the SPEI timescale, the more data that's going to get cut off...
str(for_spei)
split_for_spei<- split(for_spei, for_spei$site)
str(split_for_spei)



plot(bal_spei12)

for_spei$SPEI_1 <- calc_1_spei(for_spei, 2008,5)
for_spei$SPEI_3
for_spei$SPEI_6
for_spei$SPEI_9

calc_1_spei <- function(x,y,z){
  SPEI_1 <- spei(ts(x[,'BAL'], freq=12, start=c(y,z)), 1)
  return(SPEI_1) 
}

calc_3_spei <- function(x,y,z){
  SPEI_3 <- spei(ts(x[,'BAL'], freq=12, start=c(y,z)), 3)
  return(SPEI_3)
}

calc_6_spei <- function(x,y,z){
  SPEI_6 <- spei(ts(x[,'BAL'], freq=12, start=c(y,z)), 6)
  return(SPEI_6)
  
}

calc_9_spei <- function(x,y,z){
  SPEI_9 <- spei(ts(x[,'BAL'], freq=12, start=c(y,z)), 9)
  
}
spei(ts(for_spei[,'BAL'], freq=12, start=c(2008,5)), 12)
spei(ts(for_spei[,'BAL'], freq=12, start=c(2008,5)), 12)
spei(ts(for_spei[,'BAL'], freq=12, start=c(2008,5)), 12)
spei(ts(for_spei[,'BAL'], freq=12, start=c(2008,5)), 12)
spei(ts(for_spei[,'BAL'], freq=12, start=c(2008,5)), 12)

