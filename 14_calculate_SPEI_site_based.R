#File to calculate SPEI on site basis
#Current goal: SPEI (back from September) - 1, 3, 6, 9, and 12-month SPEI for all sites/all months. 
#Re-run RF models then
library(SPEI)
library(dplyr)
library(plyr)
library(raster)

site_based <- read.csv("D:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_10_13_2017_less_NA.csv")
str(site_based)
#first: compare thornthwaite and hargreaves methods of calculating SPEI 
#For thornthwaite -> will need to calculate mean monthly temp (or from MODIS_LST?)
#site_based$tmed <- ((site_based$tmax + site_based$tmin)/2)
#remove rows with NA for daymet data 

completeFun <- function(data, desiredCols){
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


str(site_based)

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

