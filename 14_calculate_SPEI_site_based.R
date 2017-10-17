#File to calculate SPEI on site basis
#Current goal: SPEI (back from September) - 1, 3, 6, 9, and 12-month SPEI for all sites/all months. 
#Re-run RF models then
library(SPEI)
library(dplyr)
library(plyr)

site_based <- read.csv("F:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_10_13_2017_less_NA.csv")
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
for_spei <- read.csv("F:/Upscaling_Project/Site_based_RF/for_spei.csv")

head(for_spei)

for_spei$BAl <- for_spei$precip - for_spei$PET_thorn 
