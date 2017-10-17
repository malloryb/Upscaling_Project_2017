#Code to deal with rasters 
#If you are looking for the file to makee TIFs from raw MODIS files see 9_25 tifs file (not in this Project)
library(tiff)
library(raster)
library(rgdal)
library(lubridate)
library(MODIS)
library(ncdf4)
library(MODIStsp)
#MeanIgnoringZeroes -> user-defined function to get mean without zeros

meanIgnoringZeroes <- function(x) {
  mean(x[x!=0],na.rm=T)
}

meanIgnoringLSTZeros <- function(x) {
  mean(x[x!=-273.15],na.rm=T)
}

mean_na <- function(x) {
  mean(x,na.rm=T)
}

scale_raster <- function(x) {
  filename <- paste(x)
  print(filename)
  rast <- raster(paste("Params", x, sep="/"))
  rast_date <- extractDate(x, pos1=10, pos2=16, asDate=TRUE)
  rast_date <- rast_date[[1]][1]
  date <- (as.character(rast_date))
  rast <- setMinMax(rast)
  rast[rast==0]<-NA
  rast <- ((rast * 0.02) - 273.15)
  rast <- setNames(rast, date)
  return(rast)
}

s2 <- lapply(filenames, scale_raster)
stack(s2)

#Overlay rasters by month
#TAKES FOREVER - 12:54pm to 
Jan_mean <- overlay(s2[[1]], s2[[2]], s2[[3]], s2[[4]], fun=mean_na)
Feb_mean <- overlay(s2[[5]], s2[[6]], s2[[7]], s2[[8]], fun=mean_na)
Mar_mean <- overlay(s2[[9]], s2[[10]], s2[[11]], s2[[12]], fun=mean_na)
Apr_mean <- overlay(s2[[13]], s2[[14]], s2[[15]], fun=mean_na)
May_mean <- overlay(s2[[16]], s2[[17]], s2[[18]], s2[[19]], fun=mean_na)
Jun_mean <- overlay(s2[[20]], s2[[21]], s2[[22]], fun=mean_na)
Jul_mean <- overlay(s2[[23]], s2[[24]], s2[[25]], s2[[26]], fun=mean_na)
Aug_mean <- overlay(s2[[27]], s2[[28]], s2[[29]], s2[[30]], fun=mean_na)
Sep_mean <- overlay(s2[[31]], s2[[32]], s2[[33]], s2[[34]], fun=mean_na)
Oct_mean <- overlay(s2[[35]], s2[[36]], s2[[37]], fun=mean_na)
Nov_mean <- overlay(s2[[38]], s2[[39]], s2[[40]], s2[[41]], fun=mean_na)
Dec_mean <- overlay(s2[[42]], s2[[43]], s2[[44]], s2[[45]], fun=mean_na)

stk <- stack(Jan_mean,Feb_mean,Mar_mean,Apr_mean, May_mean, Jun_mean, Jul_mean, Aug_mean,Sep_mean, Oct_mean, Nov_mean, Dec_mean)
names(stk) <- c('Jan','Feb','Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')


writeRaster(stk, file="C:/Users/Mallory/Documents/MRT/Monthly_Daytime_LST_2001.nc") 

s <- stack(Jan_mean, Apr_mean)

#Plot
plot(LST1)
#Set all 0 values as NA
LST1[LST1==0]<-NA
#Rescale properly; https://gis.stackexchange.com/questions/72524/how-do-i-convert-the-lst-values-on-the-modis-lst-image-to-degree-celsius
LST1 <- ((LST1 * 0.02) - 273.15)
#Plot
plot(LST1)

#Read reasters from Guillermo
#MODIS files each have 23 bands corresponding to each 16-day value
#Daymet files have one band for each month from 2000-2016 -> 204 bands in file in total.

Precip <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Tmax <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
EVI <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif")
str(EVI)
str(Precip)
setMinMax(Precip)
Precip[Precip==-9999] <- NA 
plot(Precip[[1]])
Precip


str(Tmax)
setMinMax(Tmax)
Tmax[Tmax==-9999] <- NA 
plot(Tmax[[1]])
Tmax

plot(Precip)
plot(Tmax)


scale_raster <- function(x) {
  filename <- paste(x)
  print(filename)
  #rast <- raster(paste("NDVIParam", x, sep="/"))
  ##rast_date <- extractDate(x, pos1=10, pos2=16, asDate=TRUE)
  rast_date <- rast_date[[1]][1]
  date <- (as.character(rast_date))
  rast <- setMinMax(rast)
  rast[rast==-3000]<-NA
  rast <- ((rast * 0.0001))
  rast <- setNames(rast, date)
  return(rast)
}



plot(Precip[[1]])
plot(Temp[[1]])



