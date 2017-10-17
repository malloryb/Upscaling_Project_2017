#Code to deal with rasters 
#If you are looking for the file to makee
library(tiff)
library(raster)
library(rgdal)
library(lubridate)
library(MODIS)
library(ncdf4)
library(MODIStsp)
#For LST-----------------------------------------------
#Set working directory
setwd("C:/Users/Mallory/Documents/MRT/")
#List of .TIF files to process
filenames <- list.files("Params/", pattern= "*tif$")
filenames_Jan <- list.files("Params/", pattern="*A200100.*tif$")


rast_test <- raster("C:/Users/Mallory/Documents/MRT/Params/MOD11A2.A2001017.LST_Day_1km.tif")
plot(rast_test)
extent(rast_test)
res(rast_test)


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

#Function to process MODIS LST 1km Daytime rasters
#Function must (could do lapply): 
#Read in TIFF file 
#Extract Date from TIFF file
#Rescale raster
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

#For NDVI

#List of .TIF files to process
filenames_NDVI <- list.files("NDVIParam/", pattern= "*tif$")
filenames_Jan_NDVI <- list.files("NDVIParam/", pattern="*A200100.*tif$")


scale_raster_NDVI <- function(x) {
  filename <- paste(x)
  print(filename)
  rast <- raster(paste("NDVIParam", x, sep="/"))
  rast_date <- extractDate(x, pos1=10, pos2=16, asDate=TRUE)
  rast_date <- rast_date[[1]][1]
  date <- (as.character(rast_date))
  rast <- setMinMax(rast)
  rast[rast==-3000]<-NA
  rast <- ((rast * 0.0001))
  rast <- setNames(rast, date)
  return(rast)
}


s22 <-lapply(filenames_NDVI, scale_raster_NDVI)
s32 <- lapply(filenames_Jan_NDVI, scale_raster_NDVI)

plot(s32[[1]])

#Overlay rasters by month
#TAKES FOREVER
Jan_mean <- overlay(s22[[1]], s22[[2]], fun=mean_na)
Feb_mean <- overlay(s22[[3]], s22[[4]], fun=mean_na)
Mar_mean <- overlay(s22[[5]], s22[[6]], fun=mean_na)
Apr_mean <- overlay(s22[[7]], s22[[8]], fun=mean_na)
May_mean <- overlay(s22[[9]], s22[[10]], fun=mean_na)
Jun_mean <- overlay(s22[[11]], s22[[12]], fun=mean_na)
Jul_mean <- overlay(s22[[13]], s22[[14]], fun=mean_na)
Aug_mean <- overlay(s22[[15]], s22[[16]], fun=mean_na)
Sep_mean <- overlay(s22[[17]], s22[[18]], fun=mean_na)
Oct_mean <- overlay(s22[[19]],  fun=mean_na)
Nov_mean <- overlay(s22[[20]], s22[[21]], fun=mean_na)
Dec_mean <- overlay(s22[[22]], s22[[23]], fun=mean_na)

stkNDVI <- stack(Jan_mean,Feb_mean,Mar_mean,Apr_mean, May_mean, Jun_mean, Jul_mean, Aug_mean,Sep_mean, Oct_mean, Nov_mean, Dec_mean)
names(stkNDVI) <- c('Jan','Feb','Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')


writeRaster(stkNDVI, file="C:/Users/Mallory/Documents/MRT/Monthly_NDVI_2001.nc", overwrite=TRUE) 
memory.limit(size = 150000)
memory.limit()
gc()


#Read rasters from Guillermo
Precip <-raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Temp <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
EVI <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif")

plot(Precip[[1]])
setMinMax(Precip)
Precip[Precip==-9999] <- NA 
plot(Precip[[1]])
Precip

Jan_2001_precip <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
       band = 13)

Jan_2001_precip[Jan_2001_precip==-9999] <- NA 
plot(Jan_2001_precip)

Jan_2001_temp <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 13)

Jan_2001_temp[Jan_2001_temp==-9999] <- NA 
plot(Jan_2001_temp)

Jan1 <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=1)
Jan2 <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=2)

Jan_2001_EVI <- overlay(Jan1, Jan2, fun=mean_na)

Jan_2001_EVI[Jan_2001_EVI<0] <- NA 
Jan_2001_EVI <- (Jan_2001_EVI * 0.0001)
plot(Jan_2001_EVI)
Jan_2001_EVI




