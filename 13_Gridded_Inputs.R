#Code to deal with rasters 
#If you are looking for the file to makee
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

#Function to process MODIS EVI rasters
#Function must (could do lapply): 
#Read in TIFF file 
#Extract Date from TIFF file
#Rescale raster
#For NDVI
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

#Read rasters from Guillermo--------------------------------
Precip <-raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Tmax <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
Tmin <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif")
EVI <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif")


#Precip
Jan_2001_precip <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
       band = 13)

Jan_2001_precip[Jan_2001_precip==-9999] <- NA 
plot(Jan_2001_precip)


#Tmax
Jan_2001_tmax <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 13)

Jan_2001_tmax[Jan_2001_tmax==-9999] <- NA 
plot(Jan_2001_tmax)

#Tmin
Jan_2001_tmin <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 13)

Jan_2001_tmin[Jan_2001_tmin==-9999] <- NA 


#EVI
Jan1 <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=1)
Jan2 <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=2)

#OK this seriously took over an hour
Jan_2001_EVI <- overlay(Jan1, Jan2, fun=mean_na)

#Remote NAs and rescale (EVI scale factor)
#I think scale value is actually -3000...tray that next time

Jan_2001_EVI[Jan_2001_EVI=-3000] <-NA
#Jan_2001_EVI[Jan_2001_EVI<0] <- NA 
Jan_2001_EVI <- (Jan_2001_EVI * 0.0001)
Jan_2001_EVI
plot(Jan_2001_EVI)

#Trying to algin rasters so they can be stacked
Upscext <- extent(Jan_2001_EVI)

#Resample takes a bit but not too long...
Jan_2001_tminresample <- resample(Jan_2001_tmin, Jan_2001_EVI, method="bilinear")
Jan_2001_tmaxresample <- resample(Jan_2001_tmax, Jan_2001_EVI, method="bilinear")
Jan_2001_Precipesample <- resample(Jan_2001_precip, Jan_2001_EVI, method="bilinear")

#Create blank raster for month with value of "1"
month = raster (ext=Upscext, res=0.002081004)
values(month) <-1
plot(month)

#Stack
Jan_2001 <- stack(Jan_2001_tminresample, Jan_2001_tmaxresample, Jan_2001_Precipesample, Jan_2001_EVI, month)
#Rename rasters in raster stack
#Calling EVI "NDVI" for now. Then: tmax, tmin, and month.

names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

writeRaster(Jan_2001, file="F:/Upscaling_Project/Gridded_Inputs/Jan_2001_.nc")

#Actually I think we want to write it as a .tif
#to write: 
writeRaster(Jan_2001, filename="F:/Upscaling_Project/Gridded_inputs/Jan_2001.tif")
#to read: Jan_2001 <- stack("F:/Upscaling_Project/Gridded_inputs/Jan_2001.tif")
