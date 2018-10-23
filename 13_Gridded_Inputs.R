#Code to deal with rasters 
#If you are looking for the file to makee
library(tiff)
library(raster)
library(rgdal)
library(lubridate)
library(MODIS)
library(ncdf4)
library(MODIStsp)

getwd()
setwd("/mnt/DATA/users/mbarnes/")
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

writeRaster(stkNDVI, file="C:/Users/Mallory/Documents/MRT/Monthly_NDVI_2002.nc", overwrite=TRUE) 
memory.limit(size = 150000)
memory.limit()
gc()

#Read rasters from Guillermo--------------------------------
<<<<<<< HEAD
Precip <-raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Tmax <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
Tmin <- raster("F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif")
EVI <- raster("F:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2002_EVI.tif")
=======
Precip <-raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Tmax <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")
Tmin <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif")
EVI <- raster("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif")
>>>>>>> d15b80bb988155f969cb2c28b085112ad24868ac

#Jan_2002---------------------------------------------------------------
#Precip
<<<<<<< HEAD
Jan_2002_precip <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
=======
Jan_2001_precip <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
>>>>>>> d15b80bb988155f969cb2c28b085112ad24868ac
       band = 13)

Jan_2001_precip[Jan_2001_precip==-9999] <- NA 
plot(Jan_2001_precip)


#Tmax
Jan_2001_tmax <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 13)

Jan_2001_tmax[Jan_2001_tmax==-9999] <- NA 
plot(Jan_2001_tmax)

#Tmin
Jan_2001_tmin <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif", 
                        band = 13)

Jan_2001_tmin[Jan_2001_tmin==-9999] <- NA 


#EVI
Jan1 <- raster("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=1)
Jan2 <- raster("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=2)

#OK this seriously took over an hour on ARS computer, seems to be quicker on my laptop
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
#writeRaster(Jan_2001, file="F:/Upscaling_Project/Gridded_Inputs/Jan_2001_.nc")
#Actually I think we want to write it as a .tif
#to write: 
writeRaster(Jan_2001.2, filename="D:/Upscaling_Project/Gridded_inputs/Jan_2001.tif")
Jan_2001 <- stack("D:/Upscaling_Project/Gridded_inputs/Jan_2001.tif")


#June_2001---------------------------------------------------------------
#Precip
Jun_2001_precip <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
                          band = 18)

Jun_2001_precip[Jun_2001_precip==-9999] <- NA 
plot(Jun_2001_precip)

#Tmax
Jun_2001_tmax <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 18)

Jun_2001_tmax[Jun_2001_tmax==-9999] <- NA 
plot(Jun_2001_tmax)

#Tmin
Jun_2001_tmin <- raster("D:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif", 
                        band = 18)

Jun_2001_tmin[Jun_2001_tmin==-9999] <- NA 
plot(Jun_2001_tmin)

#EVI
Jun1 <- raster("D:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=11)
Jun2 <- raster("D:/Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif", band=12)

#OK this seriously took over an hour - less time on personal laptop for whatever reason
Jun_2001_EVI <- overlay(Jun1, Jun2, fun=mean_na)

#Remote NAs and rescale (EVI scale factor)
#I think scale value is actually -3000...tray that next time

Jun_2001_EVI[Jun_2001_EVI==-3000] <-NA
#Jun_2001_EVI[Jun_2001_EVI<0] <- NA 
Jun_2001_EVI <- (Jun_2001_EVI * 0.0001)
Jun_2001_EVI
plot(Jun_2001_EVI)

#Trying to algin rasters so they can be stacked
Upscext <- extent(Jun_2001_EVI)

#Resample takes a bit but not too long...
Jun_2001_tminresample <- resample(Jun_2001_tmin, Jun_2001_EVI, method="bilinear")
Jun_2001_tmaxresample <- resample(Jun_2001_tmax, Jun_2001_EVI, method="bilinear")
Jun_2001_Precipesample <- resample(Jun_2001_precip, Jun_2001_EVI, method="bilinear")

#Create blank raster for month with value of "1"
month = raster (ext=Upscext, res=0.002081004)
values(month) <-6
plot(month)

#Stack
Jun_2001 <- stack(Jun_2001_tminresample, Jun_2001_tmaxresample, Jun_2001_Precipesample, Jun_2001_EVI, month)
#Rename rasters in raster stack
#Calling EVI "NDVI" for now. Then: tmax, tmin, and month.
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
writeRaster(Jun_2001, filename="D:/Upscaling_Project/Gridded_inputs/Jun_2001.tif", overwrite=TRUE)


#Batch processing --------------

#Apply function to scale NDVI properly to all layers in raster stack
#Then overlay with mean.na function

EVI2001 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2001_EVI.tif")


scaleVI <- function(x) {
  x[x==-3000]<-NA
  y<- ((x* 0.0001))
  return(y)
}

ndvi_scaled <- calc(EVI2001, scaleVI)
plot(ndvi_scaled)
#replaced ndvi_scaled w/s22 before this 

Jan_2001_EVI <- overlay(ndvi_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2001_EVI, filename="Upscaling_Project/Gridded_Inputs/EVI/Jan_2001_EVI.tif")
Feb_2001_EVI <- overlay(ndvi_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2001_EVI, filename="My Drive/Upscaling_Project/Feb_2001_EVI.tif")
Mar_2001_EVI <- overlay(ndvi_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2001_EVI, filename="My Drive/Upscaling_Project/Mar_2001_EVI.tif")
Apr_2001_EVI <- overlay(ndvi_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2001_EVI, filename="My Drive/Upscaling_Project/Apr_2001_EVI.tif")
May_2001_EVI <- overlay(ndvi_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2001_EVI, filename="My Drive/Upscaling_Project/May_2001_EVI.tif")
Jun_2001_EVI <- overlay(ndvi_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2001_EVI, filename="My Drive/Upscaling_Project/Jun_2001_EVI.tif")
Jul_2001_EVI <- overlay(ndvi_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2001_EVI, filename="My Drive/Upscaling_Project/Jul_2001_EVI.tif")
Aug_2001_EVI <- overlay(ndvi_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2001_EVI, filename="My Drive/Upscaling_Project/Aug_2001_EVI.tif")
Sep_2001_EVI <- overlay(ndvi_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2001_EVI, filename="My Drive/Upscaling_Project/Sep_2001_EVI.tif")
Oct_2001_EVI <- overlay(ndvi_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2001_EVI, filename="My Drive/Upscaling_Project/Oct_2001_EVI.tif")
Nov_2001_EVI <- overlay(ndvi_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2001_EVI, filename="My Drive/Upscaling_Project/Nov_2001_EVI.tif")
Dec_2001_EVI <- overlay(ndvi_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2001_EVI, filename="My Drive/Upscaling_Project/Dec_2001_EVI.tif")

Apr_2001_EVI <- raster("Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2001_EVI.tif")


#Precip
Apr_2001_precip <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif", 
                          band = 15)

Apr_2001_precip[Apr_2001_precip==-9999] <- NA 
plot(Feb_2001_precip)

#Tmax
Apr_2001_tmax <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif", 
                        band = 15)

Apr_2001_tmax[Apr_2001_tmax==-9999] <- NA 
plot(Feb_2001_tmax)

#Tmin
Apr_2001_tmin <- raster("Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif", 
                        band = 15)

Apr_2001_tmin[Apr_2001_tmin==-9999] <- NA 
plot(Feb_2001_tmin)

#Trying to algin rasters so they can be stacked
Upscext <- extent(Apr_2001_EVI)

#Resample takes a bit but not too long...
Apr_2001_tminresample <- resample(Apr_2001_tmin, Apr_2001_EVI, method="bilinear")
Apr_2001_tmaxresample <- resample(Apr_2001_tmax, Apr_2001_EVI, method="bilinear")
Apr_2001_Precipesample <- resample(Apr_2001_precip, Apr_2001_EVI, method="bilinear")

#Create blank raster for month with value of "1" for Jan, "2" for Feb, etc. 
month = raster (ext=Upscext, res=0.002081004)
values(month) <-4
plot(month)

#Stack
Apr_2001 <- stack(Apr_2001_tminresample, Apr_2001_tmaxresample, Apr_2001_Precipesample, Apr_2001_EVI, month)
#Rename rasters in raster stack
#Calling EVI "NDVI" for now. Then: tmax, tmin, and month.
names(Apr_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
Apr_2001

writeRaster(Apr_2001, filename="Upscaling_Project/Gridded_inputs/Apr_2001.tif")

plot(raster("Upscaling_Project/Gridded_Inputs/Apr_2001.tif"))
#read RF5 random forest model
RF5 <- readRDS("Upscaling_Project/Upscaling_Project_2017/RF5_10_18.rds")
Apr_2001_GPP <- predict(Apr_2001, RF5, ext=sw)
plot(Apr_2001_GPP, main="April 2001 upscaled GPP", zlim=c(0,7))
writeRaster(Apr_2001_GPP, filename="")


<<<<<<< HEAD
#To re-crate all EVI for 1 year: 

for year in 2003:2016){
  
}

#Apply function to scale NDVI properly to all layers in raster stack
#Then overlay with mean.na function
rasterOptions()
rasterOptions(maxmemory= 1e+09, chunksize=2e+08)
gc()

EVI2003 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2003_EVI.tif")
EVI2004 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2004_EVI.tif")
EVI2005 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2005_EVI.tif")
EVI2006 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2006_EVI.tif")
EVI2007 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2007_EVI.tif")
EVI2008 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2008_EVI.tif")
EVI2009 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2009_EVI.tif")
EVI2010 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2010_EVI.tif")
EVI2011 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2011_EVI.tif")
EVI2012 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2012_EVI.tif")
EVI2013 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2013_EVI.tif")
EVI2014 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2014_EVI.tif")
EVI2015 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2015_EVI.tif")
EVI2016 <- stack("Upscaling_Project/Gridded_Inputs/EVI/upscalingArea_2016_EVI.tif")

rast_list <- list(ls(pattern='EVI20*'))

scaleVI <- function(x) {
  x[x==-3000]<-NA
  y<- ((x* 0.0001))
  return(y)
}

ndvi2003_scaled <- calc(EVI2003, scaleVI)
ndvi2004_scaled <- calc(EVI2004, scaleVI)
ndvi2005_scaled <- calc(EVI2005, scaleVI)
ndvi2006_scaled <- calc(EVI2006, scaleVI)
ndvi2007_scaled <- calc(EVI2007, scaleVI)
ndvi2008_scaled <- calc(EVI2008, scaleVI)
ndvi2009_scaled <- calc(EVI2009, scaleVI)
ndvi2010_scaled <- calc(EVI2010, scaleVI)
ndvi2011_scaled <- calc(EVI2011, scaleVI)
ndvi2012_scaled <- calc(EVI2012, scaleVI)
ndvi2013_scaled <- calc(EVI2013, scaleVI)
ndvi2014_scaled <- calc(EVI2014, scaleVI)
ndvi2015_scaled <- calc(EVI2015, scaleVI)
ndvi2016_scaled <- calc(EVI2016, scaleVI)


plot(ndvi_scaled)
#For 2003
Jan_2003_EVI <- overlay(ndvi2003_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2003_EVI.tif")
Feb_2003_EVI <- overlay(ndvi2003_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2003_EVI.tif")
Mar_2003_EVI <- overlay(ndvi2003_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2003_EVI.tif")
Apr_2003_EVI <- overlay(ndvi2003_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2003_EVI.tif")
May_2003_EVI <- overlay(ndvi2003_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2003_EVI.tif")
Jun_2003_EVI <- overlay(ndvi2003_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2003_EVI.tif")
Jul_2003_EVI <- overlay(ndvi2003_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2003_EVI.tif")
Aug_2003_EVI <- overlay(ndvi2003_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2003_EVI.tif")
Sep_2003_EVI <- overlay(ndvi2003_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2003_EVI.tif")
Oct_2003_EVI <- overlay(ndvi2003_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2003_EVI.tif")
Nov_2003_EVI <- overlay(ndvi2003_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2003_EVI.tif")
Dec_2003_EVI <- overlay(ndvi2003_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2003_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2003_EVI.tif")

#For 2004
Jan_2004_EVI <- overlay(ndvi2004_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2004_EVI.tif")
Feb_2004_EVI <- overlay(ndvi2004_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2004_EVI.tif")
Mar_2004_EVI <- overlay(ndvi2004_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2004_EVI.tif")
Apr_2004_EVI <- overlay(ndvi2004_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2004_EVI.tif")
May_2004_EVI <- overlay(ndvi2004_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2004_EVI.tif")
Jun_2004_EVI <- overlay(ndvi2004_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2004_EVI.tif")
Jul_2004_EVI <- overlay(ndvi2004_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2004_EVI.tif")
Aug_2004_EVI <- overlay(ndvi2004_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2004_EVI.tif")
Sep_2004_EVI <- overlay(ndvi2004_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2004_EVI.tif")
Oct_2004_EVI <- overlay(ndvi2004_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2004_EVI.tif")
Nov_2004_EVI <- overlay(ndvi2004_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2004_EVI.tif")
Dec_2004_EVI <- overlay(ndvi2004_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2004_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2004_EVI.tif")
#For 2005
Jan_2005_EVI <- overlay(ndvi2005_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2005_EVI.tif")
Feb_2005_EVI <- overlay(ndvi2005_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2005_EVI.tif")
Mar_2005_EVI <- overlay(ndvi2005_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2005_EVI.tif")
Apr_2005_EVI <- overlay(ndvi2005_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2005_EVI.tif")
May_2005_EVI <- overlay(ndvi2005_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2005_EVI.tif")
Jun_2005_EVI <- overlay(ndvi2005_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2005_EVI.tif")
Jul_2005_EVI <- overlay(ndvi2005_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2005_EVI.tif")
Aug_2005_EVI <- overlay(ndvi2005_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2005_EVI.tif")
Sep_2005_EVI <- overlay(ndvi2005_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2005_EVI.tif")
Oct_2005_EVI <- overlay(ndvi2005_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2005_EVI.tif")
Nov_2005_EVI <- overlay(ndvi2005_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2005_EVI.tif")
Dec_2005_EVI <- overlay(ndvi2005_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2005_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2005_EVI.tif")
#For 2006
Jan_2006_EVI <- overlay(ndvi2006_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2006_EVI.tif")
Feb_2006_EVI <- overlay(ndvi2006_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2006_EVI.tif")
Mar_2006_EVI <- overlay(ndvi2006_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2006_EVI.tif")
Apr_2006_EVI <- overlay(ndvi2006_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2006_EVI.tif")
May_2006_EVI <- overlay(ndvi2006_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2006_EVI.tif")
Jun_2006_EVI <- overlay(ndvi2006_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2006_EVI.tif")
Jul_2006_EVI <- overlay(ndvi2006_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2006_EVI.tif")
Aug_2006_EVI <- overlay(ndvi2006_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2006_EVI, filename="Upscaling_Project#2) create MAP and MAT rasters from Daymet data and put in gridded input datasets
#3) Add Water/Gridded_Inputs/Monthly_EVI/Aug_2006_EVI.tif")
Sep_2006_EVI <- overlay(ndvi2006_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2006_EVI.tif")
Oct_2006_EVI <- overlay(ndvi2006_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2006_EVI.tif")
Nov_2006_EVI <- overlay(ndvi2006_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2006_EVI.tif")
Dec_2006_EVI <- overlay(ndvi2006_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2006_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2006_EVI.tif")
#For 2007
Jan_2007_EVI <- overlay(ndvi2007_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2007_EVI.tif")
Feb_2007_EVI <- overlay(ndvi2007_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2007_EVI.tif")
Mar_2007_EVI <- overlay(ndvi2007_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2007_EVI.tif")
Apr_2007_EVI <- overlay(ndvi2007_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2007_EVI.tif")
May_2007_EVI <- overlay(ndvi2007_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2007_EVI.tif")
Jun_2007_EVI <- overlay(ndvi2007_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2007_EVI.tif")
Jul_2007_EVI <- overlay(ndvi2007_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2007_EVI.tif")
Aug_2007_EVI <- overlay(ndvi2007_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2007_EVI.tif")
Sep_2007_EVI <- overlay(ndvi2007_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2007_EVI.tif")
Oct_2007_EVI <- overlay(ndvi2007_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2007_EVI.tif")
Nov_2007_EVI <- overlay(ndvi2007_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2007_EVI.tif")
Dec_2007_EVI <- overlay(ndvi2007_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2007_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2007_EVI.tif")
#For 2008
Jan_2008_EVI <- overlay(ndvi2008_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2008_EVI.tif")
Feb_2008_EVI <- overlay(ndvi2008_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2008_EVI.tif")
Mar_2008_EVI <- overlay(ndvi2008_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2008_EVI.tif")
Apr_2008_EVI <- overlay(ndvi2008_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2008_EVI.tif")
May_2008_EVI <- overlay(ndvi2008_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2008_EVI.tif")
Jun_2008_EVI <- overlay(ndvi2008_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2008_EVI.tif")
Jul_2008_EVI <- overlay(ndvi2008_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2008_EVI.tif")
Aug_2008_EVI <- overlay(ndvi2008_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2008_EVI.tif")
Sep_2008_EVI <- overlay(ndvi2008_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2008_EVI.tif")
Oct_2008_EVI <- overlay(ndvi2008_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2008_EVI.tif")
Nov_2008_EVI <- overlay(ndvi2008_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2008_EVI.tif")
Dec_2008_EVI <- overlay(ndvi2008_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2008_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2008_EVI.tif")
#For 2009
Jan_2009_EVI <- overlay(ndvi2009_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2009_EVI.tif")
Feb_2009_EVI <- overlay(ndvi2009_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2009_EVI.tif")
Mar_2009_EVI <- overlay(ndvi2009_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2009_EVI.tif")
Apr_2009_EVI <- overlay(ndvi2009_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2009_EVI.tif")
May_2009_EVI <- overlay(ndvi2009_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2009_EVI.tif")
Jun_2009_EVI <- overlay(ndvi2009_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2009_EVI.tif")
Jul_2009_EVI <- overlay(ndvi2009_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2009_EVI.tif")
Aug_2009_EVI <- overlay(ndvi2009_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2009_EVI.tif")
Sep_2009_EVI <- overlay(ndvi2009_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2009_EVI.tif")
Oct_2009_EVI <- overlay(ndvi2009_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2009_EVI.tif")
Nov_2009_EVI <- overlay(ndvi2009_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2009_EVI.tif")
Dec_2009_EVI <- overlay(ndvi2009_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2009_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2009_EVI.tif")
#For 2010

Jan_2010_EVI <- overlay(ndvi2010_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2010_EVI.tif")
Feb_2010_EVI <- overlay(ndvi2010_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2010_EVI.tif")
Mar_2010_EVI <- overlay(ndvi2010_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2010_EVI.tif")
Apr_2010_EVI <- overlay(ndvi2010_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2010_EVI.tif")
May_2010_EVI <- overlay(ndvi2010_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2010_EVI.tif")
Jun_2010_EVI <- overlay(ndvi2010_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2010_EVI.tif")
Jul_2010_EVI <- overlay(ndvi2010_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2010_EVI.tif")
Aug_2010_EVI <- overlay(ndvi2010_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2010_EVI.tif")
Sep_2010_EVI <- overlay(ndvi2010_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2010_EVI.tif")
Oct_2010_EVI <- overlay(ndvi2010_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2010_EVI.tif")
Nov_2010_EVI <- overlay(ndvi2010_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2010_EVI.tif")
Dec_2010_EVI <- overlay(ndvi2010_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2010_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2010_EVI.tif")
#For 2011
Jan_2011_EVI <- overlay(ndvi2011_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2011_EVI.tif")
Feb_2011_EVI <- overlay(ndvi2011_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2011_EVI.tif")
Mar_2011_EVI <- overlay(ndvi2011_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2011_EVI.tif")
Apr_2011_EVI <- overlay(ndvi2011_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2011_EVI.tif")
May_2011_EVI <- overlay(ndvi2011_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2011_EVI.tif")
Jun_2011_EVI <- overlay(ndvi2011_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2011_EVI.tif")
Jul_2011_EVI <- overlay(ndvi2011_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2011_EVI.tif")
Aug_2011_EVI <- overlay(ndvi2011_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2011_EVI.tif")
Sep_2011_EVI <- overlay(ndvi2011_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2011_EVI.tif")
Oct_2011_EVI <- overlay(ndvi2011_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2011_EVI.tif")
Nov_2011_EVI <- overlay(ndvi2011_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2011_EVI.tif")
Dec_2011_EVI <- overlay(ndvi2011_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2011_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2011_EVI.tif")
#For 2012
Jan_2012_EVI <- overlay(ndvi2012_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2012_EVI.tif")
Feb_2012_EVI <- overlay(ndvi2012_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2012_EVI.tif")
Mar_2012_EVI <- overlay(ndvi2012_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2012_EVI.tif")
Apr_2012_EVI <- overlay(ndvi2012_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2012_EVI.tif")
May_2012_EVI <- overlay(ndvi2012_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2012_EVI.tif")
Jun_2012_EVI <- overlay(ndvi2012_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2012_EVI.tif")
Jul_2012_EVI <- overlay(ndvi2012_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2012_EVI.tif")
Aug_2012_EVI <- overlay(ndvi2012_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2012_EVI.tif")
Sep_2012_EVI <- overlay(ndvi2012_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2012_EVI.tif")
Oct_2012_EVI <- overlay(ndvi2012_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2012_EVI.tif")
Nov_2012_EVI <- overlay(ndvi2012_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2012_EVI.tif")
Dec_2012_EVI <- overlay(ndvi2012_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2012_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2012_EVI.tif")
#For 2013
Jan_2013_EVI <- overlay(ndvi2013_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2013_EVI.tif")
Feb_2013_EVI <- overlay(ndvi2013_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2013_EVI.tif")
Mar_2013_EVI <- overlay(ndvi2013_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2013_EVI.tif")
Apr_2013_EVI <- overlay(ndvi2013_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2013_EVI.tif")
May_2013_EVI <- overlay(ndvi2013_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2013_EVI.tif")
Jun_2013_EVI <- overlay(ndvi2013_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2013_EVI.tif")
Jul_2013_EVI <- overlay(ndvi2013_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2013_EVI.tif")
Aug_2013_EVI <- overlay(ndvi2013_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2013_EVI.tif")
Sep_2013_EVI <- overlay(ndvi2013_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2013_EVI.tif")
Oct_2013_EVI <- overlay(ndvi2013_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2013_EVI.tif")
Nov_2013_EVI <- overlay(ndvi2013_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2013_EVI.tif")
Dec_2013_EVI <- overlay(ndvi2013_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2013_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2013_EVI.tif")
#For 2014
Jan_2014_EVI <- overlay(ndvi2014_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2014_EVI.tif")
Feb_2014_EVI <- overlay(ndvi2014_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2014_EVI.tif")
Mar_2014_EVI <- overlay(ndvi2014_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2014_EVI.tif")
Apr_2014_EVI <- overlay(ndvi2014_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2014_EVI.tif")
May_2014_EVI <- overlay(ndvi2014_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2014_EVI.tif")
Jun_2014_EVI <- overlay(ndvi2014_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2014_EVI.tif")
Jul_2014_EVI <- overlay(ndvi2014_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2014_EVI.tif")
Aug_2014_EVI <- overlay(ndvi2014_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2014_EVI.tif")
Sep_2014_EVI <- overlay(ndvi2014_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2014_EVI.tif")
Oct_2014_EVI <- overlay(ndvi2014_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2014_EVI.tif")
Nov_2014_EVI <- overlay(ndvi2014_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2014_EVI.tif")
Dec_2014_EVI <- overlay(ndvi2014_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2014_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2014_EVI.tif")
#For 2015
Jan_2015_EVI <- overlay(ndvi2015_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2015_EVI.tif")
Feb_2015_EVI <- overlay(ndvi2015_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2015_EVI.tif")
Mar_2015_EVI <- overlay(ndvi2015_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2015_EVI.tif")
Apr_2015_EVI <- overlay(ndvi2015_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2015_EVI.tif")
May_2015_EVI <- overlay(ndvi2015_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2015_EVI.tif")
Jun_2015_EVI <- overlay(ndvi2015_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2015_EVI.tif")
Jul_2015_EVI <- overlay(ndvi2015_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2015_EVI.tif")
Aug_2015_EVI <- overlay(ndvi2015_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2015_EVI.tif")
Sep_2015_EVI <- overlay(ndvi2015_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2015_EVI.tif")
Oct_2015_EVI <- overlay(ndvi2015_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2015_EVI.tif")
Nov_2015_EVI <- overlay(ndvi2015_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2015_EVI.tif")
Dec_2015_EVI <- overlay(ndvi2015_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2015_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2015_EVI.tif")
#For 2016
Jan_2016_EVI <- overlay(ndvi2016_scaled[[1]], ndvi_scaled[[2]], fun=mean_na)
writeRaster(Jan_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jan_2016_EVI.tif")
Feb_2016_EVI <- overlay(ndvi2016_scaled[[3]], ndvi_scaled[[4]], fun=mean_na)
writeRaster(Feb_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Feb_2016_EVI.tif")
Mar_2016_EVI <- overlay(ndvi2016_scaled[[5]], ndvi_scaled[[6]], fun=mean_na)
writeRaster(Mar_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Mar_2016_EVI.tif")
Apr_2016_EVI <- overlay(ndvi2016_scaled[[7]], ndvi_scaled[[8]], fun=mean_na)
writeRaster(Apr_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Apr_2016_EVI.tif")
May_2016_EVI <- overlay(ndvi2016_scaled[[9]], ndvi_scaled[[10]], fun=mean_na)
writeRaster(May_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/May_2016_EVI.tif")
Jun_2016_EVI <- overlay(ndvi2016_scaled[[11]], ndvi_scaled[[12]], fun=mean_na)
writeRaster(Jun_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jun_2016_EVI.tif")
Jul_2016_EVI <- overlay(ndvi2016_scaled[[13]], ndvi_scaled[[14]], fun=mean_na)
writeRaster(Jul_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Jul_2016_EVI.tif")
Aug_2016_EVI <- overlay(ndvi2016_scaled[[15]], ndvi_scaled[[16]], fun=mean_na)
writeRaster(Aug_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Aug_2016_EVI.tif")
Sep_2016_EVI <- overlay(ndvi2016_scaled[[17]], ndvi_scaled[[18]], fun=mean_na)
writeRaster(Sep_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Sep_2016_EVI.tif")
Oct_2016_EVI <- overlay(ndvi2016_scaled[[19]],  fun=mean_na)
writeRaster(Oct_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Oct_2016_EVI.tif")
Nov_2016_EVI <- overlay(ndvi2016_scaled[[20]], ndvi_scaled[[21]], fun=mean_na)
writeRaster(Nov_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Nov_2016_EVI.tif")
Dec_2016_EVI <- overlay(ndvi2016_scaled[[22]], ndvi_scaled[[23]], fun=mean_na)
writeRaster(Dec_2016_EVI, filename="Upscaling_Project/Gridded_Inputs/Monthly_EVI/Dec_2016_EVI.tif")

#Create input files-----------------
#Now to create input files for all of these 
getwd()
setwd("/mnt/DATA/users/mbarnes/")

library(doParallel)
library(foreach)
library(raster)
UseCores <- detectCores() -1

rasterOptions(maxmemory=1e+10)
create_input_raster <- function(band1, month, monthno, year){
  filenameEVI <- paste0("Upscaling_Project/Gridded_Inputs/Monthly_EVI/", month, "_", year, "_", "EVI.tif")
  EVI <- raster(filenameEVI)
  filenamePrecip <- "Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif"
  filenameTmax <- "Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif"
  filenameTmin <- "Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif"
  dem <- "Upscaling_Project/Gridded_Inputs/Hydro1k/SW_dem.tif"
=======

#Batch create input files for RF model: 
#Function needs to: 
#1) Open proper Tmax, Tmin, and Precip files (from Daymet files)
#2) Set -9999 to NA for all
#3) Resample Daymet files to EVI resolution
#4) Create raster with only 1 value (month) as input
#5) Write out input files 
#How to do this? Function will be different for each one...ugh

create_input_raster <- function(band1, month, monthno, year){
  filenameEVI <- paste0("F:/Upscaling_Project/Gridded_Inputs/Monthly_EVI/", month, "_", year, "_", "EVI.tif")
  EVI <- raster(filenameEVI)
  filenamePrecip <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif"
  filenameTmax <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif"
  filenameTmin <- "F:/Upscaling_Project/Gridded_Inputs/Daymet/upscalingArea_DAYMET_tmin_2000_2016_AOI.tif"
>>>>>>> 5ad1e365d8d54c1d0773e139a75e498abf0ef0a4
  
  Precip <- raster(filenamePrecip, band = band1)
  Tmax <- raster(filenameTmax, band = band1)
  Tmin <- raster(filenameTmin, band = band1)
<<<<<<< HEAD
  print("Files read in")
=======
>>>>>>> 5ad1e365d8d54c1d0773e139a75e498abf0ef0a4
  
  Tmax[Tmax==-9999] <-NA
  Tmin[Tmin==-9999] <-NA
  Precip[Precip==-9999] <-NA
<<<<<<< HEAD
  print("Subsetting done")
  Upscext <- extent(EVI)
   Precipresample <- resample(Precip, EVI, method="bilinear")
  Tmaxresample <- resample(Tmax, EVI, method="bilinear")
  Tminresample <- resample(Tmin, EVI, method="bilinear")
    print("Resampling done")
  monthrast = raster (ext=Upscext, res=0.002081004)
  values(monthrast) <- monthno
  
  #rast_stack <- stack(Tminresample, Tmaxresample, Precipresample, EVI, monthrast, dem)
  #names(rast_stack) <- paste(c("tmin", "tmax", "precip", "NDVI", "month","dem"))
  
  rast_stack <- stack(EVI, monthrast, dem, Precipresample, Tmaxresample, Tminresample)
  print("rasters stacked")
  names(rast_stack) <- paste(c("NDVI", "month", "dem", "precip", "Tmax", "Tmin"))
  outputfilename <- paste("Upscaling_Project/Gridded_Inputs/RF_Input/",month,"_",year,".tif", sep="")
  print(paste("writing out", outputfilename))
  print(rast_stack)
  writeRaster(rast_stack, outputfilename, overwrite=TRUE)
  gc()
  }

#For 2001-----------------------------------
create_input_raster(band1=13, month="Jan", monthno=1, year=2001)
create_input_raster(band1=14, month="Feb", monthno=2, year=2001)
create_input_raster(band1=15, month="Mar", monthno=3, year=2001)
create_input_raster(band1=16, month="Apr", monthno=4, year=2001)
create_input_raster(band1=17, month="May", monthno=5, year=2001)
create_input_raster(band1=18, month="Jun", monthno=6, year=2001)
create_input_raster(band1=19, month="Jul", monthno=7, year=2001)
create_input_raster(band1=20, month="Aug", monthno=8, year=2001)
create_input_raster(band1=21, month="Sep", monthno=9, year=2001)
create_input_raster(band1=22, month="Oct", monthno=10, year=2001)
create_input_raster(band1=23, month="Nov", monthno=11, year=2001)
create_input_raster(band1=24, month="Dec", monthno=12, year=2001)

#Jan_2001 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2001.tif")
#names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
#Feb_2002 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2002.tif")
#names(Feb_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2002.tif")
#names(Mar_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2002.tif")
#names(Apr_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2002.tif")
#names(May_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2002.tif")
#names(Jun_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2002.tif")
#names(Jul_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2002.tif")
#names(Aug_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2002.tif")
#names(Sep_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2002.tif")
#names(Oct_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2002<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2002.tif")
#names(Nov_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2002.tif")
#names(Dec_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2002-----------------------------------
=======
  
  Upscext <- extent(EVI)
  
  Precipresample <- resample(Precip, EVI, method="bilinear")
  Tmaxresample <- resample(Tmax, EVI, method="bilinear")
  Tminresample <- resample(Tmin, EVI, method="bilinear")
  
  monthrast = raster (ext=Upscext, res=0.002081004)
  values(monthrast) <- monthno
  
  rast_stack <- stack(Tminresample, Tmaxresample, Precipresample, EVI, monthrast)
  names(rast_stack) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
  
  outputfilename <- paste("F:/Upscaling_Project/Gridded_inputs/RF_Input/",month,"_",year,".tif")
  writeRaster(rast_stack, outputfilename)
  return(rast_stack)
}


>>>>>>> 5ad1e365d8d54c1d0773e139a75e498abf0ef0a4
create_input_raster(band1=25, month="Jan", monthno=1, year=2002)
create_input_raster(band1=26, month="Feb", monthno=2, year=2002)
create_input_raster(band1=27, month="Mar", monthno=3, year=2002)
create_input_raster(band1=28, month="Apr", monthno=4, year=2002)
create_input_raster(band1=29, month="May", monthno=5, year=2002)
create_input_raster(band1=30, month="Jun", monthno=6, year=2002)
create_input_raster(band1=31, month="Jul", monthno=7, year=2002)
create_input_raster(band1=32, month="Aug", monthno=8, year=2002)
create_input_raster(band1=33, month="Sep", monthno=9, year=2002)
create_input_raster(band1=34, month="Oct", monthno=10, year=2002)
create_input_raster(band1=35, month="Nov", monthno=11, year=2002)
create_input_raster(band1=36, month="Dec", monthno=12, year=2002)

<<<<<<< HEAD
#Jan_2002 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2002.tif")
#names(Jan_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2002 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2002.tif")
#names(Feb_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2002.tif")
#names(Mar_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2002.tif")
#names(Apr_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2002.tif")
#names(May_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2002.tif")
#names(Jun_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2002.tif")
#names(Jul_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2002.tif")
#names(Aug_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2002.tif")
#names(Sep_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2002.tif")
#names(Oct_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2002<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2002.tif")
#names(Nov_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2002<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2002.tif")
#names(Dec_2002) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2003---------
create_input_raster(band1=37, month="Jan", monthno=1, year=2003)
create_input_raster(band1=38, month="Feb", monthno=2, year=2003)
create_input_raster(band1=39, month="Mar", monthno=3, year=2003)
create_input_raster(band1=40, month="Apr", monthno=4, year=2003)
create_input_raster(band1=41, month="May", monthno=5, year=2003)
create_input_raster(band1=42, month="Jun", monthno=6, year=2003)
create_input_raster(band1=43, month="Jul", monthno=7, year=2003)
create_input_raster(band1=44, month="Aug", monthno=8, year=2003)
create_input_raster(band1=45, month="Sep", monthno=9, year=2003)
create_input_raster(band1=46, month="Oct", monthno=10, year=2003)
create_input_raster(band1=47, month="Nov", monthno=11, year=2003)
create_input_raster(band1=48, month="Dec", monthno=12, year=2003)

#Jan_2003 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2003.tif")
#names(Jan_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2003 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2003.tif")
#names(Feb_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2003.tif")
#names(Mar_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2003.tif")
#names(Apr_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2003.tif")
#names(May_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2003.tif")
#names(Jun_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2003.tif")
#names(Jul_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2003.tif")
#names(Aug_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2003.tif")
#names(Sep_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2003.tif")
#names(Oct_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2003<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2003.tif")
#names(Nov_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2003<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2003.tif")
#names(Dec_2003) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2004--------
create_input_raster(band1=49, month="Jan", monthno=1, year=2004)
create_input_raster(band1=50, month="Feb", monthno=2, year=2004)
create_input_raster(band1=51, month="Mar", monthno=3, year=2004)
create_input_raster(band1=52, month="Apr", monthno=4, year=2004)
create_input_raster(band1=53, month="May", monthno=5, year=2004)
create_input_raster(band1=54, month="Jun", monthno=6, year=2004)
create_input_raster(band1=55, month="Jul", monthno=7, year=2004)
create_input_raster(band1=56, month="Aug", monthno=8, year=2004)
create_input_raster(band1=57, month="Sep", monthno=9, year=2004)
create_input_raster(band1=58, month="Oct", monthno=10, year=2004)
create_input_raster(band1=59, month="Nov", monthno=11, year=2004)
create_input_raster(band1=60, month="Dec", monthno=12, year=2004)

#Jan_2004 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2004.tif")
#names(Jan_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2004 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2004.tif")
#names(Feb_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2004.tif")
#names(Mar_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2004.tif")
#names(Apr_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2004.tif")
#names(May_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2004.tif")
#names(Jun_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2004.tif")
#names(Jul_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2004.tif")
#names(Aug_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2004.tif")
#names(Sep_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2004.tif")
#names(Oct_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2004<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2004.tif")
#names(Nov_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2004<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2004.tif")
#names(Dec_2004) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2005--------
create_input_raster(band1=61, month="Jan", monthno=1, year=2005)
create_input_raster(band1=62, month="Feb", monthno=2, year=2005)
create_input_raster(band1=63, month="Mar", monthno=3, year=2005)
create_input_raster(band1=64, month="Apr", monthno=4, year=2005)
create_input_raster(band1=65, month="May", monthno=5, year=2005)
create_input_raster(band1=66, month="Jun", monthno=6, year=2005)
create_input_raster(band1=67, month="Jul", monthno=7, year=2005)
create_input_raster(band1=68, month="Aug", monthno=8, year=2005)
create_input_raster(band1=69, month="Sep", monthno=9, year=2005)
create_input_raster(band1=70, month="Oct", monthno=10, year=2005)
create_input_raster(band1=71, month="Nov", monthno=11, year=2005)
create_input_raster(band1=72, month="Dec", monthno=12, year=2005)

#Jan_2005 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2005.tif")
#names(Jan_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2005 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2005.tif")
#names(Feb_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2005.tif")
#names(Mar_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2005.tif")
#names(Apr_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2005.tif")
#names(May_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2005.tif")
#names(Jun_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2005.tif")
#names(Jul_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2005.tif")
#names(Aug_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2005.tif")
#names(Sep_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2005.tif")
#names(Oct_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2005<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2005.tif")
#names(Nov_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2005<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2005.tif")
#names(Dec_2005) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2006---------
create_input_raster(band1=73, month="Jan", monthno=1, year=2006)
create_input_raster(band1=74, month="Feb", monthno=2, year=2006)
create_input_raster(band1=75, month="Mar", monthno=3, year=2006)
create_input_raster(band1=76, month="Apr", monthno=4, year=2006)
create_input_raster(band1=77, month="May", monthno=5, year=2006)
create_input_raster(band1=78, month="Jun", monthno=6, year=2006)
create_input_raster(band1=79, month="Jul", monthno=7, year=2006)
create_input_raster(band1=80, month="Aug", monthno=8, year=2006)
create_input_raster(band1=81, month="Sep", monthno=9, year=2006)
create_input_raster(band1=82, month="Oct", monthno=10, year=2006)
create_input_raster(band1=83, month="Nov", monthno=11, year=2006)
create_input_raster(band1=84, month="Dec", monthno=12, year=2006)

#Jan_2006 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2006.tif")
#names(Jan_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2006 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2006.tif")
#names(Feb_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2006.tif")
#names(Mar_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2006.tif")
#names(Apr_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2006.tif")
#names(May_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2006.tif")
#names(Jun_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2006.tif")
#names(Jul_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2006.tif")
#names(Aug_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2006.tif")
#names(Sep_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2006.tif")
#names(Oct_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2006<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2006.tif")
#names(Nov_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2006<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2006.tif")
#names(Dec_2006) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2007---------
create_input_raster(band1=85, month="Jan", monthno=1, year=2007)
create_input_raster(band1=86, month="Feb", monthno=2, year=2007)
create_input_raster(band1=87, month="Mar", monthno=3, year=2007)
create_input_raster(band1=88, month="Apr", monthno=4, year=2007)
create_input_raster(band1=89, month="May", monthno=5, year=2007)
create_input_raster(band1=90, month="Jun", monthno=6, year=2007)
create_input_raster(band1=91, month="Jul", monthno=7, year=2007)
create_input_raster(band1=92, month="Aug", monthno=8, year=2007)
create_input_raster(band1=93, month="Sep", monthno=9, year=2007)
create_input_raster(band1=94, month="Oct", monthno=10, year=2007)
create_input_raster(band1=95, month="Nov", monthno=11, year=2007)
create_input_raster(band1=96, month="Dec", monthno=12, year=2007)

#Jan_2007 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2007.tif")
#names(Jan_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2007 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2007.tif")
#names(Feb_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2007.tif")
#names(Mar_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2007.tif")
#names(Apr_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2007.tif")
#names(May_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2007.tif")
#names(Jun_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2007.tif")
#names(Jul_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2007.tif")
#names(Aug_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2007.tif")
#names(Sep_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2007.tif")
#names(Oct_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2007<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2007.tif")
#names(Nov_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2007<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2007.tif")
#names(Dec_2007) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2008---------
create_input_raster(band1=97, month="Jan", monthno=1, year=2008)
create_input_raster(band1=98, month="Feb", monthno=2, year=2008)
create_input_raster(band1=99, month="Mar", monthno=3, year=2008)
create_input_raster(band1=100, month="Apr", monthno=4, year=2008)
create_input_raster(band1=101, month="May", monthno=5, year=2008)
create_input_raster(band1=102, month="Jun", monthno=6, year=2008)
create_input_raster(band1=103, month="Jul", monthno=7, year=2008)
create_input_raster(band1=104, month="Aug", monthno=8, year=2008)
create_input_raster(band1=105, month="Sep", monthno=9, year=2008)
create_input_raster(band1=106, month="Oct", monthno=10, year=2008)
create_input_raster(band1=107, month="Nov", monthno=11, year=2008)
create_input_raster(band1=108, month="Dec", monthno=12, year=2008)


#Jan_2008 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2008.tif")
#names(Jan_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2008 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2008.tif")
#names(Feb_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2008.tif")
#names(Mar_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2008.tif")
#names(Apr_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2008.tif")
#names(May_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2008.tif")
#names(Jun_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2008.tif")
#names(Jul_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2008.tif")
#names(Aug_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2008.tif")
#names(Sep_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2008.tif")
#names(Oct_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2008<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2008.tif")
#names(Nov_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2008<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2008.tif")
#names(Dec_2008) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))


#For 2009---------
create_input_raster(band1=109, month="Jan", monthno=1, year=2009)
create_input_raster(band1=110, month="Feb", monthno=2, year=2009)
create_input_raster(band1=111, month="Mar", monthno=3, year=2009)
create_input_raster(band1=112, month="Apr", monthno=4, year=2009)
create_input_raster(band1=113, month="May", monthno=5, year=2009)
create_input_raster(band1=114, month="Jun", monthno=6, year=2009)
create_input_raster(band1=115, month="Jul", monthno=7, year=2009)
create_input_raster(band1=116, month="Aug", monthno=8, year=2009)
create_input_raster(band1=117, month="Sep", monthno=9, year=2009)
create_input_raster(band1=118, month="Oct", monthno=10, year=2009)
create_input_raster(band1=119, month="Nov", monthno=11, year=2009)
create_input_raster(band1=120, month="Dec", monthno=12, year=2009)

#Jan_2009 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2009.tif")
#names(Jan_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2009 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2009.tif")
#names(Feb_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2009.tif")
#names(Mar_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2009.tif")
#names(Apr_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2009.tif")
#names(May_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2009.tif")
#names(Jun_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2009.tif")
#names(Jul_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2009.tif")
#names(Aug_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2009.tif")
#names(Sep_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2009.tif")
#names(Oct_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2009<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2009.tif")
#names(Nov_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2009<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2009.tif")
#names(Dec_2009) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#3) Add Water Balance rasters to gridded input datasets 
#For 2010---------
create_input_raster(band1=121, month="Jan", monthno=1, year=2010)
create_input_raster(band1=122, month="Feb", monthno=2, year=2010)
create_input_raster(band1=123, month="Mar", monthno=3, year=2010)
create_input_raster(band1=124, month="Apr", monthno=4, year=2010)
create_input_raster(band1=125, month="May", monthno=5, year=2010)
create_input_raster(band1=126, month="Jun", monthno=6, year=2010)
create_input_raster(band1=127, month="Jul", monthno=7, year=2010)
create_input_raster(band1=128, month="Aug", monthno=8, year=2010)
create_input_raster(band1=129, month="Sep", monthno=9, year=2010)
create_input_raster(band1=130, month="Oct", monthno=10, year=2010)
create_input_raster(band1=131, month="Nov", monthno=11, year=2010)
create_input_raster(band1=132, month="Dec", monthno=12, year=2010)

#Jan_2010 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2010.tif")
#names(Jan_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2010 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2010.tif")
#names(Feb_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2010.tif")
#names(Mar_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2010.tif")
#names(Apr_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2010.tif")
#names(May_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2010.tif")
#names(Jun_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2010.tif")
#names(Jul_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2010.tif")
#names(Aug_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2010.tif")
#names(Sep_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2010.tif")
#names(Oct_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2010<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2010.tif")
#names(Nov_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2010<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2010.tif")
#names(Dec_2010) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2011--------
create_input_raster(band1=133, month="Jan", monthno=1, year=2011)
create_input_raster(band1=134, month="Feb", monthno=2, year=2011)
create_input_raster(band1=135, month="Mar", monthno=3, year=2011)
create_input_raster(band1=136, month="Apr", monthno=4, year=2011)
create_input_raster(band1=137, month="May", monthno=5, year=2011)
create_input_raster(band1=138, month="Jun", monthno=6, year=2011)
create_input_raster(band1=139, month="Jul", monthno=7, year=2011)
create_input_raster(band1=140, month="Aug", monthno=8, year=2011)
create_input_raster(band1=141, month="Sep", monthno=9, year=2011)
create_input_raster(band1=142, month="Oct", monthno=10, year=2011)
create_input_raster(band1=143, month="Nov", monthno=11, year=2011)
create_input_raster(band1=144, month="Dec", monthno=12, year=2011)

#Jan_2011 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2011.tif")
#names(Jan_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2011 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2011.tif")
#names(Feb_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2011.tif")
#names(Mar_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2011.tif")
#names(Apr_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2011.tif")
#names(May_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2011.tif")
#names(Jun_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2011.tif")
#names(Jul_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2011.tif")
#names(Aug_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2011.tif")
#names(Sep_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2011.tif")
#names(Oct_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2011<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2011.tif")
#names(Nov_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2011<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2011.tif")
#names(Dec_2011) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2012---------
create_input_raster(band1=145, month="Jan", monthno=1, year=2012)
create_input_raster(band1=146, month="Feb", monthno=2, year=2012)
create_input_raster(band1=147, month="Mar", monthno=3, year=2012)
create_input_raster(band1=148, month="Apr", monthno=4, year=2012)
create_input_raster(band1=149, month="May", monthno=5, year=2012)
create_input_raster(band1=150, month="Jun", monthno=6, year=2012)
create_input_raster(band1=151, month="Jul", monthno=7, year=2012)
create_input_raster(band1=152, month="Aug", monthno=8, year=2012)
create_input_raster(band1=153, month="Sep", monthno=9, year=2012)
create_input_raster(band1=154, month="Oct", monthno=10, year=2012)
create_input_raster(band1=155, month="Nov", monthno=11, year=2012)
create_input_raster(band1=156, month="Dec", monthno=12, year=2012)


#Jan_2012 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2012.tif")
#names(Jan_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2012 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2012.tif")
#names(Feb_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2012.tif")
#names(Mar_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2012.tif")
#names(Apr_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2012.tif")
#names(May_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2012.tif")
#names(Jun_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2012.tif")
#names(Jul_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2012.tif")
#names(Aug_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2012.tif")
#names(Sep_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2012.tif")
#names(Oct_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2012<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2012.tif")
#names(Nov_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2012<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2012.tif")
#names(Dec_2012) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2013---------

create_input_raster(band1=157, month="Jan", monthno=1, year=2013)
create_input_raster(band1=158, month="Feb", monthno=2, year=2013)
create_input_raster(band1=159, month="Mar", monthno=3, year=2013)
create_input_raster(band1=160, month="Apr", monthno=4, year=2013)
create_input_raster(band1=161, month="May", monthno=5, year=2013)
create_input_raster(band1=162, month="Jun", monthno=6, year=2013)
create_input_raster(band1=163, month="Jul", monthno=7, year=2013)
create_input_raster(band1=164, month="Aug", monthno=8, year=2013)
create_input_raster(band1=165, month="Sep", monthno=9, year=2013)
create_input_raster(band1=166, month="Oct", monthno=10, year=2013)
create_input_raster(band1=167, month="Nov", monthno=11, year=2013)
create_input_raster(band1=168, month="Dec", monthno=12, year=2013)

#Jan_2013 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2013.tif")
#names(Jan_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2013 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2013.tif")
#names(Feb_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2013.tif")
#names(Mar_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2013.tif")
#names(Apr_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2013.tif")
#names(May_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2013.tif")
#names(Jun_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2013.tif")
#names(Jul_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2013.tif")
#names(Aug_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2013.tif")
#names(Sep_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2013.tif")
#names(Oct_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2013<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2013.tif")
#names(Nov_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2013<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2013.tif")
#names(Dec_2013) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2014--------


create_input_raster(band1=169, month="Jan", monthno=1, year=2014)
create_input_raster(band1=170, month="Feb", monthno=2, year=2014)
create_input_raster(band1=171, month="Mar", monthno=3, year=2014)
create_input_raster(band1=172, month="Apr", monthno=4, year=2014)
create_input_raster(band1=173, month="May", monthno=5, year=2014)
create_input_raster(band1=174, month="Jun", monthno=6, year=2014)
create_input_raster(band1=175, month="Jul", monthno=7, year=2014)
create_input_raster(band1=176, month="Aug", monthno=8, year=2014)
create_input_raster(band1=177, month="Sep", monthno=9, year=2014)
create_input_raster(band1=178, month="Oct", monthno=10, year=2014)
create_input_raster(band1=179, month="Nov", monthno=11, year=2014)
create_input_raster(band1=180, month="Dec", monthno=12, year=2014)

#Jan_2014 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2014.tif")
#names(Jan_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2014 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2014.tif")
#names(Feb_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2014.tif")
#names(Mar_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2014.tif")
#names(Apr_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2014.tif")
#names(May_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2014.tif")
#names(Jun_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2014.tif")
#names(Jul_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2014.tif")
#names(Aug_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2014.tif")
#names(Sep_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2014.tif")
#names(Oct_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2014<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2014.tif")
#names(Nov_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2014<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2014.tif")
#names(Dec_2014) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2015---------
create_input_raster(band1=181, month="Jan", monthno=1, year=2015)
create_input_raster(band1=182, month="Feb", monthno=2, year=2015)
create_input_raster(band1=183, month="Mar", monthno=3, year=2015)
create_input_raster(band1=184, month="Apr", monthno=4, year=2015)
create_input_raster(band1=185, month="May", monthno=5, year=2015)
create_input_raster(band1=186, month="Jun", monthno=6, year=2015)
create_input_raster(band1=187, month="Jul", monthno=7, year=2015)
create_input_raster(band1=188, month="Aug", monthno=8, year=2015)
create_input_raster(band1=189, month="Sep", monthno=9, year=2015)
create_input_raster(band1=190, month="Oct", monthno=10, year=2015)
create_input_raster(band1=191, month="Nov", monthno=11, year=2015)
create_input_raster(band1=192, month="Dec", monthno=12, year=2015)

#Jan_2015 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2015.tif")
#names(Jan_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2015 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2015.tif")
#names(Feb_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2015.tif")
#names(Mar_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2015.tif")
#names(Apr_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2015.tif")
#names(May_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2015.tif")
#names(Jun_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2015.tif")
#names(Jul_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2015.tif")
#names(Aug_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2015.tif")
#names(Sep_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2015.tif")
#names(Oct_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2015<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2015.tif")
#names(Nov_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2015<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2015.tif")
#names(Dec_2015) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))

#For 2016--------
create_input_raster(band1=193, month="Jan", monthno=1, year=2016)
create_input_raster(band1=194, month="Feb", monthno=2, year=2016)
create_input_raster(band1=195, month="Mar", monthno=3, year=2016)
create_input_raster(band1=196, month="Apr", monthno=4, year=2016)
create_input_raster(band1=197, month="May", monthno=5, year=2016)
create_input_raster(band1=198, month="Jun", monthno=6, year=2016)
create_input_raster(band1=199, month="Jul", monthno=7, year=2016)
create_input_raster(band1=200, month="Aug", monthno=8, year=2016)
create_input_raster(band1=201, month="Sep", monthno=9, year=2016)
create_input_raster(band1=202, month="Oct", monthno=10, year=2016)
create_input_raster(band1=203, month="Nov", monthno=11, year=2016)
create_input_raster(band1=204, month="Dec", monthno=12, year=2016)

#Jan_2016 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2016.tif")
#names(Jan_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Feb_2016 <- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2016.tif")
#names(Feb_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Mar_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2016.tif")
#names(Mar_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Apr_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2016.tif")
#names(Apr_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#May_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2016.tif")
#names(May_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jun_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2016.tif")
#names(Jun_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Jul_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2016.tif")
#names(Jul_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Aug_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2016.tif")
#names(Aug_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Sep_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2016.tif")
#names(Sep_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Oct_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2016.tif")
#names(Oct_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Nov_2016<-stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2016.tif")
#names(Nov_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))
#Dec_2016<- stack("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2016.tif")
#names(Dec_2016) <- paste(c("tmin", "tmax", "precip", "NDVI", "month"))




=======
LC_Resample <- raster("F:/Upscaling_Project/Gridded_Inputs/LC_Resample.tif")
dem <- raster("F:/Upscaling_Project/Gridded_Inputs/Hydro1k/SW_dem.tif")

Jan_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2001.tif"), dem)
names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Feb_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2001.tif"), dem)
names(Feb_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Mar_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2001.tif"), dem)
names(Mar_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Apr_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2001.tif"), dem)
names(Apr_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
May_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2001.tif"), dem)
names(May_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Jun_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2001.tif"), dem)
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Jul_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2001.tif"), dem)
names(Jul_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Aug_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2001.tif"), dem)
names(Aug_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Sep_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2001.tif"), dem)
names(Sep_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Oct_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2001.tif"), dem)
names(Oct_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Nov_2001<-stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2001.tif"), dem)
names(Nov_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
Dec_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2001.tif"), dem)
names(Dec_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "elev"))
sw <- extent(Jan_2001)


RF5 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF5_11_13.rds")

#started @ 3:50 pm
Jan_2001_GPP <- predict(Jan_2001, RF5, ext=sw)
Feb_2001_GPP <- predict(Feb_2001, RF5, ext=sw)
Mar_2001_GPP <- predict(Mar_2001, RF5, ext=sw)
Apr_2001_GPP <- predict(Apr_2001, RF5, ext=sw)
May_2001_GPP <- predict(May_2001, RF5, ext=sw)
Jun_2001_GPP <- predict(Jun_2001, RF5, ext=sw)
Jul_2001_GPP <- predict(Jul_2001, RF5, ext=sw)
Aug_2001_GPP <- predict(Aug_2001, RF5, ext=sw)
Sep_2001_GPP <- predict(Sep_2001, RF5, ext=sw)
Oct_2001_GPP <- predict(Oct_2001, RF5, ext=sw)
Nov_2001_GPP <- predict(Nov_2001, RF5, ext=sw)
Dec_2001_GPP <- predict(Dec_2001, RF5, ext=sw)


Stack_2001_GPP <- stack(Jan_2001_GPP, Feb_2001_GPP, Mar_2001_GPP, Apr_2001_GPP, May_2001_GPP, Jun_2001_GPP, Jul_2001_GPP,
      Aug_2001_GPP, Sep_2001_GPP, Oct_2001_GPP, Nov_2001_GPP, Dec_2001_GPP)

names(Stack_2001_GPP) <- paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                 "Sep", "Oct", "Nov", "Dec"))
plot(Stack_2001_GPP, zlim=c(0,6))
writeRaster(Stack_2001_GPP, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Stack_2001_GPP.tif")
writeRaster(Stack_2001_GPP, "F:/Upscaling_Project/Stack_2001_GPP.tif")

Stack_2001_GPP <- stack("F:/Upscaling_Project/Stack_2001_GPP.tif")
plot(Stack_2001_GPP, zlim=c(0,6))

Stack_2001_GPP

#US-Srm
SRMpoint <- cbind(-110.86, 31.82)
SRMresult <- extract(Stack_2001_GPP, SRMpoint)
SRMplot(result[1:12])

#US-fuf
FUFpoint <- cbind(-111.76, 35.12)
FUFresult <-extract(Stack_2001_GPP, FUFpoint)
plot(FUFresult[1:12])

#US-CA
CApoint <- cbind(-119.76, 37.12)
CAresult <-extract(Stack_2001_GPP, CApoint)
plot(CAresult[1:12])

#US-Wkg
WKGpoint <- cbind(-109.96, 31.74)
WKGresult <-extract(Stack_2001_GPP, WKGpoint)
plot(WKGresult[1:12])



#as.data.frame(getValues(Stack_2002_GPP))
#timeseries <- extract(Stack_2002_GPP, 1:ncell(Stack_2002_GPP))
#head(timeseries)
#writeRaster(Stack_2002_GPP, "F:/Upscaling_Project/Upscaling_Project_2017/GPP_2002.grd", format="raster", overwrite=TRUE)


file <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-srm_merged.csv")
file$date <- as.Date(paste("01", file$monthyear, sep="_"), format="%d_%b_%Y")
str(file)
file[10:20,]
#Initial TS plot

p <- ggplot() +
  geom_line(data = file, aes(x = date, y = GPP, color =I("red"))) +
  geom_line(data = file, aes(x = date, y = Jung_2011, color = I("blue"))) +
  geom_line(data = file, aes(x = date, y = Jung_2017, color = I("green"))) +
  xlab('data_date') +
  ylab('GPP')

p

#get mean seasonal cycle
file$month <- month(file$date)
file$year <- year(file$date)

seasonal <- ddply(file, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
seasonal$Barnes <- (WKGresult[1:12])
str(seasonal)

q <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonal, aes(x=month, y=Barnes, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Wkg")+
  theme_bw()



point <- cbind(-110.75, 31.75)
result <- extract(Stack_2001_GPP, point)
str(result)
str(seasonal)
>>>>>>> 5ad1e365d8d54c1d0773e139a75e498abf0ef0a4
