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

#OK this seriously took over an hour on ARS computer, seems to be quicker on USDA computer
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
  
  Precip <- raster(filenamePrecip, band = band1)
  Tmax <- raster(filenameTmax, band = band1)
  Tmin <- raster(filenameTmin, band = band1)
  
  Tmax[Tmax==-9999] <-NA
  Tmin[Tmin==-9999] <-NA
  Precip[Precip==-9999] <-NA
  
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

LC_Resample <- raster("F:/Upscaling_Project/Gridded_Inputs/LC_Resample.tif")
plot(LC_Resample)
Jan_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jan_2001.tif"), LC_Resample)
names(Jan_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Feb_2001 <- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Feb_2001.tif"), LC_Resample)
names(Feb_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Mar_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Mar_2001.tif"), LC_Resample)
names(Mar_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Apr_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Apr_2001.tif"), LC_Resample)
names(Apr_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
May_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/May_2001.tif"), LC_Resample)
names(May_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Jun_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jun_2001.tif"), LC_Resample)
names(Jun_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Jul_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Jul_2001.tif"), LC_Resample)
names(Jul_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Aug_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Aug_2001.tif"), LC_Resample)
names(Aug_2001) <-paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Sep_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Sep_2001.tif"), LC_Resample)
names(Sep_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Oct_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Oct_2001.tif"), LC_Resample)
names(Oct_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Nov_2001<-stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Nov_2001.tif"), LC_Resample)
names(Nov_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))
Dec_2001<- stack(("F:/Upscaling_Project/Gridded_Inputs/RF_Input/Dec_2001.tif"), LC_Resample)
names(Dec_2001) <- paste(c("tmin", "tmax", "precip", "NDVI", "month", "IGBP_no"))

sw <- extent(Jan_2001)


RF5 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF5_11_8.rds")

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
point <- cbind(-110.86, 31.82)
result <- extract(Stack_2001_GPP, point)
plot(result[1:12])

#US-fuf
point <- cbind(-111.76, 35.12)
result <-extract(Stack_2001_GPP, point)
plot(result[1:12])

#US-CA
point <- cbind(-119.76, 37.12)
result <-extract(Stack_2001_GPP, point)
plot(result[1:12])

#US-Wkg
point <- cbind(-109.96, 31.74)
result <-extract(Stack_2001_GPP, point)
plot(result[1:12])



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

q <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  geom_line(data= seasonal, aes(x=month, y=Barnes, color=I("purple")))+
  xlab('month') +
  ylab('GPP')+
  ggtitle("US-Wkg")+
  theme_bw()

q


point <- cbind(-110.75, 31.75)
result <- extract(Stack_2001_GPP, point)
str(result)
str(seasonal)
seasonal$Barnes <- (result[1:12])
str(seasonal)
