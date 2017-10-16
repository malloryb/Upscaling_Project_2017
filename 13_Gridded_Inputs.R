#Code to deal with rasters 

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

#If it's scaled rasters....shouldn't the funciton be mean (ignoring -273.15)? 


s <- scale_raster("MOD11A2.A2001001.LST_Day_1km.tif")

s2 <- lapply(filenames, scale_raster)
s3 <- lapply(filenames_Jan, scale_raster)
stack(s2)

?list.files
myfun("Params/MOD11A2.A2001001.LST_Day_1km.tif")
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
list1 <- unstack(s)
list1
b <- brick(s)
list2 <- unstack(b)

pal.cols<-colorRampPalette(c("blue", "red"))(20)
#If doing for a leap year - double chack this 
plot(Jan_mean, col=pal.cols)
plot(Feb_mean)
plot(Dec_mean)
plot(Mar_mean)

#Load in single LST file
LST1 <- raster("Params/MOD11A2.A2001001.LST_Day_1km.tif")
LST1_date <- extractDate("MOD11A2.A2001001.LST_Day_1km.tif", pos1=10, pos2=16, asDate=TRUE)
LST1_date[[1]][1]
#Set minmax and plot
LST1 <- setMinMax(LST1)
#Plot
plot(LST1)
#Set all 0 values as NA
LST1[LST1==0]<-NA
#Rescale properly; https://gis.stackexchange.com/questions/72524/how-do-i-convert-the-lst-values-on-the-modis-lst-image-to-degree-celsius
LST1 <- ((LST1 * 0.02) - 273.15)
#Plot
plot(LST1)

#Second Raster file
LST2 <- raster("Params/MOD11A2.A2001097.LST_Day_1km.tif")
#Set minmax and plot
LST2 <- setMinMax(LST2)
#Plot
plot(LST2)

#Set all 0 values as NA
LST2[LST2==0]<-NA
#Rescale properly; https://gis.stackexchange.com/questions/72524/how-do-i-convert-the-lst-values-on-the-modis-lst-image-to-degree-celsius
LST2 <- ((LST2 * 0.02) - 273.15)
#Plot
LST2
plot(LST2, col=rev(heat.colors(n=8)))
LST1
LST2
#X and Y axes flipped
#Extent for all rasters the same: extent: -9565882, -6152882, 5655714, 14482714  (xmin, xmax, ymin, ymax)

imported_raster=raster(str_name)

a <- raster(xmn=-100, xmx=100, ymn=-90, ymx=90,res=10) 

# for reproducible example
r1 <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90,res=10)
r2 <- raster(xmn=-190, xmx=180, ymn=-80, ymx=10,res=10)
r3 <- raster(xmn=-50, xmx=130, ymn=-80, ymx=90,res=10)

a <- setValues(a, 1:ncell(a))
r1 <- setValues(r1, 1:ncell(r1))
r2 <- setValues(r2, 1:ncell(r2))
r3 <- setValues(r3, 1:ncell(r3))

files <- list()
files[[1]] <- r1
files[[2]] <- r2
files[[3]] <- r3

results <- list()

for(i in 1:length(files)) {
  e <- extent(a)
  r <-files[[i]] # raster(files[i])
  rc <- crop(r, e)
  if(sum(as.matrix(extent(rc))!=as.matrix(e)) == 0){ # edited
    rc <- mask(rc, a) # You can't mask with extent, only with a Raster layer, RStack or RBrick
  }else{
    rc <- extend(rc,a)
    rc<- mask(rc, a)
  }
  
  # commented for reproducible example      
  results[[i]] <- rc # rw <- writeRaster(rc, outfiles[i], overwrite=TRUE)
  # print(outfiles[i])
  
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



#Read reasters from Guillermo


raster("C:/Users/Mallory/odrive/UA_Google_Drive/Daymet_MODIS_Rasters_All/upscalingArea_2016_EVI.tif")
Precip <-raster("C:/Users/Mallory/odrive/UA_Google_Drive/Daymet_MODIS_Rasters_All/upscalingArea_DAYMET_prcp_2000_2016_AOI.tif")
Temp <- raster("C:/Users/Mallory/odrive/UA_Google_Drive/Daymet_MODIS_Rasters_All/upscalingArea_DAYMET_tmax_2000_2016_AOI.tif")

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

setMinMax(Precip)
Precip[Precip==-9999] <- NA 
plot(Precip[[1]])
Precip


