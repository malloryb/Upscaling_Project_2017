## Daymet Met Files
## Load All Daymet .TIFs 
library(raster)
library(rgdal)
library(sp)
##Navigate to file with .TIFs
setwd("C:/Users/Mallory/odrive/UA_Google_Drive/Daymet_V3_Monthly_Climatology/data/")
##Load all .TIFs into a list - this gets only the precip files and "na" is north america
rlist=list.files(getwd(), pattern="(prcp).*(_na).*\\.tif$", full.names=FALSE)
Daymet_Precip <- stack(rlist)
plot(Daymet_Precip[[4]])
plot(Daymet_Precip)


#Apply to all rasters in stack: 
#1) Reproject
#2) Subset to southwest region only
#3) Extract time series from each site
#3) Write rasters? 

for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
stack(rlist)
#Automatically loads first band
Jan_1980 <- raster("daymet_v3_prcp_monttl_1980_na.tif")
#Load all 12 bands for 1980 into a raster stack
Precip_1980 <- stack("daymet_v3_prcp_monttl_1980_na.tif")
#Plot all 12 bands in stack
plot(Precip_1980)
#Plot 3rd band in stack (should be March)
plot(Precip_1980[[3]])
##Create raster stack
##Subset spatially 
##Pull out TS by lat/long
