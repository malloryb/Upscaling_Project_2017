#Creating global map
library(gdal)
library(gdalUtils)
library(raster)

#To-do: create input raster. Testing on 1 month. 
#NDVI data from: ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_4km/geo_TIFF/
#The data is weekly. Average the 4 .tifs together to get average veg...will be faster if subsetted first though...
#User guide here: https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH_doc/VHP_uguide_v1.4_2013_1221.pdf

#Found NDVI! 
Jul_2010_wk1 <- raster("D:/Upscaling_Project/Test_Global_Upscaling/VHP.G04.C07.NN.P2010027.SM.SMN.tif")
#Set -999 to NA and Apply scaling factor: 0.0010
Jul_2010_wk1[Jul_2010_wk1 ==-9999] <- NA
#Get spei12, spei1, and spei6 data
spei6 <- brick("D:/Upscaling_Project/Test_Global_Upscaling/spei06.nc")[[1315]]
spei12 <- brick("D:/Upscaling_Project/Test_Global_Upscaling/spei12.nc")[[1315]]
spei1 <- brick("D:/Upscaling_Project/Test_Global_Upscaling/spei01.nc")[[1315]]
#Precip
precip <- brick("D:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.pre.dat.nc")[[115]]
#Tmin
tmin <- brick("D:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.tmn.dat.nc")[[115]]
#Tmax
tmax <- brick("D:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.tmx.dat.nc")[[115]]
#VP
vp<- brick("D:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.vap.dat.nc")[[115]]
#Srad: workflow here https://gis.stackexchange.com/questions/20018/how-can-i-convert-data-in-the-form-of-lat-lon-value-into-a-raster-file-using-r
#srad from here: https://eosweb.larc.nasa.gov/cgi-bin/sse/global.cgi?email=skip@larc.nasa.gov
pts  <- read.csv("D:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
pts <- pts[,c(1:2, 9)]
coordinates(pts)=~Lon+Lat
proj4string(pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
pts = spTransform(pts,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
gridded(pts) = TRUE
srad = raster(pts)
projection(srad) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(srad)
#Daylength the same as srad but also must convert to seconds from hours
pts  <- read.csv("D:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
pts <- pts[,c(1:2)]
pts2  <- read.csv("D:/Upscaling_Project/Test_Global_Upscaling/daylight.csv")
pts2 <- pts2[,c(1,8)]
merged <- merge(pts, pts2, by="Lat")
coordinates(merged)=~Lon+Lat
proj4string(merged)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
pts = spTransform(merged,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
gridded(pts) = TRUE
dayl = raster(pts)
projection(dayl) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
dayl <- dayl*3600
plot(dayl)
#month raster
sh <- extent(-180,180,-90, -89.1)
shrast = raster(ext=sh, re=0.5)
monthrast = raster(ext=cru_ext, res=0.5)
values(monthrast) <- 7
values(shrast) <- 1
month <- mosaic(monthrast, shrast, fun=min)
plot(month)

#Get MAP/MAT: http://www.worldclim.org/bioclim
climate <- getData('worldclim', var='bio', res=2.5)
#BIO1 = Annual Mean Temperature
#BIO12 = Annual Precipitation
MAT <- (climate$bio1)/10
MAP <- (climate$bio12)
plot(MAT)
plot(MAP)
#Elevation
# Isolate the name of the first sds
library(raster)
library(sp)
library(rgdal) 
filename <- "D:/Upscaling_Project/Test_Global_Upscaling/dem060.hdf"
outfile="testout"
gdal_translate(filename,outfile,sds=TRUE,verbose=TRUE)
file.rename(outfile,paste(outfile,".tif",sep=""))
test <- raster("D:/Upscaling_Project/Upscaling_Project_2017/testout.tif")
elev <- flip(test, direction="y")
elev[elev ==-9999] <- NA
crs(elev) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
extent(elev) <- c(-180, 180, -90, 90)
#Get month
sh <- extent(-180,180,-90, 0)
shrast = raster(ext=sh, re=0.5)
monthrast = raster(ext=cru_ext, res=0.5)
values(monthrast) <- 7
values(shrast) <- 1
month <- mosaic(monthrast, shrast, fun=min)
plot(month)
#Then transform to whatever projection the raster is:
#Need to resample all to the same resolution and extent -> we'll do the resolution of the cru data 
cru_ext <- extent(vp)
spei12 <- resample(spei12, vp, method="bilinear")
spei6 <- resample(spei6, vp, method="bilinear")
spei1 <- resample(spei1, vp, method="bilinear")
Jul_2010_wk1<- resample(vp, vp, method="bilinear")
elev <- resample(elev, vp, method="bilinear")
srad <- resample(srad, vp, method="bilinear")
month <- resample(month, vp, method="bilinear")
MAT <- resample(MAT, vp, method="bilinear")
MAP <- resample(MAP, vp, method="bilinear")
dayl <- resample(dayl, vp, method="bilinear")
print("resampling done")

rast_stack <- stack(Jul_2010_wk1, month, elev, precip, tmax, tmin, MAP, MAT, srad, vp, dayl, spei1, spei6, spei12)
plot(rast_stack)
rast_ext <- extent(rast_stack[[1]])
names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "daylength", "spei1", "spei6","spei12"))

library(caret)
library(raster)
library(randomForest)
library(ncdf4)
RFC3 <- readRDS("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_3_11.rds")
#Going to mask before I do this
require(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
Jul_2001_GPP <- predict(rast_stack, RFC3, ext=rast_ext)
plot(Jul_2001_GPP)
mask <- raster("D:/Upscaling_Project/Test_Global_Upscaling/Drylands_dataset_2007/CBBNDrylands.tif")
plot(mask)
mask[mask < 0.5] <- NA
mask[mask >0.5] <-1
plot(mask)
mask <- resample(mask, Jul_2001_GPP, method="bilinear")
Jul_2001_dry <- mask(Jul_2001_GPP, mask)
plot(Jul_2001_dry)
