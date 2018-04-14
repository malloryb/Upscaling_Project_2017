#Creating global map
library(gdal)
library(gdalUtils)
library(raster)
library(stringr)
devtools::install_github("benmarwick/rrtools")
library(rrtools)
rrtools::use_compendium('Dryflux2018')
#To-do: create input raster. Testing on 1 month. 
#NDVI data from: ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/VHP_4km/geo_TIFF/
#The data is weekly. Average the 4 .tifs together to get average veg...will be faster if subsetted first though...
#User guide here: https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH_doc/VHP_uguide_v1.4_2013_1221.pdf
#Found NDVI! 
#For files up to 2010
GlobalAnalysis <- function(bandsp, bandcru, year, month, moname, shmonth){
  require(lubridate)
  require(plyr)
  require(dplyr)
  require(gdal)
  require(caret)
  require(randomForest)
  require(raster)
  require(ncdf4)
  date <-as.Date(paste(year, month, "01", sep="-"), format="%Y-%m-%d")
  #SPEI now
  spei6 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei06.nc")[[bandsp]]
  spei12 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei12.nc")[[bandsp]]
  spei1 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei01.nc")[[bandsp]]
  #Precip
  precip <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.pre.dat.nc")[[bandcru]]
  #Tmin
  tmin <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.tmn.dat.nc")[[bandcru]]
  #Tmax
  tmax <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.tmx.dat.nc")[[bandcru]]
  #VP
  vp<- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2001.2010.vap.dat.nc")[[bandcru]]
  print("spei and cru data read in")
  #month raster
  cru_ext <- extent(-180,180,-90, 90)
  sh <- extent(-180,180,-90, 0)
  shrast = raster(ext=sh, res=0.5)
  monthrast = raster(ext=cru_ext, res=0.5)
  values(monthrast) <- month
  values(shrast) <- shmonth
  monthR <- mosaic(monthrast, shrast, fun=ifelse(month <7, max, min))
  #plot(monthR)
  print("monthrast calculated")
  pts  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
  head(pts)
  pts <- pts[,c("Lat", "Lon", moname)]
  coordinates(pts)=~Lon+Lat
  proj4string(pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
  pts = spTransform(pts,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
  gridded(pts) = TRUE
  srad = raster(pts)
  projection(srad) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #Daylength
  pts  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
  pts <- pts[,c("Lat", "Lon")]
  pts2  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/daylight.csv")
  pts2 <- pts2[,c("Lat", moname)]
  merged <- merge(pts, pts2, by="Lat")
  coordinates(merged)=~Lon+Lat
  proj4string(merged)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
  pts = spTransform(merged,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
  gridded(pts) = TRUE
  dayl = raster(pts)
  projection(dayl) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  dayl <- dayl*3600
 print("srad and elev read in")
#Mat and map
MAT <- raster("F:/Upscaling_Project/Test_Global_Upscaling/MAT_raster")
MAP <- raster("F:/Upscaling_Project/Test_Global_Upscaling/MAP_raster")
print("map and mat read in")
#elevation
elev <- raster("F:/Upscaling_Project/Test_Global_Upscaling/elevation.tif")
print("all files loaded")

print("getting NDVI files")
ndvi_files <- list.files("F:/Upscaling_Project/Test_Global_Upscaling/Gridded_NDVI/")
#select all files for a given year and month
ndvi_proc <- function(x){
  year <- substring(basename(x), 17,20)
  week <- as.numeric(substring(basename(x), 22,23))
  roughday <- week*7
  date <- as.Date(paste(year, roughday, sep="-"), "%Y-%j")
  floor <- floor_date(date, unit=c("month"))
  return(as.character(floor))
}
names(ndvi_files) <-lapply(ndvi_files, ndvi_proc)

list_to_df <- function(listfordf){
  
  df <- list(list.element = listfordf)
  class(df) <- c("data.frame")
  attr(df, "row.names") <- .set_row_names(length(listfordf))
  
  if (!is.null(names(listfordf))) {
    df$name <- names(listfordf)
  }
  
  df
}
ndvi_files <- list_to_df(ndvi_files)
ndvi_files <- as.data.frame(do.call("cbind",ndvi_files))
ndvi_month <- (subset(ndvi_files, name==as.character(date)))
names(ndvi_month) <- c("filename", "date")
print(ndvi_month)
ndvi_to_average <- as.list(ndvi_month$filename)
print("processing ndvi")
proc_ndvi <- function(x){
  filename <- paste("F:/Upscaling_Project/Test_Global_Upscaling/Gridded_NDVI/", x, sep="")
  x <- raster(filename, ext=c(-180,180,-90,90))
  proj4string(x)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  x[x==-9999] <- NA
  return(x)
}

R <- stack(lapply(ndvi_to_average, proc_ndvi))
#plot(R)
NDVI <- calc(R, mean, na.rm=TRUE)
plot(NDVI)
print(NDVI)
print("monthly NDVI obtained")

spei12 <- resample(spei12, vp, method="bilinear")
print("spei12")
spei6 <- resample(spei6, vp, method="bilinear")
print("spei6")
spei1 <- resample(spei1, vp, method="bilinear")
print("spei1")
elev <- resample(elev, vp, method="bilinear")
print("elev")
srad <- resample(srad, vp, method="bilinear")
print("srad")
monthR <- resample(monthR, vp, method="bilinear")
print("month")
MAT <- resample(MAT, vp, method="bilinear")
print("MAT")
MAP <- resample(MAP, vp, method="bilinear")
print("MAP")
dayl <- resample(dayl, vp, method="bilinear")
print("dayl")
NDVI <- resample(NDVI, vp, method="bilinear")
print("NDVI")
print("resampling done")

rast_stack <- stack(NDVI, monthR, elev, precip, tmax, tmin, MAP, MAT, srad, vp, dayl, spei1, spei6, spei12)
rast_ext <- extent(rast_stack[[1]])
names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "daylength", "spei1", "spei6","spei12"))
#plot(rast_stack)

RFC3 <- readRDS("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_3_11.rds")
varImp(RFC3)
#varImpPlot(RFC3)
#Going to mask before I do this

# Read SHAPEFILE.shp from the current working directory (".")
GPP <- predict(rast_stack, RFC3, ext=rast_ext)
#plot(GPP)
mask <- raster("F:/Upscaling_Project/Test_Global_Upscaling/Drylands_dataset_2007/CBBNDrylands.tif")
#plot(mask)
mask[mask < 0.5] <- NA
mask[mask >0.5] <-1
#plot(mask)
mask <- resample(mask, GPP, method="bilinear")
GPP_drylands <- mask( GPP, mask)
title <- paste("Upscaled GPP", moname, year, sep=" ")
filenameF <- paste("F:/Upscaling_Project/Test_Global_Upscaling/", moname, year, ".tif", sep="_")
writeRaster(GPP_drylands, filenameF, overwrite=TRUE)
#plot(GPP_drylands, ggtitle = title)  
  } 
#For files 2011 and after 
GlobalAnalysis2 <- function(bandsp, bandcru, year, month, moname, shmonth){
  require(lubridate)
  require(plyr)
  require(dplyr)
  require(gdal)
  require(caret)
  require(randomForest)
  require(raster)
  require(ncdf4)
  date <-as.Date(paste(year, month, "01", sep="-"), format="%Y-%m-%d")
  #SPEI now
  spei6 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei06.nc")[[bandsp]]
  spei12 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei12.nc")[[bandsp]]
  spei1 <- brick("F:/Upscaling_Project/Test_Global_Upscaling/spei01.nc")[[bandsp]]
  print("SPEI loaded")
  #Precip
  precip <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2011.2016.pre.dat.nc")[[bandcru]]
  #Tmin
  tmin <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2011.2016.tmn.dat.nc")[[bandcru]]
  #Tmax
  tmax <- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2011.2016.tmx.dat.nc")[[bandcru]]
  #VP
  vp<- brick("F:/Upscaling_Project/Test_Global_Upscaling/cru_ts4.01.2011.2016.vap.dat.nc")[[bandcru]]
  print("spei and cru data read in")
  #month raster
  cru_ext <- extent(-180,180,-90, 90)
  sh <- extent(-180,180,-90, 0)
  shrast = raster(ext=sh, res=0.5)
  monthrast = raster(ext=cru_ext, res=0.5)
  values(monthrast) <- month
  values(shrast) <- shmonth
  monthR <- mosaic(monthrast, shrast, fun=ifelse(month <7, max, min))
  #plot(monthR)
  print("monthrast calculated")
  pts  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
  head(pts)
  pts <- pts[,c("Lat", "Lon", moname)]
  coordinates(pts)=~Lon+Lat
  proj4string(pts)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
  pts = spTransform(pts,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
  gridded(pts) = TRUE
  srad = raster(pts)
  projection(srad) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #Daylength
  pts  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/global_radiation.csv")
  pts <- pts[,c("Lat", "Lon")]
  pts2  <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/daylight.csv")
  pts2 <- pts2[,c("Lat", moname)]
  merged <- merge(pts, pts2, by="Lat")
  coordinates(merged)=~Lon+Lat
  proj4string(merged)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # set it to lat-long
  pts = spTransform(merged,CRS(("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
  gridded(pts) = TRUE
  dayl = raster(pts)
  projection(dayl) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  dayl <- dayl*3600
  print("srad and elev read in")
  #Mat and map
  MAT <- raster("F:/Upscaling_Project/Test_Global_Upscaling/MAT_raster")
  MAP <- raster("F:/Upscaling_Project/Test_Global_Upscaling/MAP_raster")
  print("map and mat read in")
  #elevation
  elev <- raster("F:/Upscaling_Project/Test_Global_Upscaling/elevation.tif")
  print("all files loaded")
  
  print("getting NDVI files")
  ndvi_files <- list.files("F:/Upscaling_Project/Test_Global_Upscaling/Gridded_NDVI/")
  #select all files for a given year and month
  ndvi_proc <- function(x){
    year <- substring(basename(x), 17,20)
    week <- as.numeric(substring(basename(x), 22,23))
    roughday <- week*7
    date <- as.Date(paste(year, roughday, sep="-"), "%Y-%j")
    floor <- floor_date(date, unit=c("month"))
    return(as.character(floor))
  }
  names(ndvi_files) <-lapply(ndvi_files, ndvi_proc)
  
  list_to_df <- function(listfordf){
    
    df <- list(list.element = listfordf)
    class(df) <- c("data.frame")
    attr(df, "row.names") <- .set_row_names(length(listfordf))
    
    if (!is.null(names(listfordf))) {
      df$name <- names(listfordf)
    }
    
    df
  }
  ndvi_files <- list_to_df(ndvi_files)
  ndvi_files <- as.data.frame(do.call("cbind",ndvi_files))
  ndvi_month <- (subset(ndvi_files, name==as.character(date)))
  names(ndvi_month) <- c("filename", "date")
  print(ndvi_month)
  ndvi_to_average <- as.list(ndvi_month$filename)
  print("processing ndvi")
  proc_ndvi <- function(x){
    filename <- paste("F:/Upscaling_Project/Test_Global_Upscaling/Gridded_NDVI/", x, sep="")
    x <- raster(filename, ext=c(-180,180,-90,90))
    proj4string(x)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    x[x==-9999] <- NA
    return(x)
  }
  
  R <- stack(lapply(ndvi_to_average, proc_ndvi))
  #plot(R)
  NDVI <- calc(R, mean, na.rm=TRUE)
  #plot(NDVI)
  print(NDVI)
  print("monthly NDVI obtained")
  
  spei12 <- resample(spei12, vp, method="bilinear")
  print("spei12")
  spei6 <- resample(spei6, vp, method="bilinear")
  print("spei6")
  spei1 <- resample(spei1, vp, method="bilinear")
  print("spei1")
  elev <- resample(elev, vp, method="bilinear")
  print("elev")
  srad <- resample(srad, vp, method="bilinear")
  print("srad")
  monthR <- resample(monthR, vp, method="bilinear")
  print("month")
  MAT <- resample(MAT, vp, method="bilinear")
  print("MAT")
  MAP <- resample(MAP, vp, method="bilinear")
  print("MAP")
  dayl <- resample(dayl, vp, method="bilinear")
  print("dayl")
  NDVI <- resample(NDVI, vp, method="bilinear")
  print("NDVI")
  print("resampling done")
  
  rast_stack <- stack(NDVI, monthR, elev, precip, tmax, tmin, MAP, MAT, srad, vp, dayl, spei1, spei6, spei12)
  rast_ext <- extent(rast_stack[[1]])
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "daylength", "spei1", "spei6","spei12"))
  #plot(rast_stack)
  
  RFC3 <- readRDS("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_3_11.rds")
  #Going to mask before I do this
  
  # Read SHAPEFILE.shp from the current working directory (".")
  GPP <- predict(rast_stack, RFC3, ext=rast_ext)
  #plot(GPP)
  mask <- raster("F:/Upscaling_Project/Test_Global_Upscaling/Drylands_dataset_2007/CBBNDrylands.tif")
  #plot(mask)
  mask[mask < 0.5] <- NA
  mask[mask >0.5] <-1
  #plot(mask)
  mask <- resample(mask, GPP, method="bilinear")
  GPP_drylands <- mask( GPP, mask)
  title <- paste("Upscaled GPP", moname, year, sep=" ")
  filenameF <- paste("F:/Upscaling_Project/Test_Global_Upscaling/", moname, year, ".tif", sep="_")
  writeRaster(GPP_drylands, filenameF, overwrite=TRUE)
  plot(GPP_drylands, ggtitle = title)  
} 


GlobalAnalysis(1191, 1, 2001, 01, "Jan", 07)
GlobalAnalysis(1192, 2, 2001, 02, "Feb", 08)
GlobalAnalysis(1193, 3, 2001, 03, "Mar", 09)
GlobalAnalysis(1194, 4, 2001, 04, "Aor", 10)
GlobalAnalysis(1195, 5, 2001, 05, "May", 11)
GlobalAnalysis(1196, 6, 2001, 06, "Jun", 12)
GlobalAnalysis(1197, 7, 2001, 07, "Jul", 01)
GlobalAnalysis(1198, 8, 2001, 08, "Aug", 02)
GlobalAnalysis(1199, 9, 2001, 09, "Sep", 03)
GlobalAnalysis(1200, 10, 2001, 10, "Oct", 04)
GlobalAnalysis(1201, 11, 2001, 11, "Nov", 05)
GlobalAnalysis(1202, 12, 2001, 12, "Dec", 06)

GlobalAnalysis(1203, 13, 2002, 01, "Jan", 07)
GlobalAnalysis(1204, 14, 2002, 02, "Feb", 08)
GlobalAnalysis(1205, 15, 2002, 03, "Mar", 09)
GlobalAnalysis(1206, 16, 2002, 04, "Aor", 10)
GlobalAnalysis(1207, 17, 2002, 05, "May", 11)
GlobalAnalysis(1208, 18, 2002, 06, "Jun", 12)
GlobalAnalysis(1209, 19, 2002, 07, "Jul", 01)
GlobalAnalysis(1210, 20, 2002, 08, "Aug", 02)
GlobalAnalysis(1211, 21, 2002, 09, "Sep", 03)
GlobalAnalysis(1212, 22, 2002, 10, "Oct", 04)
GlobalAnalysis(1213, 23, 2002, 11, "Nov", 05)
GlobalAnalysis(1214, 24, 2002, 12, "Dec", 06)

GlobalAnalysis(1215, 25, 2003, 01, "Jan", 07)
GlobalAnalysis(1216, 26, 2003, 02, "Feb", 08)
GlobalAnalysis(1217, 27, 2003, 03, "Mar", 09)
GlobalAnalysis(1218, 28, 2003, 04, "Aor", 10)
GlobalAnalysis(1219, 29, 2003, 05, "May", 11)
GlobalAnalysis(1220, 30, 2003, 06, "Jun", 12)
GlobalAnalysis(1221, 31, 2003, 07, "Jul", 01)
GlobalAnalysis(1222, 32, 2003, 08, "Aug", 02)
GlobalAnalysis(1223, 33, 2003, 09, "Sep", 03)
GlobalAnalysis(1224, 34, 2003, 10, "Oct", 04)
GlobalAnalysis(1225, 35, 2003, 11, "Nov", 05)
GlobalAnalysis(1226, 36, 2003, 12, "Dec", 06)

GlobalAnalysis(1227, 37, 2004, 01, "Jan", 07)
GlobalAnalysis(1228, 38, 2004, 02, "Feb", 08)
GlobalAnalysis(1229, 39, 2004, 03, "Mar", 09)
GlobalAnalysis(1230, 40, 2004, 04, "Aor", 10)
GlobalAnalysis(1231, 41, 2004, 05, "May", 11)
GlobalAnalysis(1232, 42, 2004, 06, "Jun", 12)
GlobalAnalysis(1233, 43, 2004, 07, "Jul", 01)
GlobalAnalysis(1234, 44, 2004, 08, "Aug", 02)
GlobalAnalysis(1235, 45, 2004, 09, "Sep", 03)
GlobalAnalysis(1236, 46, 2004, 10, "Oct", 04)
GlobalAnalysis(1237, 47, 2004, 11, "Nov", 05)
GlobalAnalysis(1238, 48, 2004, 12, "Dec", 06)

GlobalAnalysis(1239, 49, 2005, 01, "Jan", 07)
GlobalAnalysis(1240, 50, 2005, 02, "Feb", 08)
GlobalAnalysis(1241, 51, 2005, 03, "Mar", 09)
GlobalAnalysis(1242, 52, 2005, 04, "Aor", 10)
GlobalAnalysis(1243, 53, 2005, 05, "May", 11)
GlobalAnalysis(1244, 54, 2005, 06, "Jun", 12)
GlobalAnalysis(1245, 55, 2005, 07, "Jul", 01)
GlobalAnalysis(1246, 56, 2005, 08, "Aug", 02)
GlobalAnalysis(1247, 57, 2005, 09, "Sep", 03)
GlobalAnalysis(1248, 58, 2005, 10, "Oct", 04)
GlobalAnalysis(1249, 59, 2005, 11, "Nov", 05)
GlobalAnalysis(1250, 60, 2005, 12, "Dec", 06)

GlobalAnalysis(1251, 61, 2006, 01, "Jan", 07)
GlobalAnalysis(1252, 62, 2006, 02, "Feb", 08)
GlobalAnalysis(1253, 63, 2006, 03, "Mar", 09)
GlobalAnalysis(1254, 64, 2006, 04, "Aor", 10)
GlobalAnalysis(1255, 65, 2006, 05, "May", 11)
GlobalAnalysis(1256, 66, 2006, 06, "Jun", 12)
GlobalAnalysis(1257, 67, 2006, 07, "Jul", 01)
GlobalAnalysis(1258, 68, 2006, 08, "Aug", 02)
GlobalAnalysis(1259, 69, 2006, 09, "Sep", 03)
GlobalAnalysis(1260, 70, 2006, 10, "Oct", 04)
GlobalAnalysis(1261, 71, 2006, 11, "Nov", 05)
GlobalAnalysis(1262, 72, 2006, 12, "Dec", 06)

#GlobalAnalysis(1263, 73, 2007, 01, "Jan", 07)
#GlobalAnalysis(1264, 74, 2007, 02, "Feb", 08)
#GlobalAnalysis(1265, 75, 2007, 03, "Mar", 09)
#GlobalAnalysis(1266, 76, 2007, 04, "Aor", 10)
#GlobalAnalysis(1267, 77, 2007, 05, "May", 11)
#GlobalAnalysis(1268, 78, 2007, 06, "Jun", 12)
#GlobalAnalysis(1269, 79, 2007, 07, "Jul", 01)
#GlobalAnalysis(1270, 80, 2007, 08, "Aug", 02)
#GlobalAnalysis(1271, 81, 2007, 09, "Sep", 03)
#GlobalAnalysis(1272, 82, 2007, 10, "Oct", 04)
#GlobalAnalysis(1273, 83, 2007, 11, "Nov", 05)
#GlobalAnalysis(1274, 84, 2007, 12, "Dec", 06)

GlobalAnalysis(1275, 85, 2008, 01, "Jan", 07)
GlobalAnalysis(1276, 86, 2008, 02, "Feb", 08)
GlobalAnalysis(1277, 87, 2008, 03, "Mar", 09)
GlobalAnalysis(1278, 88, 2008, 04, "Aor", 10)
GlobalAnalysis(1279, 89, 2008, 05, "May", 11)
GlobalAnalysis(1280, 90, 2008, 06, "Jun", 12)
GlobalAnalysis(1281, 91, 2008, 07, "Jul", 01)
GlobalAnalysis(1282, 92, 2008, 08, "Aug", 02)
GlobalAnalysis(1283, 93, 2008, 09, "Sep", 03)
GlobalAnalysis(1284, 94, 2008, 10, "Oct", 04)
GlobalAnalysis(1285, 95, 2008, 11, "Nov", 05)
GlobalAnalysis(1286, 96, 2008, 12, "Dec", 06)

#GlobalAnalysis(1297, 97, 2009, 01, "Jan", 07)
#GlobalAnalysis(1298, 98, 2009, 02, "Feb", 08)
#GlobalAnalysis(1299, 99, 2009, 03, "Mar", 09)
#GlobalAnalysis(1300, 100, 2009, 04, "Aor", 10)
#GlobalAnalysis(1301, 101, 2009, 05, "May", 11)
#GlobalAnalysis(1302, 102, 2009, 06, "Jun", 12)
#GlobalAnalysis(1303, 103, 2009, 07, "Jul", 01)
#GlobalAnalysis(1304, 104, 2009, 08, "Aug", 02)
#GlobalAnalysis(1306, 105, 2009, 09, "Sep", 03)
#GlobalAnalysis(1306, 106, 2009, 10, "Oct", 04)
#GlobalAnalysis(1307, 107, 2009, 11, "Nov", 05)
#GlobalAnalysis(1308, 108, 2009, 12, "Dec", 06)

GlobalAnalysis(1309, 109, 2010, 01, "Jan", 07)
GlobalAnalysis(1310, 110, 2010, 02, "Feb", 08)
GlobalAnalysis(1311, 111, 2010, 03, "Mar", 09)
GlobalAnalysis(1312, 112, 2010, 04, "Aor", 10)
GlobalAnalysis(1313, 113, 2010, 05, "May", 11)
GlobalAnalysis(1314, 114, 2010, 06, "Jun", 12)
GlobalAnalysis(1315, 115, 2010, 07, "Jul", 01)
GlobalAnalysis(1316, 116, 2010, 08, "Aug", 02)
GlobalAnalysis(1317, 117, 2010, 09, "Sep", 03)
GlobalAnalysis(1318, 118, 2010, 10, "Oct", 04)
GlobalAnalysis(1395, 119, 2010, 11, "Nov", 05)
GlobalAnalysis(1320, 120, 2010, 12, "Dec", 06)

GlobalAnalysis2(1321, 1, 2011, 01, "Jan", 07)
GlobalAnalysis2(1322, 2, 2011, 02, "Feb", 08)
GlobalAnalysis2(1323, 3, 2011, 03, "Mar", 09)
GlobalAnalysis2(1324, 4, 2011, 04, "Aor", 10)
GlobalAnalysis2(1325, 5, 2011, 05, "May", 11)
GlobalAnalysis2(1326, 6, 2011, 06, "Jun", 12)
GlobalAnalysis2(1327, 7, 2011, 07, "Jul", 01)
GlobalAnalysis2(1328, 8, 2011, 08, "Aug", 02)
GlobalAnalysis2(1329, 9, 2011, 09, "Sep", 03)
GlobalAnalysis2(1330, 10, 2011, 10, "Oct", 04)
GlobalAnalysis2(1331, 11, 2011, 11, "Nov", 05)
GlobalAnalysis2(1332, 12, 2011, 12, "Dec", 06)

GlobalAnalysis2(1333, 13, 2012, 01, "Jan", 07)
GlobalAnalysis2(1334, 14, 2012, 02, "Feb", 08)
GlobalAnalysis2(1335, 15, 2012, 03, "Mar", 09)
GlobalAnalysis2(1336, 16, 2012, 04, "Aor", 10)
GlobalAnalysis2(1337, 17, 2012, 05, "May", 11)
GlobalAnalysis2(1338, 18, 2012, 06, "Jun", 12)
GlobalAnalysis2(1339, 19, 2012, 07, "Jul", 01)
GlobalAnalysis2(1340, 20, 2012, 08, "Aug", 02)
GlobalAnalysis2(1341, 21, 2012, 09, "Sep", 03)
GlobalAnalysis2(1342, 22, 2012, 10, "Oct", 04)
GlobalAnalysis2(1343, 23, 2012, 11, "Nov", 05)
GlobalAnalysis2(1344, 24, 2012, 12, "Dec", 06)

GlobalAnalysis2(1345, 25, 2013, 01, "Jan", 07)
GlobalAnalysis2(1346, 26, 2013, 02, "Feb", 08)
GlobalAnalysis2(1347, 27, 2013, 03, "Mar", 09)
GlobalAnalysis2(1348, 28, 2013, 04, "Aor", 10)
GlobalAnalysis2(1349, 29, 2013, 05, "May", 11)
GlobalAnalysis2(1350, 30, 2013, 06, "Jun", 12)
GlobalAnalysis2(1351, 31, 2013, 07, "Jul", 01)
GlobalAnalysis2(1352, 32, 2013, 08, "Aug", 02)
GlobalAnalysis2(1353, 33, 2013, 09, "Sep", 03)
GlobalAnalysis2(1354, 34, 2013, 10, "Oct", 04)
GlobalAnalysis2(1355, 35, 2013, 11, "Nov", 05)
GlobalAnalysis2(1356, 36, 2013, 12, "Dec", 06)

GlobalAnalysis2(1357, 37, 2014, 01, "Jan", 07)
GlobalAnalysis2(1358, 38, 2014, 02, "Feb", 08)
GlobalAnalysis2(1359, 39, 2014, 03, "Mar", 09)
GlobalAnalysis2(1360, 40, 2014, 04, "Aor", 10)
GlobalAnalysis2(1361, 41, 2014, 05, "May", 11)
GlobalAnalysis2(1362, 42, 2014, 06, "Jun", 12)
GlobalAnalysis2(1363, 43, 2014, 07, "Jul", 01)
GlobalAnalysis2(1364, 44, 2014, 08, "Aug", 02)
GlobalAnalysis2(1365, 45, 2014, 09, "Sep", 03)
GlobalAnalysis2(1366, 46, 2014, 10, "Oct", 04)
GlobalAnalysis2(1367, 47, 2014, 11, "Nov", 05)
GlobalAnalysis2(1368, 48, 2014, 12, "Dec", 06)


GlobalAnalysis2(1369, 49, 2015, 01, "Jan", 07)
GlobalAnalysis2(1370, 50, 2015, 02, "Feb", 08)
GlobalAnalysis2(1371, 51, 2015, 03, "Mar", 09)
GlobalAnalysis2(1372, 52, 2015, 04, "Aor", 10)
GlobalAnalysis2(1373, 53, 2015, 05, "May", 11)
GlobalAnalysis2(1374, 54, 2015, 06, "Jun", 12)
GlobalAnalysis2(1375, 55, 2015, 07, "Jul", 01)
GlobalAnalysis2(1376, 56, 2015, 08, "Aug", 02)
GlobalAnalysis2(1377, 57, 2015, 09, "Sep", 03)
GlobalAnalysis2(1378, 58, 2015, 10, "Oct", 04)
GlobalAnalysis2(1379, 59, 2015, 11, "Nov", 05)
GlobalAnalysis2(1380, 60, 2015, 12, "Dec", 06)

#OK compare strongest El Nino year in the data record with the strongest la nina year in the data record
#El Nino: 2015
#La Nina: 2010
#Add up CO2 and quantify the difference if possible
#months south vs. northern hemisphere: 
#Northern:  1  2   3  4   5   6  7  8  9 10  11 12
#Southern: 7   8  9  10  11  12  1  2 3  4  5  6
lst2010 <- list.files("F:/Upscaling_Project/Test_Global_Upscaling/", pattern="2010_.tif", full.names=TRUE)
stack2010 <- stack(lst2010)
plot(stack2010)
sum2010 <- calc(stack2010, sum, na.rm=TRUE) 
plot(sum2010)

lst2015 <- list.files("F:/Upscaling_Project/Test_Global_Upscaling/", pattern="2015_.tif", full.names=TRUE)
stack2015 <- stack(lst2015)
plot(stack2015)
sum2015 <- calc(stack2015, sum, na.rm=TRUE) 
plot(sum2015)
diff <- sum2015 - sum2010
percentdiff <- 100*((sum2015-sum2010)/(sum2015))
percentdiff[percentdiff==-Inf] <- NA
plot(percentdiff)
names(diff) <- "Difference"
cellStats(sum2010, sum)
cellStats(sum2015, sum)

summary(percentdiff)

#function from here: https://stackoverflow.com/questions/33750235/plotting-a-raster-with-the-color-ramp-diverging-around-zero
diverge0 <- function(p, ramp) {
  # p: a trellis object resulting from rasterVis::levelplot
  # ramp: the name of an RColorBrewer palette (as character), a character 
  #       vector of colour names to interpolate, or a colorRampPalette.
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(brewer.pal(11, ramp)))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(1000)[zlim[-length(zlim)]]
  p
}
library(rasterVis)
library(maps)
library(mapdata)
library(mapplots)
library(maptools)
library(rasterImage)
library(ggplot2)
library(sp)
library(grid)
library(lattice)
percentdiff

#Have to divide by 1000 to avoid integer overflow problem
neg <- sum(Which(percentdiff <0, cells=TRUE,  na.rm=TRUE))/1000
pos <- sum(Which(percentdiff >0, cells=TRUE,  na.rm=TRUE))/1000
slices <- c(neg, pos)
lbls <- c("Negative", "Positive")
colors=c("black", "white")
pie(slices, labels=lbls, col=colors)

#p <- levelplot(diff)
#diverge0(p, "RdYlBu")
mapTheme <- rasterTheme(region = brewer.pal(8, "RdBu"))
plot <- levelplot(percentdiff, margin=F, par.settings=mapTheme, at=seq(-125, 125, length.out=40), main="% change in GPP: 2015 vs. 2010")
plot + layer(sp.lines(worldSHP, lwd=0.8, col='gray'))
trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid.text(("% change \n in CO2 uptake"), 0.2, 0, hjust=0.5, vjust=1)
trellis.unfocus()

proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
worldSHP <- shapefile("F:/Upscaling_Project/Test_Global_Upscaling/TM_WORLD_BORDERS-0.3.shp")



require(colorRamps)
col5 <- colorRampPalette(c('red', 'cornsilk', 'blue'))  #create color ramp starting from blue to red
color_levels=10 #the number of colors to use
max_absolute_value=4 #what is the maximum absolute value of raster?
plot(diff, col=col5(n=color_levels), breaks=seq(-max_absolute_value,max_absolute_value,length.out=color_levels+1), axes=FALSE)

#The half-degree compraisons russ wanted
current.list <- list.files(path="D:/Upscaling_Project/Test_Global_Upscaling", 
                           pattern ="_.tif$", full.names=TRUE)
stack_FUF <- stack(current.list)
plot(stack_FUF[1])
FUFpoint <- cbind(-111.76, 35.0890)
FUFresultA1 <-extract(stack_FUF, FUFpoint)
str(FUFresultA1)
FUFresultA2 <- as.data.frame(FUFresultA1)
str(FUFresultA2)
df <- cbind(names = rownames(FUFresultA2), FUFresultA2)
library(reshape2)
long <- melt(df, id.vars = c("names"))
colnames(long) <- c("X", "name", "GPP")
long$X <- NULL
long$month <- substr(long$name, 3,5)
long$year <- substr(long$name, 7,10)
long$date <-  date <-as.Date(paste(long$year, long$month, "01", sep="-"), format="%Y-%b-%d")
str(long)
write.csv(long, "D:/Upscaling_Project/Test_Global_Upscaling/FUF_0.5_deg.csv")


#US-Wkg
WKGpoint <- cbind(-109.94, 31.7365)
WKGresult <-extract(stack_FUF, WKGpoint)
plot(WKGresult[1:12])
detach("package:tibble", unload=TRUE)
#TO DO 
#remake 3 peaked graphs JUST for Kendall and Flagstaff unmanaged forest
#You got this


#Global validation
#1) Stack up global all years
library(raster)
current.list <- list.files(path="F:/Upscaling_Project/Test_Global_Upscaling", 
                           pattern ="_.tif$", full.names=TRUE)
stack_val <- stack(current.list)
#2) Extract Point around Ausflux sites, etc. 
Drypoint <- cbind(132.3706, -15.2588)
DryresultA1 <-extract(stack_val, Drypoint)
str(DryresultA1)
DryresultA2 <- as.data.frame(DryresultA1)
str(DryresultA2)
df <- cbind(names = rownames(DryresultA2), DryresultA2)
library(reshape2)
long <- melt(df, id.vars = c("names"))
colnames(long) <- c("X", "name", "GPP")
long$X <- NULL
long$month <- substr(long$name, 3,5)
long$year <- substr(long$name, 7,10)
long$date <-  date <-as.Date(paste(long$year, long$month, "01", sep="-"), format="%Y-%b-%d")
str(long)
write.csv(long, "F:/Upscaling_Project/Test_Global_Upscaling/Dry_0.5_deg.csv")
#3)Merge with flux data 
Dry <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/Validation/FLX_AU-Dry_FLUXNET2015_SUBSET_MM_2008-2014_2-3.csv")
Flux <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/Dry_0.5_deg.csv")
str(Dry)
Dry$month <- substr(Dry$TIMESTAMP, 5,6)
Dry$year <-substr(Dry$TIMESTAMP, 1,4)
Dry$date <- as.Date(paste(Dry$year, Dry$month, "01", sep="-"))
Flux$date <- as.Date(Flux$date)
Dry$FluxGPP <- Dry$GPP_DT_VUT_REF
Flux$DryFluxGPP <- Flux$GPP
str(Dry)
str(Flux)
Aus_Dry_Val <- merge(Dry, Flux, by="date")
#4) plot!
library(ggplot2)
library(ggthemes)
library(scales)
library(psych)
library(ggpubr)
#  B<- as.character(round(cor(x$GPP, x$Barnes_GPP, use="complete.obs"), 2))
#  J<- as.character(round(cor(x$GPP, x$Jung_GPP, use="complete.obs"), 2))
  
#  print("calculated cors")
#  rmssdGPP <- as.character(round(rmssd(x$GPP), 2))
#  rmssdB <- as.character(round(rmssd(x$Barnes_GPP), 2))
#  rmssdJ <- as.character(round(rmssd(x$Jung_GPP), 2))
  
#  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
#  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  
#  print("got RMSSD")
#  lblGPP <- paste("RMSSDObserved =", rmssdGPP)
#  lblB <- paste("rmseDryFlux =", rmseBarnes, ",r=", B, ",RMSSD=",rmssdB)
#  lblJ <- paste("rmseFluxcom =", rmseJung, ",r=", J, ",RMSSD=",rmssdJ)
  
#  filename <- paste(x$site[1], "seasonal_comparison_3_29.png", sep="_")
#  print(filename)
  q <- ggplot() +
    ggtitle("Global Validation - AUS-Dry")+
    geom_line(data = Aus_Dry_Val, aes(x = date, y = FluxGPP, color =I("#BD2031")), size=2) +
    geom_line(data = Aus_Dry_Val, aes(x = date, y = DryFluxGPP, color = I("#5F20BD")), size=2) +
    #geom_errorbar(data=x,aes(x=month, ymin=Barnes_GPP- Barnes_GPP_se, ymax=Barnes_GPP+Barnes_GPP_se),colour="#5F20BD")+
    #geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#BD2031")+
    #geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    #annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    #annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    #annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    #scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =12)+
    theme(legend.position = c(0, 0))
  plot(q)

  
  library(plyr)
  plot_seasonal_cycle <- ddply(Aus_Dry_Val, .(month.x), summarize, DryFluxGPP_se=sd(DryFluxGPP, na.rm=TRUE)/sqrt(length(DryFluxGPP[!is.na(DryFluxGPP)])), DryFluxGPP=mean(DryFluxGPP, na.rm=TRUE),
                               FluxGPP_se=sd(FluxGPP, na.rm=TRUE)/sqrt(length(FluxGPP[!is.na(FluxGPP)])), FluxGPP=mean(FluxGPP, na.rm=TRUE))

  str(plot_seasonal_cycle)
  plot_seasonal_cycle$month.x <- as.numeric(plot_seasonal_cycle$month.x)
  p <- ggplot() +
    ggtitle("Global Validation - AUS-Dry")+
    geom_line(data = plot_seasonal_cycle, aes(x = month.x, y = FluxGPP, color =I("#BD2031")), size=2) +
    geom_line(data = plot_seasonal_cycle, aes(x = month.x, y = DryFluxGPP, color = I("#5F20BD")), size=2) +
    geom_errorbar(data=plot_seasonal_cycle,aes(x=month.x, ymin=DryFluxGPP- DryFluxGPP_se, ymax=DryFluxGPP+DryFluxGPP_se),colour="#5F20BD")+
    geom_errorbar(data=plot_seasonal_cycle,aes(x=month.x, ymin=FluxGPP-FluxGPP_se,ymax=FluxGPP+FluxGPP_se),colour="#BD2031")+
    #geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    #annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    #annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    #annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =18)+
    theme(legend.position = c(0, 0))
  plot(p)
  
  
  #For STP----------
  library(raster)
  current.list <- list.files(path="F:/Upscaling_Project/Test_Global_Upscaling", 
                             pattern ="_.tif$", full.names=TRUE)
  stack_val <- stack(current.list)
  #2) Extract Point around Ausflux sites, etc. 
  Drypoint <- cbind(133.3502, -17.1507)
  DryresultA1 <-extract(stack_val, Drypoint)
  str(DryresultA1)
  DryresultA2 <- as.data.frame(DryresultA1)
  str(DryresultA2)
  df <- cbind(names = rownames(DryresultA2), DryresultA2)
  library(reshape2)
  long <- melt(df, id.vars = c("names"))
  colnames(long) <- c("X", "name", "GPP")
  long$X <- NULL
  long$month <- substr(long$name, 3,5)
  long$year <- substr(long$name, 7,10)
  long$date <-  date <-as.Date(paste(long$year, long$month, "01", sep="-"), format="%Y-%b-%d")
  str(long)
  write.csv(long, "F:/Upscaling_Project/Test_Global_Upscaling/Stp_0.5_deg.csv")
  #3)Merge with flux data 
  Dry <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/Validation/FLX_AU-Stp_FLUXNET2015_SUBSET_MM_2008-2014_1-3.csv")
  Flux <- read.csv("F:/Upscaling_Project/Test_Global_Upscaling/Dry_0.5_deg.csv")
  str(Dry)
  Dry$month <- substr(Dry$TIMESTAMP, 5,6)
  Dry$year <-substr(Dry$TIMESTAMP, 1,4)
  Dry$date <- as.Date(paste(Dry$year, Dry$month, "01", sep="-"))
  Flux$date <- as.Date(Flux$date)
  Dry$FluxGPP <- Dry$GPP_DT_VUT_REF
  Flux$DryFluxGPP <- Flux$GPP
  str(Dry)
  str(Flux)
  Aus_Stp_Val <- merge(Dry, Flux, by="date")
  #4) plot!
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(psych)
  library(ggpubr)
  #  B<- as.character(round(cor(x$GPP, x$Barnes_GPP, use="complete.obs"), 2))
  #  J<- as.character(round(cor(x$GPP, x$Jung_GPP, use="complete.obs"), 2))
  
  #  print("calculated cors")
  #  rmssdGPP <- as.character(round(rmssd(x$GPP), 2))
  #  rmssdB <- as.character(round(rmssd(x$Barnes_GPP), 2))
  #  rmssdJ <- as.character(round(rmssd(x$Jung_GPP), 2))
  
  #  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  #  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  
  #  print("got RMSSD")
  #  lblGPP <- paste("RMSSDObserved =", rmssdGPP)
  #  lblB <- paste("rmseDryFlux =", rmseBarnes, ",r=", B, ",RMSSD=",rmssdB)
  #  lblJ <- paste("rmseFluxcom =", rmseJung, ",r=", J, ",RMSSD=",rmssdJ)
  
  #  filename <- paste(x$site[1], "seasonal_comparison_3_29.png", sep="_")
  #  print(filename)
  r <- ggplot() +
    ggtitle("Global Validation - AUS-Stp")+
    geom_line(data = Aus_Stp_Val, aes(x = date, y = FluxGPP, color =I("#BD2031")), size=2) +
    geom_line(data = Aus_Stp_Val, aes(x = date, y = DryFluxGPP, color = I("#5F20BD")), size=2) +
    #geom_errorbar(data=x,aes(x=month, ymin=Barnes_GPP- Barnes_GPP_se, ymax=Barnes_GPP+Barnes_GPP_se),colour="#5F20BD")+
    #geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#BD2031")+
    #geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    #annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    #annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    #annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    #scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =18)+
    theme(legend.position = c(0, 0))
  plot(r)
  
  
  library(plyr)
  plot_seasonal_cycle2 <- ddply(Aus_Stp_Val, .(month.x), summarize, DryFluxGPP_se=sd(DryFluxGPP, na.rm=TRUE)/sqrt(length(DryFluxGPP[!is.na(DryFluxGPP)])), DryFluxGPP=mean(DryFluxGPP, na.rm=TRUE),
                               FluxGPP_se=sd(FluxGPP, na.rm=TRUE)/sqrt(length(FluxGPP[!is.na(FluxGPP)])), FluxGPP=mean(FluxGPP, na.rm=TRUE))
  
  str(plot_seasonal_cycle2)
  plot_seasonal_cycle2$month.x <- as.numeric(plot_seasonal_cycle2$month.x)
  s <- ggplot() +
    ggtitle("Global Validation - AUS-Stp")+
    geom_line(data = plot_seasonal_cycle2, aes(x = month.x, y = FluxGPP, color =I("#BD2031")), size=2) +
    geom_line(data = plot_seasonal_cycle2, aes(x = month.x, y = DryFluxGPP, color = I("#5F20BD")), size=2) +
    geom_errorbar(data=plot_seasonal_cycle2,aes(x=month.x, ymin=DryFluxGPP- DryFluxGPP_se, ymax=DryFluxGPP+DryFluxGPP_se),colour="#5F20BD")+
    geom_errorbar(data=plot_seasonal_cycle2,aes(x=month.x, ymin=FluxGPP-FluxGPP_se,ymax=FluxGPP+FluxGPP_se),colour="#BD2031")+
    #geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    #annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    #annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    #annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =18)+
    theme(legend.position = c(0, 0))
  plot(s)
  
  
plot_seasonal_cycles2_scaled <- scale(plot_seasonal_cycle2[,2:5])
str(plot_seasonal_cycles2_scaled)  
