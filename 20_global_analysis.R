#Creating global map
library(gdal)
library(gdalUtils)
library(raster)
library(stringr)
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
  plot(monthR)
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
plot(R)
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
plot(rast_stack)

RFC3 <- readRDS("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_3_11.rds")
varImp(RFC3)
varImpPlot(RFC3)
#Going to mask before I do this

# Read SHAPEFILE.shp from the current working directory (".")
GPP <- predict(rast_stack, RFC3, ext=rast_ext)
plot(GPP)
mask <- raster("F:/Upscaling_Project/Test_Global_Upscaling/Drylands_dataset_2007/CBBNDrylands.tif")
#plot(mask)
mask[mask < 0.5] <- NA
mask[mask >0.5] <-1
#plot(mask)
mask <- resample(mask, GPP, method="bilinear")
GPP_drylands <- mask( GPP, mask)
title <- paste("Upscaled GPP", moname, year, sep=" ")
filenameF <- paste("F:/Upscaling_Project/Test_Global_Upscaling/", moname, year, ".tif", sep="_")
writeRaster(GPP_drylands, filenameF)
plot(GPP_drylands, ggtitle = title)  
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
  plot(monthR)
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
  plot(R)
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
  plot(rast_stack)
  
  RFC3 <- readRDS("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Project_2017/RF_C3oversample_3_11.rds")
  #Going to mask before I do this
  
  # Read SHAPEFILE.shp from the current working directory (".")
  GPP <- predict(rast_stack, RFC3, ext=rast_ext)
  plot(GPP)
  mask <- raster("F:/Upscaling_Project/Test_Global_Upscaling/Drylands_dataset_2007/CBBNDrylands.tif")
  #plot(mask)
  mask[mask < 0.5] <- NA
  mask[mask >0.5] <-1
  #plot(mask)
  mask <- resample(mask, GPP, method="bilinear")
  GPP_drylands <- mask( GPP, mask)
  title <- paste("Upscaled GPP", moname, year, sep=" ")
  filenameF <- paste("F:/Upscaling_Project/Test_Global_Upscaling/", moname, year, ".tif", sep="_")
  writeRaster(GPP_drylands, filenameF)
  plot(GPP_drylands, ggtitle = title)  
} 

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
GlobalAnalysis(1319, 119, 2010, 11, "Nov", 05)
GlobalAnalysis(1320, 120, 2010, 12, "Dec", 06)

GlobalAnalysis2(1369, 59, 2015, 01, "Jan", 07)
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
plot(diff, col=col5(n=color_levels), breaks=seq(-max_absolute_value,max_absolute_value,length.out=color_levels+1) , axes=FALSE)
