#Code to extract data from MODIS .hdf files 
#Using te following website: http://www.khufkens.com/2016/04/20/modis-hdf-data-extraction-in-r/

library(raster)
library(sp)
library(MODIS)
library(gdalUtils)
library(rgdal)

#read in all SDS strings
#using the MODIS package
#For 1 grid file: 

#This is taking awhile

gdalinfo("C:/Users/rsstudent/odrive/UA_Google_Drive/Upscaling_Data/MOD13A1_Col6/MOD13A1.A2000049.h07v06.006.2015136104433.hdf")

sds <- get_subdatasets("C:/Users/rsstudent/odrive/UA_Google_Drive/Upscaling_Data/MOD13A1_Col6/MOD13A1.A2000049.h07v06.006.2015136104433.hdf")
sds[1]

my_hdf_layer <- raster(readGDAL(sds[1], as.is=TRUE))

getSds(HdfName="C:/Users/rsstudent/odrive/UA_Google_Drive/Upscaling_Data/MOD13A1_Col6/MOD13A1.A2000049.h07v06.006.2015136104433.hdf")

#Get layer x of an hdf file

my_hdf_layer <- raster(readGDAL(sds$SDS4gdal[4], as.is. = TRUE))
