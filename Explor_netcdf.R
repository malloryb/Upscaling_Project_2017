library(ncdf4)
library(raster)
library(reshape2)
library(dplyr)
library(chron)
library(lattice)
library(RColorBrewer)

#"Using this as a template:
#http://geog.uoregon.edu/GeogR/topics/netCDF-read-ncdf4.html
#http://geog.uoregon.edu/GeogR/topics/netCDF-dataframe.html

setwd("C:/Users/rsstudent/Upscaling_Data/Jung_2011")
#retrieve a list of nc files in folder
flist <- list.files(pattern= "^.*\\.(nc|NC|Nc|Nc)$")
#Open to first file in our list
nc <- nc_open(paste0(flist[1]))
print(nc)

ncname <-  "2017315214914EnsembleGPP_GL"
ncfname <- paste(ncname, ".nc", sep="")
dname <- "gpp"
# Get a list of the NetCDF's R attributes:
attributes(nc)$names
# Get a list of the nc variable names.
attributes(nc$var)$names
# Take a look at the GPP variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[4])

print(nc)

#Get lat and long
lon <- ncvar_get (nc, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc, "lat", verbose=F)
nlat <- dim((lat))
head(lat)

print(c(nlon,nlat))

#Get time variable together
t <- ncvar_get(nc, "time")
t

nt <- dim(t)
nt

#Units of time: dats since 1582-10-14

tunits <- ncatt_get(nc, "time", "units")

#Get input variable (gp) and its attributes and verify the size of the array
gpp_array <- ncvar_get(nc, gpp)
dlname <- ncatt_get(nc, gpp, "long_name")
dunits <- ncatt_get(nc, gpp, "units")
fillvalue <- ncatt_get(nc, gpp, "_FillValue")


#Get global attributes
title <- ncatt_get(nc, 0, "title")
institution <- ncatt_get(nc, 0,"institution")
datasource <- ncatt_get(nc ,0,"source")
references <- ncatt_get(nc,0,"references")
history <- ncatt_get(nc,0,"history")
Conventions <- ncatt_get(nc,0,"Conventions")

#List files
ls()

#Convert Netcdf into dataframes so I can actually understand what's going on here
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth=as.integer(unlist(tdstr)[2])
tday=as.integer(unlist(tdstr)[3])
tyear=as.integer(unlist(tdstr)[1])
chron(t,origin=c(tmonth, tday, tyear))

#replace netCDF varaible's fill values with R NA's
gpp_array[gpp_array==fillvalue$value] <-NA

#This is the number of non-missing grid cells (i.e. land cells)
length(na.omit(as.vector(gpp_array[,,1])))

#Get single slice of the data, create an R data frame, and write a .csv file
m <- 1
gpp_slice <- gpp_array[,,m]
#dimensions should be 720 by 360 rows
#verify dimensions
dim(gpp_slice)
gpp_slice
lat
lon

image(lon,lat, gpp_slice, col=rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-1,-0.5,0,0.5, 1)
levelplot(gpp_slice ~ lon*lat, data=grid, at=cutpts, cuts=4, pretty=T, col.regions=(rev(brewer.pal(10,"YlGnBu"))))

#Still trying to extract time series
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
#vector of 'gpp' values

gpp_vec <- as.vector(gpp_slice)
length(gpp_vec)

gpp_df01 <- data.frame(cbind(lonlat, gpp_vec))
names(gpp_df01) <- c("lon", "lat", paste(dname, as.character(m), sep="_"))
head(na.omit(gpp_df01), 10)

csvfile <- "GL_GPP_1.csv"
write.table(na.omit(gpp_df01), csvfile, row.names=FALSE, sep=",")

#Convert the whole array to a dataframe----------------
gpp_vec_long <- as.vector(gpp_array)
length(gpp_vec_long)

#reshape vector into 2592000x360 (months) matrix using matrix() function and verify dimensions
#360 comes from: 
#Goes from May 1982 to May 2012 - 12 months per year * 30 years = 360
#For the ones from the 2017 dataset they will be 12 apiece for the monthly

gpp_mat <- matrix(gpp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(gpp_mat)

head(na.omit(gpp_mat))

lonlat <- as.matrix(expand.grid(lon,lat))
gpp_df02 <- data.frame(cbind(lonlat, gpp_mat))
names(gpp_df02) <- c("lon", "lat", "May1982")
head(na.omit(gpp_df02, 20))

#Can get annual mean, variance, etc. here
#eg. gpp_df02$mtwa <- apply(gpp_df02[3:14], 1, max)

dim(na.omit(gpp_df02))
csvfile <- "GL_GPP_2.csv"
write.table(na.omit(gpp_df02), csvfile,row.names=FALSE, sep=",")
