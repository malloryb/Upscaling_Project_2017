library(ncdf4)
library(raster)
library(reshape2)
library(dplyr)
library(chron)
library(lattice)
library(RColorBrewer)

#"Using this as a template:
#https://www.r-bloggers.com/a-netcdf-4-in-r-cheatsheet/"

setwd("C:/Users/rsstudent/Upscaling_Data/Jung_2011")
#retrieve a list of nc files in folder
flist <- list.files(pattern= "^.*\\.(nc|NC|Nc|Nc)$")
#Open to first file in our list
nc <- nc_open(paste0(flist[1]))
print(nc)


# Save the print(nc) dump to a text file (same name as the nc file with a txt extension)
{
  sink(paste0("data/", flist[1], ".txt"))
  print(nc)
  sink()
}

# Get a list of the NetCDF's R attributes:
attributes(nc)$names
# Get a list of the nc variable names.
attributes(nc$var)$names
# Take a look at the GPP variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[4])

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


ls()

#Convert Netcdf into dataframes so I can actually understand what's going on here




gpp <- attributes(nc$var)$names[4]
ncvar_get(nc, gpp)
# Retrieve a matrix of the GPP data using the ncvar_get function:
GPP_mean <- ncvar_get(nc, attributes(nc$var)$names[4])


# Print the data's dimensions
dim(GPP_mean)

#Retrieve lat long values
attributes(nc$dim)$names

nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[3])
nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[1])

print(paste(dim(nc_lat), "latitudes and", dim(nc_lon), "longitudes"))

#Global attributes
nc_atts <- ncatt_get(nc,0)
names(nc_atts)
