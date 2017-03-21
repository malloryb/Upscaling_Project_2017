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
setwd("C:/Users/rsstudent/Upscaling_Data/FluxCom_Monthly/")
#retrieve a list of nc files in folder
flist <- list.files(path="GPP_Reichstein", pattern= "^.*\\.(nc|NC|Nc|Nc)$")
#Open to first file in our list
process_nc <- function(files){
  #iterate through the nc
  for (i in 1: length(files)){
    #open a connection to the ith nc file
    nc_tmp <- nc_open(paste0("GPP_Reichstein/", files[i]))
    #store values from variables and attributes
    gpp_array <- ncvar_get(nc, "GPP")
    print(gpp_array)
    dlname <- ncatt_get(nc, "GPP", "long_name")
    dunits <- ncatt_get(nc, "GPP", "units")
    fillvalue <- ncatt_get(nc, "GPP", "_FillValue")
    print(fillvalue)
    nc_lat <- ncvar_get(nc_tmp, "lat", verbose=F)
    nc_lon <- ncvar_get (nc_tmp, "lon")
    nlon <- dim(nc_lon)
    nlat <- dim((nc_lat))
    t <- ncvar_get(nc_tmp, "time")
    nt <- dim(t)
    #Units of time: dats since 1582-10-14
    tunits <- ncatt_get(nc_tmp, "time", "units")
    #Get input variable (gpp) and its attributes and verify the size of the array
  #Convert Netcdf into dataframes so I can actually understand what's going on here
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth=as.integer(unlist(tdstr)[2])
tday=as.integer(unlist(tdstr)[3])
tyear=as.integer(unlist(tdstr)[1])
chron(t,origin=c(tmonth, tday, tyear))
#replace netCDF varaible's fill values with R NA's
#gpp_array[gpp_array==fillvalue$value] <-NA
#This is the number of non-missing grid cells (i.e. land cells)
length(na.omit(as.vector(gpp_array[,,1])))
#Get GPP Slice
m <- 1
gpp_slice <- gpp_array[,,m]
#Still trying to extract time series
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)
#vector of 'gpp' values
gpp_vec <- as.vector(gpp_slice)
#Convert the whole array to a dataframe----------------
gpp_vec_long <- as.vector(gpp_array)
length(gpp_vec_long)
#reshape vector into 2592000x360 (months) matrix using matrix() function and verify dimensions
gpp_mat <- matrix(gpp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(gpp_mat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_gpp_df <- data.frame(cbind(lonlat, gpp_mat))
# set the name of my new variable and bind the new data to it
if (exists("gpp_data_monthly")){
  gpp_data_monthly <- bind_cols(gpp_data_monthly, tmp_gpp_df[,-(1:2)])
}else{
  gpp_data_monthly <- tmp_gpp_df
}
}
return(gpp_data_monthly)
}

data <- process_nc(flist)



names(data) <- c("lon", "lat", "Jan1980", "Feb1980", "Mar1980", "Apr1980", "May1980", "Jun1980", "Jul1980", "Aug1980", "Sep1980", "Oct1980", "Nov1980", "Dec1980")
head(na.omit(gpp_df02, 20))
dim(na.omit(gpp_df02))
csvfile <- "MARS_GPP_all_years.csv"
write.table(na.omit(data), csvfile,row.names=FALSE, sep=",")



#If you want to extract a slice--------------------
#Get single slice of the data, create an R data frame, and write a .csv file
m <- 1
gpp_slice <- gpp_array[,,m]
#dimensions should be 720 by 360 rows
#verify dimensions
dim(gpp_slice)
gpp_slice
lat
lon

#image(lon,lat, gpp_slice, col=rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(0,0.00000001,0.00000002, 0.00000004, 0.00000006, 0.0000001)
levelplot(gpp_slice ~ lon*lat, data=grid, at=cutpts, cuts=6, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))))


