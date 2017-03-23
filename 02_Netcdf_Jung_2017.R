library(ncdf4)
library(raster)
library(reshape2)
library(dplyr)
library(chron)
library(lattice)
library(RColorBrewer)

#"Using this as a template:
#http://geog.uoregon.edu/GeogR/topics/netCDF_read_ncdf4.html
#http://geog.uoregon.edu/GeogR/topics/netCDF_dataframe.html
setwd("C:/Users/rsstudent/Upscaling_Data/FluxCom_Monthly/")
#retrieve a list of nc files in folder
flist <- list.files(path="GPP_Reichstein", pattern= "^.*\\.(nc|NC|Nc|Nc)$")
tlist <- flist[1:2]
nc <- nc_open(paste0(flist[4]))

#ncname <-  "2017315214914EnsembleGPP_MR"
#ncfname <- paste(ncname, ".nc", sep="")
dname <- "gpp"
# Get a list of the NetCDF's R attributes:
attributes(nc)$names
# Get a list of the nc variable names.
attributes(nc$var)$names
# Take a look at the GPP variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[4])

print(nc)

setwd("C:/Users/rsstudent/Upscaling_Data/FluxCom_Monthly/GPP_Reichstein/")
process_nc1 <- function(f){
    nc_tmp <- nc_open(f)
    #store values from variables and attributes
    gpp_array <- ncvar_get(nc_tmp, "GPP")
    print(gpp_array)
    dlname <- ncatt_get(nc_tmp, "GPP", "long_name")
    fillvalue <- ncatt_get(nc_tmp, "GPP", "_FillValue")
    nc_lat <- ncvar_get(nc_tmp, "lat", verbose=F)
    nc_lon <- ncvar_get (nc_tmp, "lon")
    nlon <- dim(nc_lon)
    nlat <- dim((nc_lat))
    t <- ncvar_get(nc_tmp, "time")
    nt <- dim(t)
    #Units of time: dats since 1582_2011_14
    tunits <- ncatt_get(nc_tmp, "time", "units")
  #Get input variable (gpp) and its attributes and verify the size of the array
  #Convert Netcdf into dataframes so I can actually understand what's going on here
  #tustr <- strsplit(tunits$value, " ")
  #tdstr <- strsplit(unlist(tustr)[3], "_")
  #tmonth=as.integer(unlist(tdstr)[2])
  #tday=as.integer(unlist(tdstr)[3])
  #tyear=as.integer(unlist(tdstr)[1])
  #chron(t,origin=c(tmonth, tday, tyear))
  #replace netCDF varaible's fill values with R NA's
  gpp_array[gpp_array==fillvalue$value] <-NA
  #This is the number of non_missing grid cells (i.e. land cells)
  #length(na.omit(as.vector(gpp_array[,,1])))
  #Get GPP Slice
  #m <- 1
  #gpp_slice <- gpp_array[,,m]
  #Still trying to extract time series
  #lonlat <- as.matrix(expand.grid(lon,lat))
  #dim(lonlat)
  #vector of 'gpp' values
  #gpp_vec <- as.vector(gpp_slice)
  #Convert the whole array to a dataframe
  gpp_vec_long <- as.vector(gpp_array)
  length(gpp_vec_long)
  #reshape vector into 2592000x360 (months) matrix using matrix() function and verify dimensions
  gpp_mat <- matrix(gpp_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(gpp_mat)
  lonlat <- as.matrix(expand.grid(lon,lat))
  print(gpp_mat)
  gpp_df <- data.frame(cbind(lonlat, gpp_mat))
return(gpp_df)
}

tmp <- lapply(flist, process_nc1) %>% bind_cols()
str(tmp)
data <- tmp[!duplicated(as.list(tmp))]
str(data)

colnames(data) <- c("lon", "lat","Jan_1980", "Feb_1980","Mar_1980","Apr_1980","May_1980","Jun_1980",
                    "Jul_1980","Aug_1980","Sep_1980","Oct_1980","Nov_1980","Dec_1980","Jan_1981","Feb_1981","Mar_1981","Apr_1981","May_1981","Jun_1981","Jul_1981","Aug_1981","Sep_1981","Oct_1981","Nov_1981","Dec_1981","Jan_1982","Feb_1982","Mar_1982","Apr_1982","May_1982","Jun_1982","Jul_1982","Aug_1982","Sep_1982","Oct_1982","Nov_1982","Dec_1982","Jan_1983","Feb_1983","Mar_1983","Apr_1983","May_1983","Jun_1983","Jul_1983","Aug_1983","Sep_1983","Oct_1983","Nov_1983","Dec_1983","Jan_1984","Feb_1984","Mar_1984","Apr_1984","May_1984","Jun_1984","Jul_1984","Aug_1984","Sep_1984","Oct_1984","Nov_1984","Dec_1984","Jan_1985","Feb_1985","Mar_1985","Apr_1985","May_1985","Jun_1985","Jul_1985","Aug_1985","Sep_1985","Oct_1985","Nov_1985","Dec_1985","Jan_1986","Feb_1986","Mar_1986","Apr_1986","May_1986","Jun_1986","Jul_1986","Aug_1986","Sep_1986","Oct_1986","Nov_1986","Dec_1986","Jan_1987","Feb_1987","Mar_1987","Apr_1987","May_1987","Jun_1987","Jul_1987","Aug_1987","Sep_1987","Oct_1987","Nov_1987","Dec_1987","Jan_1988","Feb_1988","Mar_1988","Apr_1988","May_1988","Jun_1988","Jul_1988","Aug_1988","Sep_1988","Oct_1988","Nov_1988","Dec_1988","Jan_1989","Feb_1989","Mar_1989","Apr_1989","May_1989","Jun_1989","Jul_1989","Aug_1989","Sep_1989","Oct_1989","Nov_1989","Dec_1989","Jan_1990","Feb_1990","Mar_1990","Apr_1990","May_1990","Jun_1990","Jul_1990","Aug_1990","Sep_1990","Oct_1990","Nov_1990","Dec_1990","Jan_1991","Feb_1991","Mar_1991","Apr_1991","May_1991","Jun_1991","Jul_1991","Aug_1991","Sep_1991","Oct_1991","Nov_1991","Dec_1991","Jan_1992","Feb_1992","Mar_1992","Apr_1992","May_1992","Jun_1992","Jul_1992","Aug_1992","Sep_1992","Oct_1992","Nov_1992","Dec_1992","Jan_1993","Feb_1993","Mar_1993","Apr_1993","May_1993","Jun_1993","Jul_1993","Aug_1993","Sep_1993","Oct_1993","Nov_1993","Dec_1993","Jan_1994","Feb_1994","Mar_1994","Apr_1994","May_1994","Jun_1994","Jul_1994","Aug_1994","Sep_1994","Oct_1994","Nov_1994","Dec_1994","Jan_1995","Feb_1995","Mar_1995","Apr_1995","May_1995","Jun_1995","Jul_1995","Aug_1995","Sep_1995","Oct_1995","Nov_1995","Dec_1995","Jan_1996","Feb_1996","Mar_1996","Apr_1996","May_1996","Jun_1996","Jul_1996","Aug_1996","Sep_1996","Oct_1996","Nov_1996","Dec_1996","Jan_1997","Feb_1997","Mar_1997","Apr_1997","May_1997","Jun_1997","Jul_1997","Aug_1997","Sep_1997","Oct_1997","Nov_1997","Dec_1997","Jan_1998","Feb_1998","Mar_1998","Apr_1998","May_1998","Jun_1998","Jul_1998","Aug_1998","Sep_1998","Oct_1998","Nov_1998","Dec_1998","Jan_1999","Feb_1999","Mar_1999","Apr_1999","May_1999","Jun_1999","Jul_1999","Aug_1999","Sep_1999","Oct_1999","Nov_1999", "Dec_1999","Jan_2000","Feb_2000","Mar_2000","Apr_2000","May_2000","Jun_2000","Jul_2000","Aug_2000","Sep_2000","Oct_2000","Nov_2000","Dec_2000","Jan_2001","Feb_2001","Mar_2001","Apr_2001","May_2001","Jun_2001","Jul_2001","Aug_2001","Sep_2001","Oct_2001","Nov_2001","Dec_2001","Jan_2002","Feb_2002","Mar_2002","Apr_2002","May_2002","Jun_2002","Jul_2002","Aug_2002","Sep_2002","Oct_2002","Nov_2002","Dec_2002","Jan_2003","Feb_2003","Mar_2003","Apr_2003","May_2003","Jun_2003","Jul_2003","Aug_2003","Sep_2003","Oct_2003","Nov_2003","Dec_2003","Jan_2004","Feb_2004","Mar_2004","Apr_2004","May_2004","Jun_2004","Jul_2004","Aug_2004","Sep_2004","Oct_2004","Nov_2004","Dec_2004","Jan_2005","Feb_2005","Mar_2005","Apr_2005","May_2005","Jun_2005","Jul_2005","Aug_2005","Sep_2005","Oct_2005","Nov_2005","Dec_2005","Jan_2006","Feb_2006","Mar_2006","Apr_2006","May_2006","Jun_2006","Jul_2006","Aug_2006","Sep_2006","Oct_2006","Nov_2006","Dec_2006","Jan_2007","Feb_2007","Mar_2007","Apr_2007","May_2007","Jun_2007","Jul_2007","Aug_2007","Sep_2007","Oct_2007","Nov_2007","Dec_2007","Jan_2008","Feb_2008","Mar_2008","Apr_2008","May_2008","Jun_2008","Jul_2008","Aug_2008","Sep_2008","Oct_2008","Nov_2008","Dec_2008","Jan_2009","Feb_2009","Mar_2009","Apr_2009","May_2009","Jun_2009","Jul_2009","Aug_2009","Sep_2009","Oct_2009","Nov_2009","Dec_2009","Jan_2010", "Feb_2010", "Mar_2010", "Apr_2010", "May_2010", "Jun_2010", "Jul_2010", 
                    "Aug_2010", "Sep_2010","Oct_2010","Nov_2010","Dec_2010","Jan_2011","Feb_2011","Mar_2011","Apr_2011","May_2011","Jun_2011","Jul_2011","Aug_2011","Sep_2011","Oct_2011","Nov_2011","Dec_2011","Jan_2012","Feb_2012","Mar_2012","Apr_2012","May_2012","Jun_2012","Jul_2012","Aug_2012","Sep_2012","Oct_2012","Nov_2012","Dec_2012","Jan_2013","Feb_2013","Mar_2013","Apr_2013","May_2013","Jun_2013","Jul_2013","Aug_2013","Sep_2013","Oct_2013","Nov_2013","Dec_2013")
head(na.omit(data, 20))
dim(na.omit(data))
csvfile <- "MARS_GPP_all_years_3_22_20117.csv"
write.table(na.omit(data), csvfile,row.names=FALSE, sep=",")




#If you want to extract a slice____________________
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


