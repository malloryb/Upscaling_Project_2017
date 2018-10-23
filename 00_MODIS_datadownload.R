

library(MODISTools)

#Downloading MODIS data with MODISTOols Package----------------

#just Kendall Grassland - subset by date first
GetDates(Product="MOD13Q1", Lat=31.736527, Long=-109.94188)
#This file would be much longer with multiple sites
modis.subset <- data.frame(lat=31.736527, long=-109.94188)
modis.subset$start.date <- 2004 
modis.subset$end.date <- 2014 

#This takes a bit - 1 full minute for 1 site 
MODISSubsets(LoadDat = modis.subset, Products = "MOD13Q1",
             Bands = c("250m_16_days_EVI", "250m_16_days_pixel_reliability"),
             Size = c(1,1))

subset.string <- read.csv(list.files(pattern= ".asc")[1], 
                          header=FALSE, as.is=TRUE)
subset.string[1,]

MODISSummaries(LoadDat = modis.subset, Product = "MOD13Q1", Bands = "250m_16_days_EVI",
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               QualityScreen = TRUE, QualityBand = "250m_16_days_pixel_reliability",
               QualityThreshold = 0)



#Downlaoding with Python Script -----------------------------
setwd("C:/Users/rsstudent/Upscaling_Data/")
f <- paste0("modis_download.py -I -r -t h18v03,h18v04 -f 2008-01-01 -e 2008-01-31 Test_Download/")
# Call the python script
system(f)