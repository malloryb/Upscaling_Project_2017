library(reshape2)
library(plyr)
library(lubridate)
library(tidyverse)

#Deal with 3km LST and NDVI data ----------------------------------------------------------------
LST <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_MOD11A2_LST_Day_3000m.csv")
NDVI <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_MOD13A2_NDVI_Day_3000m.csv")
str(LST)
#Get rid of first two columns
LST<- subset(LST, select = -c(latitude,longitude))
str(LST)
#Append "US-" to all site names and make all lower case 
LST$site = tolower(paste('us', LST$site, sep='-'))
#Reshape
LST_long <- melt(LST, id.vars = c("site"))
#Rename "Variable" column to "Date" and "Value" to "LST"
LST_long <- rename(LST_long, replace = c("variable"="date"))
LST_long <- rename(LST_long, replace = c("value"="LST"))
#Truncate "Date" and convert to date
LST_long$date <- as.Date(substr(LST_long$date, 13,22), "%Y.%m.%d")
#Convert from celsius to Kelvin
LST_long$LST <- LST_long$LST - 273.15
str(LST_long)
write.csv(LST_long, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_LST_3km_long.csv")
#Format NDVI
str(NDVI)
#Get rid of first two columns
NDVI<- subset(NDVI, select = -c(latitude,longitude))
str(NDVI)
#Append "US-" to all site names and make all lower case 
NDVI$site = tolower(paste('us', NDVI$site, sep='-'))
#Reshape
NDVI_long <- melt(NDVI, id.vars = c("site"))
#Rename "Variable" column to "Date" and "Value" to "NDVI"
head(NDVI_long)
NDVI_long <- rename(NDVI_long, replace = c("variable"="date"))
NDVI_long <- rename(NDVI_long, replace = c("value"="NDVI"))
#Truncate "Date" and convert to date
NDVI_long$date <- as.Date(substr(NDVI_long$date, 6,15), "%Y.%m.%d")
str(NDVI_long)
#Write file
write.csv(NDVI_long, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_NDVI_3km_long.csv")
#Now need to aggregate both LST and NDVI to monthly
#LST first - floor date to aggregate to monthly
LST_long$my <- floor_date(LST_long$date, "month")
#Ddply based on both my (month-year) and on site
LST_monthly_means <- ddply(LST_long, .(site, my), summarise, LST = mean(LST, na.rm=TRUE))
LST_monthly_means <- rename(LST_monthly_means, replace = c("my"="date"))
#Write out
write.csv(LST_monthly_means, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_LST_3km_monthly_means.csv")
#NDVI next - make sure to change any negative NDVI values to "NA"
str(NDVI_long)
NDVI_long$NDVI[NDVI_long$NDVI < 0] <- NA
#Floor date to aggregate to monthly
NDVI_long$my <- floor_date(NDVI_long$date, "month")
#Ddply based on both my (month-year) and on site
NDVI_monthly_means <- ddply(NDVI_long, .(site, my), summarise, NDVI = mean(NDVI, na.rm=TRUE))
NDVI_monthly_means <- rename(NDVI_monthly_means, replace = c("my"="date"))
str(NDVI_monthly_means)
#Write out
write.csv(NDVI_monthly_means, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_NDVI_3km_monthly_means.csv")
#Merge monthly NDVI and monthly LST files into one monthly RS files list
MODIS_monthly_means <- merge(LST_monthly_means, NDVI_monthly_means)
write.csv(MODIS_monthly_means, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")


#Get monthly daymet data-----------------------------
#Want a function that: 
#1. formats date
#2. Gets monthly statistics
#3. writes out results into data frame with sitename column
setwd("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Daymet/")
Daymet_list <- list.files(pattern="^u..*csv$")
fname <- basename(Daymet_list[1])
substring(fname, 1,6)
#can we add SPEI calculations to this file? 
format_daymet <- function(x){
  fname <- substring(basename(x), 1,6)
  print(fname)
  file  <- read.csv(x, skip=6)
  head(file)
  file$date <-as.Date(paste(file$year, file$yday, sep="-"), format="%Y-%j")
  file$my <- floor_date(file$date, "month")
  es <- 0.6108 * exp(17.27 * file$tmax..deg.c. / (file$tmax..deg.c. + 237.3))
  file$vpd <- es -(file$vp..Pa./1000) 
  head(file)
  Monthly_means <- ddply(file, .(my), summarise, daylength = mean(dayl..s., na.rm=TRUE),
                         precip=sum(prcp..mm.day., na.rm=TRUE), srad=mean(srad..W.m.2., na.rm=TRUE), swe=sum(swe..kg.m.2., na.rm=TRUE),
                         tmax=mean(tmax..deg.c., na.rm=TRUE), tmin=mean(tmin..deg.c., na.rm=TRUE), vp=mean(vp..Pa., na.rm=TRUE), vpd=mean(vpd, na.rm=TRUE))
  Monthly_means <- plyr::rename(Monthly_means, replace = c("my"="date"))
  Monthly_means$site <- fname
  
  return(as.data.frame(Monthly_means))
  
}

test <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Daymet/us-aud.csv")
test$tmed <- (test$tmax + test$tmin/2)
FF$PET <- thornthwaite(FF$tmed, FF$Latitude[1], na.rm=TRUE)
FF$BAL <- FF$precip - FF$PET
FF$spei1 <-spei(FF[,'BAL'],1) 

daymet_tmp <- lapply(Daymet_list, format_daymet)
daymet <- do.call(rbind, daymet_tmp)
daymet[800:1200,]
write.csv(daymet, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Daymet_monthly_all_5_5_18.csv")


#Get all flux files into one------------------------------
setwd("C:/Users/Mallory/odrive/UA_Google_Drive/Biederman_Flux/")

fluxlist = list.files(pattern="^u.*csv$")
fluxlist

#Format_flux - function to: 
#1. Get rid of "x" column
#2.Format date
#3. Add column with sitename
#4. append into one large data frame

format_flux <- function(x){
  fname <- substring(basename(x), 1,6)
  file  <- read.csv(x)
  file<- subset(file, select = -c(X))
  file$date <- as.Date(with(file, paste("01-", monthyear, sep="")), format="%d-%b-%Y")
  file <- subset(file, select=-c(monthyear))
  file$site <- fname
  return(as.data.frame(file))
  
}

flux_tmp <- lapply(fluxlist, format_flux)
flux <- do.call(rbind, flux_tmp)
write.csv(flux, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Flux_data_all.csv")


#THE MERGE----------------------------------------------
#Read all three big .csv files and merge by date

#Read in monthly files 
#Flux
Flux_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Flux_data_all.csv")
#Daymet
Daymet_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Daymet_monthly_all.csv")
#MODIS
MODIS_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")


#Check files, delete "X", and format date col for merge 
str(Flux_merge)
Flux_merge<- subset(Flux_merge, select = -c(X))
Flux_merge$date <- as.Date(Flux_merge$date, format="%Y-%m-%d")
Flux_merge$sitedate <- with(Flux_merge, paste(site,date, sep="-"))

str(Daymet_merge)
Daymet_merge<- subset(Daymet_merge, select = -c(X))
Daymet_merge$date <- as.Date(Daymet_merge$date, format="%Y-%m-%d")
Daymet_merge$sitedate <- with(Daymet_merge, paste(site,date, sep="-"))

str(MODIS_merge)
MODIS_merge<- subset(MODIS_merge, select = -c(X))
MODIS_merge$date <- as.Date(MODIS_merge$date, format="%Y-%m-%d")
MODIS_merge$sitedate <- with(MODIS_merge, paste(site,date, sep="-"))


#Merge! And clean up merged mess. Final merged file should have same number
#of observations as the "flux_merge" file since we should have complete records
#For both daymet and MODIS - 2197 obs for all

Flux_daymet <- merge(Flux_merge, Daymet_merge, by="sitedate", all.x=T)
Merged_all <- merge(Flux_daymet, MODIS_merge, by="sitedate", all.x=T)

str(Merged_all)

#Cleanup
#Delete extraneous columns
Merged_all <- subset(Merged_all, select = -c(sitedate, date.x, site.x, date.y, site.y))
#Get month
Merged_all$month <- substr(Merged_all$date, 6,7)
#Format_Date
Merged_all$date <- as.Date(Merged_all$date)
Merged_all$IGBP <- NA

str$Merged_all
#Add IGBP column based on lookup table------------------ not working yet 
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Veg_Type_Lookup.csv")
str(IGBP_lookup)

All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGPB <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGPB)

write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_7_12_2017.csv")


