#Fixing the offset issue (water year starting on Nov 1st vs. Oct 1st) and re-running the site-level analysis
#Created 2/2/2018
#To-do's
#1) Merge daymet files with fixed flux files
#2) Merge item #1) with EVI data and LST data
#3) Merge item #2) with Hydro 1k data
#4) Calculate water balance using 'spei' package for site-based RF (X)
#5) Calculate spei for site-based RF
#6) Run site-based RF with proper input variables
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)
#1) Merge daymet files with fixed flux files----------------------
#Read all three big .csv files and merge by date

#Flux
Flux_merge <- read.csv("D:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_2_2_2018.csv")
#Daymet
Daymet_merge <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Daymet_monthly_all.csv") 
#MODIS
MODIS_merge <-  read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")

#Check files, delete "X", and format date col for merge 
str(Flux_merge)
Flux_merge$date <- as.Date(Flux_merge$date, format="%m/%d/%Y")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soy", "us-so3")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-sob", "us-so2")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-son", "us-so4")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soo", "us-so2")
Flux_merge$sitedate <- with(Flux_merge, paste(site,date, sep="-"))


#Replace sites "us-ray" and "us-tex" with their proper name (they are from mexiflux not ameriflux)
Daymet_merge<- subset(Daymet_merge, select = -c(X))
Daymet_merge$date <- as.Date(Daymet_merge$date, format="%Y-%m-%d")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-ray", "mx-ray")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-tes", "mx-tes")
Daymet_merge$sitedate <- with(Daymet_merge, paste(site,date, sep="-"))

MODIS_merge<- subset(MODIS_merge, select = -c(X))
MODIS_merge$date <- as.Date(MODIS_merge$date, format="%Y-%m-%d")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-ray", "mx-ray")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-tes", "mx-tes")
MODIS_merge$sitedate <- with(MODIS_merge, paste(site,date, sep="-"))
str(MODIS_merge)

levels(unlist(as.factor(Daymet_merge$site)))
levels(unlist(as.factor(Flux_merge$site)))
levels(unlist(as.factor(MODIS_merge$site)))

head(Daymet_merge)
head(Flux_merge)
head(MODIS_merge)
#Merge! And clean up merged mess. Final merged file should have same number
#of observations as the "flux_merge" file since we should have complete records
#For both daymet and MODIS - 2197 obs for all
Flux_daymet <- merge(Flux_merge, Daymet_merge, by="sitedate", all.x=T)
Merged_all <- merge(Flux_daymet, MODIS_merge, by="sitedate", all.x=T)

str(Flux_daymet)
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
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Veg_Type_Lookup_2018.csv")
str(IGBP_lookup)
IGBP_lookup
All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGPB <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGPB)

write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_6_2018.csv")

#Merge SPEI --------------------------------------------
#For now, we should be able to just merge the dataset with the other parts of the big spreadsheet from before

Old_csv <- read.csv("D:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_12_5.csv")
New_csv <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_6_2018.csv")

str(New_csv)
New_csv$site_mo <- paste(New_csv$site, paste("0", New_csv$month, sep=""),  sep="_")

str(Old_csv)
Old_csv$site_mo <- paste(Old_csv$site, paste("0", Old_csv$month, sep=""), sep="_")
Old_csv_sub <- Old_csv[,c("date", "Latitude", "Longitude", "SPEI_1", "elev", "IGBP_no", "SPEI_3", "SPEI_6", "SPEI_9", "SPEI_IGBP", "CCP", "MAT", "MAP", "site_mo")]
merge(New_csv, Old_csv_sub, by="site_mo", all.x=T)



str(New_csv)  
str(Old_csv_sub)

ggplot(New_csv, aes(x='date', y='GPP', color='site', group='site')) +
geom_line()  

qplot(New_csv$date, New_csv$GPP)

ggplot(pd.melt(New_csv, id_vars=['site']), aes(x='date', y='GPP', color='site')) +
geom_line()

??pd.melt
