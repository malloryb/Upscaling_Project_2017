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
Daymet_merge$tmed <- ((Daymet_merge$tmax + Daymet_merge$tmin)/2)
Daymet_merge$date <- as.Date(Daymet_merge$date, format="%Y-%m-%d")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-ray", "mx-ray")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-tes", "mx-tes")
Daymet_merge$sitedate <- with(Daymet_merge, paste(site,date, sep="-"))
unique(Daymet_merge$site)

#Set latitudes by site
str(Daymet_merge)
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
IGBP_lookup <- IGBP_lookup[,c("site", "lat")]
Lat_merge <- plyr::rename(IGBP_lookup, c("site"="site", "lat"="Latitude"))
Daymet_for_SPEI <- merge(Daymet_merge, Lat_merge, by="site")
str(Daymet_for_SPEI)
unique(Daymet_for_SPEI$site)
#split 
#More sites in here than in the lookup file - so should be lots of blanks re: Latitude 
X <- split(Daymet_for_SPEI, Daymet_for_SPEI$site)
df1  <- X[[1]]
df2  <- X[[2]]
df3  <- X[[3]]
df4  <- X[[4]]
df5  <- X[[5]]
df6  <- X[[6]]
df7  <- X[[7]]
df8  <- X[[8]]
df9  <- X[[9]]
df10 <- X[[10]]
df11 <- X[[11]]
df12 <- X[[12]]
df13 <- X[[13]]
df14 <- X[[14]]
df15 <- X[[15]]
df16 <- X[[16]]
df17 <- X[[17]]
df18 <- X[[18]]
df19 <- X[[19]]
df20 <- X[[20]]
df21 <- X[[21]]
df22 <- X[[22]]
df23 <- X[[23]]
df24 <- X[[24]]


#SPEI_calc_function
SPEI_calc <- function(A){
  A <- A[order(A$date),]
  print(A$site[1])
  print("calculate water balance")
  A$PET <- thornthwaite(A$tmed, A$Latitude[1], na.rm=TRUE)
  A$BAL <- A$precip - A$PET
  print("calculate spei")
  spei1 <- (spei(A[,'BAL'], 1)$fitted)
  spei1 <- data.frame(spei1 = c(spei1), time=c(time(spei1))) 
  spei3 <- (spei(A[,'BAL'], 3)$fitted)
  spei3 <- data.frame(spei3 = c(spei3), time = c(time(spei3))) 
  spei6 <- (spei(A[,'BAL'], 6)$fitted)
  spei6 <- data.frame(spei6 = c(spei6), time = c(time(spei6))) 
  spei9 <- (spei(A[,'BAL'], 9)$fitted)
  spei9 <- data.frame(spei9 = c(spei9), time = c(time(spei9))) 
  spei12 <-(spei(A[,'BAL'], 12)$fitted)
  spei12 <-data.frame(spei12 = c(spei12), time = c(time(spei12))) 
  print("bind everything together")
  A$PET <- as.numeric(A$PET)
  A$BAL <- as.numeric(A$BAL)
  spei_all <- cbind(spei1, spei3, spei6, spei9, spei12)
  spei_all$date <- format(date_decimal(spei_all$time), "%m-%d-%Y")
  spei_all$date <- as.Date(spei_all$date, format="%m-%d-%Y")
  spei_all$date <- floor_date((spei_all$date +1), "month")
  spei_all$month <- month(spei_all$date)
  #columns to get rid of in the final thing: 16, 18, 20, 22, 24 (all caled "time") and the second "date (postion=25)
  final <- cbind(A, spei_all)
  final <- final[-c(16,18,20,22,24,25)]
  print(head(final))
  print(nrow(A))
  print(nrow(final))
  return(final)
}

#Get list of dataframes using pattern (thank you stack overflow!: https://stackoverflow.com/questions/14954399/put-multiple-data-frames-into-list-smart-way)
l.df <- lapply(ls(pattern="df[0-9]+"), function(x) get(x))
str(l.df)
#Apply & combine in one
Daymet_merge <- do.call("rbind", lapply(l.df, SPEI_calc))

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
#Format_Date
Merged_all$date <- as.Date(Merged_all$date)
Merged_all$IGBP <- NA
str(Merged_all)
#Add IGBP column based on lookup table------------------ not working yet 
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
str(IGBP_lookup)
IGBP_lookup
All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGPB <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGPB)

#Write out .csv file
write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_15_2018.csv")

