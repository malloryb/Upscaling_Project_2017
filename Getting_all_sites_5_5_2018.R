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
library(SPEI)

merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
  #Function from here: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
  # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
  add.id.column.to.data <- function(DATA)
  {
    data.frame(DATA, id... = seq_len(nrow(DATA)))
  }
  # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
  order.by.id...and.remove.it <- function(DATA)
  {
    # gets in a data.frame with the "id..." column.  Orders by it and returns it
    if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
    
    ss_r <- order(DATA$id...)
    ss_c <- colnames(DATA) != "id..."
    DATA[ss_r, ss_c]
  }
  
  # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
  # tmp()
  
  if(!missing(keep_order))
  {
    if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
    if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
    # if you didn't get "return" by now - issue a warning.
    warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
  } else {return(merge(x=x,y=y,..., sort = sort))}
}

#1) Merge daymet files with fixed flux files----------------------
#Read all three big .csv files and merge by date

#Flux
Flux_merge <- read.csv("F:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_3_2_2018.csv")
#Daymet
Daymet_merge <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Daymet_monthly_all_5_5_18.csv") 
#MODIS
MODIS_merge <-  read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/MODIS_3km_monthly.csv")

#Check files, delete "X", and format date col for merge 
str(Flux_merge)
Flux_merge$date <- as.Date(Flux_merge$date, format="%Y-%m-%d")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soy", "us-so3")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-sob", "us-so2")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-son", "us-so4")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-soo", "us-so2")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-ray", "mx-ray")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-tes", "mx-tes")
Flux_merge$site <- str_replace_all(Flux_merge$site, "us-lpa", "mx-lpa")
Flux_merge$sitedate <- with(Flux_merge, paste(site,date, sep="-"))

which(is.na(Flux_merge$date), arr.ind=TRUE)
Flux_merge <- Flux_merge[-c(176, 1308), ]
unique(Flux_merge$site)
#Replace sites "us-ray" and "us-tex" with their proper name (they are from mexiflux not ameriflux)
Daymet_merge<- subset(Daymet_merge, select = -c(X))
Daymet_merge$tmed <- ((Daymet_merge$tmax + Daymet_merge$tmin)/2)
Daymet_merge$date <- as.Date(Daymet_merge$date, format="%Y-%m-%d")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-ray", "mx-ray")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-tes", "mx-tes")
Daymet_merge$site <- str_replace_all(Daymet_merge$site, "us-lpa", "mx-lpa")
Daymet_merge$sitedate <- with(Daymet_merge, paste(site,date, sep="-"))
unique(Daymet_merge$site)


#Set latittude by site for SPEI analysis
str(Daymet_merge)
IGBP_lookup <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
IGBP_lookup <- IGBP_lookup[,c("site", "lat")]
IGBP_lookup$site <- str_replace_all(IGBP_lookup$site, "us-lpa", "mx-lpa")
str(IGBP_lookup)
unique(IGBP_lookup$site)

Lat_merge <- plyr::rename(IGBP_lookup, c("site"="site", "lat"="Latitude"))
Daymet_for_SPEI <- merge(Daymet_merge, Lat_merge, by="site")
str(Daymet_for_SPEI)
unique(Daymet_for_SPEI$site)

#split apply combine to calculate SPEI from Daymet data 
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

#SPEI_calc_function - spei1, spei3, spei6, spei9, and spei12 
SPEI_calc <- function(A){
  A <- A[order(A$date),]
  A$date <- as.Date(A$date, format="%Y-%m-%d")
  print("calculate water balance")
  A$PET <- thornthwaite(A$tmed, A$Latitude[1], na.rm=TRUE)
  A$BAL <- A$precip - A$PET
  print("calculate spei")
  spei1 <- (spei(A[,'BAL'], 1)$fitted)
  spei1 <- data.frame(spei1 = c(spei1), time=A$date) 
  spei3 <- (spei(A[,'BAL'], 3)$fitted)
  spei3 <- data.frame(spei3 = c(spei3), time=A$date) 
  spei6 <- (spei(A[,'BAL'], 6)$fitted)
  spei6 <- data.frame(spei6 = c(spei6), time=A$date) 
  spei9 <- (spei(A[,'BAL'], 9)$fitted)
  spei9 <- data.frame(spei9 = c(spei9), time=A$date) 
  spei12 <-(spei(A[,'BAL'], 12)$fitted)
  spei12 <-data.frame(spei12 = c(spei12), time=A$date) 
  spei24 <-(spei(A[,'BAL'], 12)$fitted)
  spei24 <-data.frame(spei24 = c(spei24), time=A$date) 
  spei48 <-(spei(A[,'BAL'], 12)$fitted)
  spei48 <-data.frame(spei48 = c(spei48), time=A$date) 
    print("bind everything together")
  A$PET <- as.numeric(A$PET)
  A$BAL <- as.numeric(A$BAL)
  spei_all <- cbind(spei1, spei3, spei6, spei9, spei12,spei24,spei48)
  print(head(spei_all))
  spei_all$date <- as.Date(spei_all$time, format="%Y-%m-%d")
  spei_all$date <- floor_date((spei_all$date +1), "month")
  spei_all$month <- month(spei_all$date)
  #columns to get rid of in the final thing: 16, 18, 20, 22, 24 (all caled "time") and the second "date (postion=25)
  #print(head(A))
  print(head(spei_all))
  final <- cbind(A, spei_all)
  print("these")
  str(final)
  drops <- c("time","date")
  final[ , !(names(final) %in% drops)]
  #print(nrow(A))
  #print(nrow(final))
  #return(final)
}

testspei <- l.df[[1]]
#Get list of dataframes using pattern (thank you stack overflow!: https://stackoverflow.com/questions/14954399/put-multiple-data-frames-into-list-smart-way)
l.df <- lapply(ls(pattern="df[0-9]+"), function(x) get(x))
str(l.df)
#Apply & combine in one
Daymet_merge <- do.call("rbind", lapply(l.df, SPEI_calc))
str(Daymet_merge)

#Now merge the MODIS, flux, and Daymet files together
MODIS_merge<- subset(MODIS_merge, select = -c(X))
MODIS_merge$date <- as.Date(MODIS_merge$date, format="%Y-%m-%d")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-ray", "mx-ray")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-tes", "mx-tes")
MODIS_merge$site <- str_replace_all(MODIS_merge$site, "us-lpa", "mx-lpa")
MODIS_merge$sitedate <- with(MODIS_merge, paste(site,date, sep="-"))
str(MODIS_merge)

levels(unlist(as.factor(Daymet_merge$site)))
levels(unlist(as.factor(Flux_merge$site)))
levels(unlist(as.factor(MODIS_merge$site)))

str(Flux_merge)

head(Daymet_merge)
head(Flux_merge)
head(MODIS_merge)
unique(Daymet_merge$site)

#Merge! And clean up merged mess. Final merged file should have same number
#of observations as the "flux_merge" file since we should have complete records
#For both daymet and MODIS - 2197 obs for all

Flux_daymet <- merge(Flux_merge, Daymet_merge, by="sitedate", all.x=T)
str(Flux_daymet)
Merged_all <- merge(Flux_daymet, MODIS_merge, by="sitedate")
str(Merged_all)
unique(Flux_daymet$site.x)
unique(Flux_daymet$site.y)
unique(MODIS_merge$site)
unique(Merged_all$site)
#Cleanup
#Delete extraneous columns
str(Merged_all)
Merged_all <- subset(Merged_all, select = -c(X, X1, site.x, date.y, site.y,sitedate))
Merged_all <- rename(Merged_all, 'EVI'='NDVI')
Merged_all$sitedate <- paste(Merged_all$site, Merged_all$monthyear, sep="-")
#Format_Date
Merged_all <- plyr::rename(Merged_all, replace = c("date.x"= "date"))
Merged_all$IGBP <- NA
str(Merged_all)
unique(Merged_all$site)
#Add IGBP column based on lookup table------------------ not working yet 
IGBP_lookup <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
str(IGBP_lookup)
IGBP_lookup$site <- str_replace_all(IGBP_lookup$site, "us-lpa", "mx-lpa")
All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGBP <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGBP)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Add NDVI in addition to EVI to the Allsites file
setwd("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Projct/MODIS_NDVI/")

ndvilist <- list.files("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Projct/MODIS_NDVI/")
ndvilist

modis_ndvi <- function(file){
  filename <- tools::file_path_sans_ext(basename(file))
  print(filename)
  tstndvi <- read.csv(file)
  tstndvi <- dplyr::rename(tstndvi, date=system.time_start, NDVI=NDVI_1)
  tstndvi$month <- substr(as.character(tstndvi$date), 1,3)
  tstndvi$year <- substrRight(as.character(tstndvi$date),4)
  tstndvi$monthyear <- paste(tstndvi$month, tstndvi$year, sep="-")
  tstndvi$date <- paste(tstndvi$monthyear, "01", sep="-")
  mergendvi <- ddply(tstndvi, ~monthyear, summarize, NDVI=mean(NDVI, na.rm=TRUE))
  mergendvi$site <- paste("us", substr(filename, 1,3), sep="-")
  mergendvi$site <- str_replace_all(mergendvi$site, "us-ray", "mx-ray")
  mergendvi$site <- str_replace_all(mergendvi$site, "us-tes", "mx-tes")
  mergendvi$site <- str_replace_all(mergendvi$site, "us-lpa", "mx-lpa")
  mergendvi$sitedate <- paste(mergendvi$site, mergendvi$monthyear, sep="-")
  return(mergendvi)
}

ndvi <- do.call("rbind", lapply(ndvilist, modis_ndvi))
str(ndvi)
str(ALL_inc_IGBP)
unique(ndvi$site)
unique(ALL_inc_IGBP$site)
All_sites_ndvi <- merge(ALL_inc_IGBP, ndvi, by="sitedate")
str(All_sites_ndvi)
unique(All_sites_ndvi$site.x)
All_sites_ndvi <- subset(All_sites_ndvi, select = -c(site.y, monthyear.y))
All_sites_ndvi <- rename(All_sites_ndvi, 'site'='site.x')
All_sites_ndvi <- rename(All_sites_ndvi, 'monthyear'='monthyear.x')
head(All_sites_ndvi)
write.csv(All_sites_ndvi, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Projct/All_sites_5_7_2018.csv")
#function to code month in a way that will allow for automatic s vs. n. hemisphere distinctions
#going to have to split, apply, combine 
#Function also calculates amplitude for each site
X <- split(All_sites_ndvi, All_sites_ndvi$site)

xf1  <- X[[1]]
xf2  <- X[[2]]
xf3  <- X[[3]]
xf4  <- X[[4]]
xf5  <- X[[5]]
xf6  <- X[[6]]
xf7  <- X[[7]]
xf8  <- X[[8]]
xf9  <- X[[9]]
xf10 <- X[[10]]
xf11 <- X[[11]]
xf12 <- X[[12]]
xf13 <- X[[13]]
xf14 <- X[[14]]
xf15 <- X[[15]]
xf16 <- X[[16]]
xf17 <- X[[17]]
xf18 <- X[[18]]
xf19 <- X[[19]]
xf20 <- X[[20]]
xf21 <- X[[21]]
xf22 <- X[[22]]
xf23 <- X[[23]]
xf24 <- X[[24]]

monthR <- function(x){
  #key data frame 
  df=data.frame(month=numeric(12), SH=character(12), NH=character(12))
  df$month <- c(1:12)
  df$SH <- c("su2", "su3", "fa1", "fa2", "fa3", "wi1", "wi2", "wi3", "sp1", "sp2", "sp3", "su1")
  df$NH <- c("wi2", "wi3", "sp1", "sp2", "sp3", "su1", "su2", "su3", "fa1", "fa2", "fa3", "wi1")
  SH <- subset(df[1:2],)
  NH <- df[,-2]
  x <- x[order(as.Date(x$date, "%Y-%m-%d")),]
  x1 <- x[1:24,]
  maxmo <- x1$month[which.max(x1$daylength)]
  
  hem <- function(x) {if(maxmo==6){
    return(merge(x, NH, by="month"))
  }
    else{
    return(merge(x, SH, by="month"))
    } 
   
}

  
  y <- hem(x)
  y$amp <- max(y$daylength) - min(y$daylength)
  colnames(y)[41] <- "hmon"
  y$X <- NULL
  print(head(y))
  print(cor(y$vpd, y$VPD, use="complete.obs"))
  return(y)

  }

#Apply & combine#
testfrm <- X[[1]]
#testfrm <- testfrm[order(as.Date(testfrm$date, "%Y-%m-%d")),]
monthR(testfrm)
l2.df <- lapply(ls(pattern="xf[0-9]+"), function(x) get(x))
str(l2.df)

all_sites_hem <- do.call("rbind", lapply(l2.df, monthR))

write.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_Projct/All_sites_hmon_5_6.csv")

#Function that incorporates month and daylength
myplot <- ggplot(data=All_sites, aes(x=month, y=daylength, colour=site)) + geom_line()
myplot %+% subset(All_sites, site %in% c("us-fuf", "mx-lpa"))

  geom_line(All_sites$site="us-fuf")+
  geom_line(All_sites$site="mx-lpa")

#Write out .csv file
write.csv(All_sites, "C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_5_5_2018.csv")

str(All_sites)
#Get random forest models (this code could be cleaned up significantly) 
#2. Run site-based RF with proper variables ------------------
library(lubridate)
library(caret)
library(randomForest)
library(dplyr)
library(plyr)

#From UC-Irvine Machine learning repository
#Now Doing 3 different models: one for spring ("Mar-May), summer("Jun-Sep"), Inactive("Oct-"feb")
All_sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_5_5_2018.csv") 
str(All_sites)


head(All_sites)
#Checking on SPEI
#SPEI_Check <- All_sites[c("date", "site", "spei1", "spei12")]
#SPEI_Check$date <- as.Date(SPEI_Check$date, format="%Y-%m-%d")
#SPEI_Check$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
#SPEI_Check$site_year <- do.call(paste, c(SPEI_Check[c("site", "year")], sep = "_")) 

#Checked out whether SPEI is correlated with other stuff
#SPEI_cors <- ddply(SPEI_Check, .(site_year), summarize,
#        spei1mean = mean(spei1,na.rm=TRUE), spei1max= spei1[which.max( abs(spei1))], spei1min=spei1[which.min( abs(spei1))], 
#        spei1med=median_hilow(spei1, na.rm=TRUE), spei12 = first(spei12))
#cor(SPEI_cors$spei12, SPEI_cors$spei1mean, use="complete.obs")
#cor(SPEI_cors$spei12, SPEI_cors$spei1max, use="complete.obs")
#cor(SPEI_cors$spei12, SPEI_cors$spei1min, use="complete.obs")
#cor(SPEI_cors$spei12, SPEI_cors$spei1med, use="complete.obs")

#Fix column names and add numeric columns
str(All_sites)
All_sites$elev <- as.numeric(All_sites$elev)
All_sites$year <- as.factor(year(as.Date(All_sites$date, format="%Y-%m-%d")))
All_sites$month <- as.factor(All_sites$month)
All_sites$precip <- as.numeric(All_sites$precip)
All_sites$swe <- as.numeric(All_sites$swe)

#Prepare your data
#Normalization makes data easier for the RF algorithm to learn
#Two types of normalization: 
#example normalization (normalize each case individually)
#feature normalization (adjust each feature in the same way across all cases)
#Normalization a good idea when one attribute has a wide range of values compared to others

#User-defined normalization function
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

#Function to see if any cols have NAs. Now that I'm not standardizing GPP anymore having issues w/ NAs
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) any(is.na(x))))]
}

str(All_sites)
All_sites <- All_sites[c("GPP", "date", "daylength", "site", "elev", "month", "srad", "swe", "tmed", "tmax", "tmin", "BAL", "PET", 
                         "precip", "vp", "MAP", "MAT", "NDVI", "spei1", "spei3", "spei6", "spei9", "spei12")]

#408th (row 409) value for All_Sites$spei3 is "Inf" for some reason, and so is the 1495th value. Going to replace it with the average of the two surrounding values: 0.495
#Why are they in different spots? at 266 and 1448 this time
which(sapply(All_sites$spei3, is.infinite))

All_sites$spei3[265:267]
All_sites$spei3[266] <- 0.908
All_sites$spei3[1447:1449]
All_sites$spei3[1448] <- -1.0609

#Get rid of non-complete cases and double check
summary(All_sites)
All_sites <- All_sites[complete.cases(All_sites),]
summary(All_sites)
#Going to try squaring GPP
All_sites$GPP <- sqrt(All_sites$GPP)
#No longer going to normalize variables as per here: https://stats.stackexchange.com/questions/57010/is-it-essential-to-do-normalization-for-svm-and-random-forest
#Here's where we can split
#Timesilces
#mypartition <- createIrregularTimeSlices(All_sites$date, initialWindow=48, horizon=12, unit="month", fixedWindow=T)
#ctrl <- trainControl(index=mypartition$train, indexOut=mypartition$test)
#tsmod <- train(All_sites.training[colsA1], All_sites.training[,5], method="rf", trControl=ctrl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

#Split into training and testing data

index <- createDataPartition(All_sites$GPP, p=0.80, list=FALSE)
index

#Resample data to overrepresent high GPP observations(? should I have done this)
#Subset training set
All_sites.training <- All_sites[index,]
All_sites.test <- All_sites[-index,]
str(All_sites)

#All_sites.trianing <- preProcess(All_sites.training, method = c("center", "scale"))
str(All_sites.training)
str(All_sites.test)

#Overview of algorithms supported by caret function
names(getModelInfo())

head(All_sites)
#Model with all:
colsA1 <- c(3, 5:23)
head(All_sites.training)
head(All_sites.training[,colsA1])
str(All_sites.training[,colsA1])
head(All_sites.training[,1:2])

str(All_sites.training)
#Model with: NDVI, daylength, MAP, MAT, vp, month, srad, spei12, PET, tmax, elev, tmin, precip, spei1
colsA2 <- c(3, 5:7, 10:11, 13:19, 23)
head(All_sites.training)
head(All_sites.training[,colsA2])
head(All_sites.training[,5:6])


#Model with: NDVI, daylength, MAP, MAT, vp, month, srad, tmax, elev, tmin, precip, spei3, spei9, spei12, spei6, spei1)
colsA3 <- c(3, 5:7, 10:11, 14:18)
head(All_sites.training)
head(All_sites.training[,colsA3])
head(All_sites.training[,5:6])

#Model same as A2 but without PET 
colsA4 <- c(3, 5:7, 10:11, 14:19, 23)
head(All_sites.training)
head(All_sites.training[,colsA4])
head(All_sites.training[,5:6])

#this doesn't seem to be working
All_sites.training[!complete.cases(All_sites.training),]

#Train a model (trying both KNN and random forest)
#Each of these takes awhile: approx 10 mins
myControl <- trainControl(method="repeatedcv", repeats=5, number=10)

model_rfA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA1 <- train(All_sites.training[,colsA1], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA2 <- train(All_sites.training[,colsA2], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA3 <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA3.sqrt <- train(All_sites.training[,colsA3], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

model_rfA4 <- train(All_sites.training[,colsA4], All_sites.training[,1], method='rf', trControl=myControl, importance=TRUE, do.trace=TRUE, allowParallel=TRUE)
model_tsA4 <- train(All_sites.training[,colsA4], All_sites.training[,1], method='rf', trControl=trainControl(method="timeslice", initialWindow=48, horizon=12, fixedWindow =TRUE), importance=TRUE, do.trace=TRUE, allowParallel=TRUE)

pred_rfA1 <- as.numeric(predict(object=model_rfA1, All_sites.test[,colsA1]))
pred_tsA1 <- as.numeric(predict(object=model_tsA1, All_sites.test[,colsA1]))

pred_rfA2 <- as.numeric(predict(object=model_rfA2, All_sites.test[,colsA2]))
pred_tsA2 <- as.numeric(predict(object=model_tsA2, All_sites.test[,colsA2]))

pred_rfA3 <- as.numeric(predict(object=model_rfA3, All_sites.test[,colsA3]))
pred_tsA3t <- as.numeric(predict(object=model_tsA3, All_sites.test[,colsA3]))

pred_rfA4 <- as.numeric(predict(object=model_rfA4, All_sites.test[,colsA4]))
pred_tsA4 <- as.numeric(predict(object=model_tsA4, All_sites.test[,colsA4]))


cor(pred_rfA1, All_sites.test[,1])
cor(pred_tsA1, All_sites.test[,1])
cor(pred_rfA2, All_sites.test[,1])
cor(pred_tsA2, All_sites.test[,1])
cor(pred_rfA3, All_sites.test[,1])
cor(pred_tsA3, All_sites.test[,1])
cor(pred_rfA4, All_sites.test[,1])
cor(pred_tsA4, All_sites.test[,1])


postResample(pred=pred_rfA1, obs=All_sites.test[,1])
postResample(pred=pred_tsA1, obs=All_sites.test[,1])
postResample(pred=pred_rfA2, obs=All_sites.test[,1])
postResample(pred=pred_tsA2, obs=All_sites.test[,1])
postResample(pred=pred_rfA3, obs=All_sites.test[,1])
postResample(pred=pred_tsA3, obs=All_sites.test[,1])

postResample(pred=pred_rfA4, obs=All_sites.test[,1])
postResample(pred=pred_tsA4, obs=All_sites.test[,1])


RF_F1 <- model_rfA1$finalModel
RF_T1 <- model_tsA1$finalModel
RF_F2 <- model_rfA2$finalModel
RF_T2 <- model_tsA2$finalModel
RF_F3 <- model_rfA3$finalModel
RF_T3 <- model_tsA3$finalModel
RF_F4 <- model_rfA4$finalModel
RF_T4 <- model_tsA4$finalModel


varImpPlot(RF_F1)
varImpPlot(RF_T1)
varImpPlot(RF_F2)
varImpPlot(RF_T2)
varImpPlot(RF_F3)
varImpPlot(RF_T3)

varImpPlot(RF_F4)
varImpPlot(RF_T4)

saveRDS(RF_F1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F1_2_16.rds")
saveRDS(RF_T1, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T1_2_16.rds")
saveRDS(RF_F2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F2_2_16.rds")
saveRDS(RF_T2, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T2_2_16.rds")
saveRDS(RF_F3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F3_3_3.rds")
saveRDS(RF_T3, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T3_3_3.rds")
saveRDS(RF_F4, "F:/Upscaling_Project/Upscaling_Project_2017/RF_F4_3_3.rds")
saveRDS(RF_T4, "F:/Upscaling_Project/Upscaling_Project_2017/RF_T4_3_3.rds")

RFA1
RFA2
lb1 <- paste("R^2 == ", "0.70")
RMSE1 <- paste("RMSE==", "0.76")
lb2 <- paste("R^2 == ", "0.78")
RMSE2 <- paste("RMSE==", "0.37")

#Plot predicted vs. measured
#Model A1
qplot(predsummer, All_sites.test[,5]) + 
  geom_point(shape=19, colour="tomato2", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,7.5)+
  ylim(0,7.5)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb1, parse=TRUE, x = 0.5, y = 6, size = 5, colour = "Black")+
  annotate("text", label = RMSE1, parse=TRUE, x = 0.5, y = 5.5, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - A1")  

#Model A2 
qplot(predA2, All_sites.test[,5]) + 
  geom_point(shape=19, colour="tomato2", size=3)+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,7.5)+
  ylim(0,7.5)+
  xlab("Flux monthly GPP")+
  ylab("Predicted monthly GPP")+
  theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
  annotate("text", label = lb2, parse=TRUE, x = 0.5, y = 6, size = 5, colour = "Black")+
  annotate("text", label = RMSE2, parse=TRUE, x = 0.5, y = 5.5, size = 5, colour = "Black")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ggtitle("Random forest - A2 (A1 + water balance)")  

##3. Running RF models on gridded data subsets --------------------------
library(caret)
library(randomForest)
raster("F:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif", band=93)
#Giant function to do all analyses and create input data - however doing this on STORM with a
#modified functino now 
RF_Val_Analysis <- function(band1, bandsp, month, monthno, year){
  require(SPEI)
  require(ncdf4)
  require(raster)
  require(dplyr)
  #Read in files
  filename <- paste0("F:/Upscaling_Project/Gridded_Inputs/Input_rasters/",month,"_",year, ".tif")
  filenameDayl <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_dayl_2000_2016_AOI.tif"
  filenameSrad <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_srad_2000_2016_AOI.tif"
  filenameVP <- "F:/Upscaling_Project/Gridded_Inputs/upscalingArea_DAYMET_vp_2000_2016_AOI.tif"
  filenameSPEI <- "F:/Upscaling_Project/Gridded_Inputs/Monthly_scale_SPEI_2000-2013.tif"
  filenamespei12 <- "F:/SPEIBase/spei12.nc"
  print(filename)
  MAP_resample <- raster("F:/Upscaling_Project/Gridded_Inputs/MAP_resample.tif")
  MAT_resample <- stack("F:/Upscaling_Project/Gridded_Inputs/MAT_resample.tif")
  inputrast <- stack(filename)
  names(inputrast) <- paste(c("NDVI", "month", "elev", "precip", "tmax", "tmin"))
  #inputrast <-(dropLayer(inputrast, 6))
  srad <- raster(filenameSrad, band = band1)
  vp <- raster(filenameVP, band= band1)
  dayl <- raster(filenameDayl)
  SPEI_1 <- raster(filenameSPEI, band=band1)
  spei12 <- brick(filenamespei12)
  spei12 <- spei12[[bandsp]]
  print("files loaded")
  #Process and resample Daymet variables
  srad[srad==-9999] <-NA
  vp[vp==-9999] <-NA
  print("Subsetting done")
  Sradresample <- resample(srad, MAP_resample, method="bilinear")
  SPEIresample <- resample(SPEI_1, MAT_resample, method="bilinear")
  spei12resample <- resample(spei12, MAT_resample, method="bilinear")
  vpresample <- resample(vp, MAT_resample, method="bilinear")
  daylresample <- resample(dayl, MAT_resample, method="bilinear")
  print("resampling done")
  
  #Raster stack for prediction
  rast_stack <- stack(inputrast, MAP_resample, MAT_resample, Sradresample, vpresample, SPEIresample, daylresample, spei12resample)
  names(rast_stack) <- paste(c("NDVI", "month", "elev", "precip", "tmax","tmin", "MAP", "MAT","srad", "vp", "spei1", "daylength", "spei12"))
  print("raster stacked")
  
  #Predict and write out model A1
  sw <- extent(rast_stack)
  print(sw)
  
  create_extent <- function(x){
    radius <- 0.5 # radius in kilometers
    yPlus <- x[1,2]+ radius
    xPlus <- x[1,1]+ radius
    yMinus <- x[1,2] - radius
    xMinus <- x[1,1] - radius
    e <- extent(xMinus, xPlus, yMinus, yPlus)
    return(e)
     }
  
  #Trying to just do a radius around a point
  AUDpoint <- cbind(-110.509, 31.591)
  COPpoint <- cbind(-109.66, 38.162)
  FUFpoint <- cbind(-111.762,	35.089)
  LPApoint <- cbind(-110.438, 24.1293)
  MPJpoint <- cbind(-106.239,	34.43828)
  #MX site
  RAYpoint <- cbind(-110.537,	29.741)
  SCCpoint<- cbind(-116.45,	33.61)
  SCFpoint<- cbind(-116.45,	33.808)
  SCWpoint<- cbind(-116.455,	33.605)
  SEGpoint<- cbind(-106.7,	34.360)
  SENpoint<- cbind(-106.68,	34.358)
  SESpoint <- cbind(-106.745,	34.335)
  SO4point<- cbind(-116.6406,	33.385)
  SO2point<- cbind(-116.6228,	33.374)
  SO3point<- cbind(-116.6226,	33.377)
  SRCpoint<- cbind(-110.8395,	31.908)
  SRGpoint <- cbind(-110.828,	31.789)
  SRMpoint <- cbind(-110.866, 31.821)
  #MX Site
  TESpoint<- cbind(-109.298,	27.8446)
  VCMpoint<- cbind(-106.532,	35.888)
  VCPpoint<- cbind(-106.597,	35.864)
  WHSpoint<- cbind(-110.052,	31.744)
  WJSpoint<- cbind(-105.862,	34.426)
  WKGpoint <- cbind(-109.942,	31.737)
  
  audex <- create_extent(AUDpoint)
  aud <- crop(rast_stack,audex)
  aud <- extent(aud)
  
  copex <- create_extent(COPpoint)
  cop <- crop(rast_stack,copex)
  cop <- extent(cop)
  
  fufex <- create_extent(FUFpoint)
  fuf <- crop(rast_stack,fufex)
  fuf <- extent(fuf)
  
  lpaex <- create_extent(LPApoint)
  lpa <- crop(rast_stack,lpaex)
  lpa <- extent(lpa)
  
  mpjex <- create_extent(MPJpoint)
  mpj <- crop(rast_stack,mpjex)
  mpj <- extent(mpj)
  
  rayex <- create_extent(RAYpoint)
  ray <- crop(rast_stack,rayex)
  ray <- extent(ray)
  
  
  sccex <- create_extent(SCCpoint)
  scc <- crop(rast_stack,sccex)
  scc <- extent(scc)
  
  scfex <- create_extent(SCFpoint)
  scf <- crop(rast_stack,scfex)
  scf <- extent(scf)
  
  scwex <- create_extent(SCWpoint)
  scw <- crop(rast_stack,scwex)
  scw <- extent(scw)
  
  segex <- create_extent(SEGpoint)
  seg <- crop(rast_stack,segex)
  seg <- extent(seg)
  
  senex <- create_extent(SENpoint)
  sen <- crop(rast_stack,senex)
  sen <- extent(sen)
  
  sesex <- create_extent(SESpoint)
  ses <- crop(rast_stack,sesex)
  ses <- extent(ses)
  
  so4ex <- create_extent(SO4point)
  so4 <- crop(rast_stack,so4ex)
  so4 <- extent(so4)
  
  so2ex <- create_extent(SO2point)
  so2 <- crop(rast_stack,so2ex)
  so2 <- extent(so2)
  
  so3ex <- create_extent(SO3point)
  so3 <- crop(rast_stack,so3ex)
  so3 <- extent(so3)
  
  srcex <- create_extent(SRCpoint)
  src <- crop(rast_stack,srcex)
  src <- extent(src)
  
  srgex <- create_extent(SRGpoint)
  srg <- crop(rast_stack,srgex)
  srg <- extent(srg)
  
  srmex <- create_extent(SRMpoint)
  srm <- crop(rast_stack,srmex)
  srm <- extent(srm)
  
  tesex <- create_extent(TESpoint)
  tes <- crop(rast_stack,tesex)
  tes <- extent(tes)
  
  vcmex <- create_extent(VCMpoint)
  vcm <- crop(rast_stack,vcmex)
  vcm <- extent(vcm)

  vcpex <- create_extent(VCPpoint)
  vcp <- crop(rast_stack,vcpex)
  vcp <- extent(vcp)
  
  whsex <- create_extent(WHSpoint)
  whs <- crop(rast_stack,whsex)
  whs <- extent(whs)
  
  wjsex <- create_extent(WJSpoint)
  wjs <- crop(rast_stack,wjsex)
  wjs <- extent(wjs)
  
  wkgex <- create_extent(WKGpoint)
  wkg <- crop(rast_stack,wkgex)
  wkg <- extent(wkg)
  
  print("subset points")
  
  #Read models
  RFF3<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_F3_2_16.rds")
  RFT3<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_T3_2_16.rds")
  RFF4<- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_F4_2_27.rds")
  RFT4 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF_T4_2_27.rds")
  RFC3 <- readRDS("F:/Upscaling_Project/Upscaling_Project_2017/RF")
  
  
  #For each site: 4 .tifs written out - write function and lapply over list of extents? Or at least function to apply models and write them out
  Apply_RF <- function(site, sname, env= parent.frame()){
    print(ls())
    print(month)
    print(year)
    #Function needs to apply to a site and 
    RFF3_predicted <- predict(rast_stack, RFF3, ext=site)
    plot(RFF3_predicted)
    RFT3_predicted <- predict(rast_stack, RFT3, ext=site)
    RFF4_predicted <- predict(rast_stack, RFF4, ext=site)
    RFT4_predicted <- predict(rast_stack, RFT4, ext=site)
    
    print(month)
    print(year)
    print(site)
    
    outputfilenameF3 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_F3/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameT3 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_T3/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameF4 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_F4/",month,"_",year,"_", sname, ".tif", sep="")
    outputfilenameT4 <- paste("F:/Upscaling_Project/Upscaled_GPP/RF_T4/",month,"_",year,"_", sname, ".tif", sep="")
    
    print(paste("writing out", outputfilenameF3))
    writeRaster(RFF3_predicted, outputfilenameF3, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameT3))
    writeRaster(RFT3_predicted, outputfilenameT3, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameF4))
    writeRaster(RFF4_predicted, outputfilenameF4, overwrite=TRUE)
    
    print(paste("writing out", outputfilenameT4))
    writeRaster(RFT4_predicted, outputfilenameT4, overwrite=TRUE)
    
  }

Apply_RF(aud, "us_aud")
Apply_RF(cop, "us_cop")
Apply_RF(fuf, "us_fuf")
Apply_RF(lpa, "us_lpa")
Apply_RF(mpj, "us_mpj")  
Apply_RF(ray, "us_ray")  
Apply_RF(scc, "us_scc")
Apply_RF(scf, "us_scf")
Apply_RF(scw, "us_scw")
Apply_RF(seg, "us_seg")
Apply_RF(sen, "us_sen")
Apply_RF(ses, "us_ses")  
Apply_RF(so4, "us_so4")  
Apply_RF(so2, "us_so2")
Apply_RF(so3, "us_so3")
Apply_RF(src, "us_src")
Apply_RF(srg, "us_srg")
Apply_RF(srm, "us_srm")
Apply_RF(tes, "us_tes")  
Apply_RF(vcm, "us_vcm")  
Apply_RF(vcp, "us_vcp")
Apply_RF(whs, "us_whs")
Apply_RF(wks, "us_wks")
Apply_RF(wkg, "us_wkg")

  gc()
}

RF_Val_Analysis(band1=85, bandsp=1273, month="Jan", monthno=1, year=2007)


#4. Comparing Tower flux data and Jung 2017 data------------------------------------------------------------
library(ggplot2)
library(lubridate)
library(plyr)
library(stringr)
#Plan: Merge new flux files (sums) with Jung 2017 files (extracted yesterday)
Jung_files <- list.files("F:/Upscaling_Project/Jung_Comps/", pattern="*_*.csv$")

#Function to read all files in list and apply necessary corrections
myDb <- lapply(Jung_files, function(x){
  dat <- read.csv(x, skip=2)
  names(dat) <- paste(c("monthyear", "Jung_GPP"))
  dat$monthyear <- as.character(dat$monthyear)
  dat$site <- tools::file_path_sans_ext(basename(x))
  return(dat)
})

str(myDb)

#Rbind dataframes to gether
Jung_2017 <- do.call(rbind,myDb)

Flux_files <- read.csv("F:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_3_2_2018.csv")
#Merge by "site_date" column
str(Flux_files)
Flux_files$site <- str_replace_all(Flux_files$site, "us-soy", "us-so3")
Flux_files$site <- str_replace_all(Flux_files$site, "us-sob", "us-so2")
Flux_files$site <- str_replace_all(Flux_files$site, "us-son", "us-so4")
Flux_files$site <- str_replace_all(Flux_files$site, "us-soo", "us-so2")
Flux_files$monthyear <- (gsub('([[:punct:]])|\\-','_',Flux_files$monthyear))
Flux_files$site <- (gsub('([[:punct:]])|\\-','_',Flux_files$site))
Flux_files$site <- as.factor(Flux_files$site)
Flux_files$sitedate <- paste(Flux_files$monthyear, Flux_files$site, sep="_")

str(Jung_2017)
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_soy", "us_so3")
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_sob", "us_so2")
Jung_2017$site <- str_replace_all(Jung_2017$site, "us_son", "us_so4")
Jung_2017$site <- as.factor(Jung_2017$site)
Jung_2017$sitedate <- paste(Jung_2017$monthyear, Jung_2017$site, sep="_")


levels(Jung_2017$site)
levels(Flux_files$site)
#Merged file
Merged_Jung_Comp <- merge(Flux_files, Jung_2017, by="sitedate", all.x=T)
#Delete columns: site.y, X, X1, monthyear.y
drops <- c("site.y", "X", "X1", "monthyear.y")
Merged_Jung_Comp <- Merged_Jung_Comp[, !(names(Merged_Jung_Comp) %in% drops)]
#write.csv(Merged_Jung_Comp, "F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")

#5. Graphing intraseasonal comparisons-----------------------------
#Need to create some graphs now:
library(ggthemes)
library(plyr)

Merged_Jung_Comp <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
Merged_Jung_Comp <- Merged_Jung_Comp[!(is.na(Merged_Jung_Comp$year)),] 
Merged_Jung_Comp$date <- as.Date(Merged_Jung_Comp$date, format="%Y-%m-%d")
Merged_Jung_Comp$month <- month(Merged_Jung_Comp$date)
Merged_Jung_Comp$year <- year(Merged_Jung_Comp$date)

#Delete all files after 2013 (Fluxcom only goes through 2013)
Merged_Jung_Comp<-Merged_Jung_Comp[!(Merged_Jung_Comp$year > 2013 | Merged_Jung_Comp$year < 1999),]
new_DF <- Merged_Jung_Comp[is.na(Merged_Jung_Comp$Jung_GPP),]
#Check there's no NAs
new_DF
summary(Merged_Jung_Comp)

#Split - apply - combine 
#split
out <- split(Merged_Jung_Comp, Merged_Jung_Comp$site.x)
str(out)
#Apply
seasonal_func <- function(x){
  df <- ddply(x, .(month, site.x), summarize, Jung_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])) , Jung_GPP=mean(Jung_GPP, na.rm=TRUE), GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])) , GPP=mean(GPP, na.rm=TRUE))
  return(df)
}

lapply(out, seasonal_func)
#Combine
seasonal_to_plot <- do.call(rbind, lapply(out, seasonal_func))
seasonal_to_plot
names(seasonal_to_plot)[names(seasonal_to_plot) == 'site.x'] <- 'site'

#Plotting function that plots all sites serparately and writes out graphs
#Where to write out .png files
setwd("D:/Upscaling_Project/Jung_Comps/")

plot_seasonal_cycle <- function(x){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  r <- as.character(round(cor(x$GPP, x$Jung_GPP), 3))
  rmssdGPP <- as.character(round(rmssd(x$GPP), 3))
  rmssdJungGPP <- as.character(round(rmssd(x$Jung_GPP), 3))
  lbl1 <- paste("rmssdFlux =", rmssdGPP)
  lbl2 <- paste("rmssdJung =", rmssdJungGPP)
  filename <- paste(x$site[1], "seasonal_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(x$site)+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("blue")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    annotate("text", label = lbl1, parse=FALSE, x = 2, y = 4.5, size = 5, colour = "Black")+
    annotate("text", label = lbl2, parse=FALSE, x = 2, y = 5, size = 5, colour = "Black")+
        geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP-Jung_se,ymax=Jung_GPP+Jung_se),colour="blue")+
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  
    plot(q)
    ggsave(filename, device='png', width=16, height=16, plot=q, dpi = 300, units = "cm")
         }

#split
list_seasons <- split(seasonal_to_plot, seasonal_to_plot$site)
#apply (and write out)
#CAREFUL THIS WRITES EVERYTHING OUT
lapply(list_seasons, plot_seasonal_cycle)

#6. Graphs of IAV correlations----------------------------
Merged_Jung_Comp <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
Merged_Jung_Comp <- Merged_Jung_Comp[!(is.na(Merged_Jung_Comp$year)),] 
summary(Merged_Jung_Comp)
Merged_Jung_Comp$date <- as.Date(Merged_Jung_Comp$date, format="%Y-%m-%d")
Merged_Jung_Comp$month <- month(Merged_Jung_Comp$date)
Merged_Jung_Comp$year <- year(Merged_Jung_Comp$date)

#Delete all files after 2013 (Fluxcom only goes through 2013)
Merged_Jung_Comp<-Merged_Jung_Comp[!(Merged_Jung_Comp$year > 2013 | Merged_Jung_Comp$year < 1999),]
new_DF <- Merged_Jung_Comp[is.na(Merged_Jung_Comp$Jung_GPP),]
#Double check there's no NA's in here
new_DF
summary(Merged_Jung_Comp)
str(Merged_Jung_Comp)

#split apply combine (with ddply) to get annual and seasonal sums of GPP (IAV)
out <- split(Merged_Jung_Comp, Merged_Jung_Comp$site.x)
str(out)
summer <- subset(Merged_Jung_Comp, month==6 | month==7 | month==8)
summer_out <- split(summer, summer$site.x)
spring <- subset(Merged_Jung_Comp, month==3 | month==4 | month==5)
spring_out <- split(spring, spring$site.x)
fall <- subset(Merged_Jung_Comp, month==9 | month==10 | month==11)
fall_out <- split(fall, fall$site.x)
winter <- subset(Merged_Jung_Comp, month==12 | month==1 | month==2)
winter_out <- split(winter, winter$site.x)

IAV_func <- function(x){
  require(plyr)
  df <- ddply(x, .(year, site.x), summarize, Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
  return(df)
}


rmssdfunc <- function(xx)
{require(psych)
  return(data.frame(RMSSDGPP = rmssd(xx$GPP), RMSSDJungGPP= rmssd(xx$Jung_GPP)))
}

RMSSD_to_plot <- do.call(rbind, lapply(out, rmssdfunc))
IAV_to_plot <- do.call(rbind, lapply(out, IAV_func))
str(IAV_to_plot)
summer_to_plot <- do.call(rbind, lapply(summer_out, IAV_func))
spring_to_plot <- do.call(rbind, lapply(spring_out, IAV_func))
winter_to_plot <- do.call(rbind, lapply(winter_out, IAV_func))
fall_to_plot <- do.call(rbind, lapply(fall_out, IAV_func))

#7. Plots of rmssd & seasonal variability ------------------------
library(ggplot2)
library(psych)
library(ggpubr)
library(stringr)
library(lubridate)
library(ggthemes)
library(plyr)
library(scales)
Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
Sites$site <- str_replace_all(Sites$site, "-", "_")
str(Merged_Jung_Comp)
Merged_Jung_Comp$diff <- Merged_Jung_Comp$GPP - Merged_Jung_Comp$Jung_GPP
names(Merged_Jung_Comp)[names(Merged_Jung_Comp) == 'site.x'] <- 'site'
Sites <- Sites[,c("site", "Vegtype")]
Veg_merge <- plyr::rename(Sites, c("site"="site", "Vegtype"="Veg"))
Residual_plot <- merge(Merged_Jung_Comp, Veg_merge, by="site")

#Split apply combine plots
Y <- split(Residual_plot, Residual_plot$Veg)
Forest <- Y[[1]]
Grassland <- Y[[2]]
Savanna <- Y[[3]]
Shrubland <- Y[[4]]
str(residplot)

stdev <- ddply(Residual_plot, .(Veg), summarize, std_dev=sd(diff,na.rm=TRUE))
formax <- (stdev[1,2]*1.96)
formin <- -(stdev[1,2]*1.96)
grassmax <- (stdev[2,2]*1.96)
grassmin <- -(stdev[2,2]*1.96)
shrubmax <- (stdev[4,2]*1.96)
shrubmin <- -(stdev[4,2]*1.96)
savmax <- (stdev[3,2]*1.96)
savmin <- -(stdev[3,2]*1.96)

ggplot(Residual_plot, aes(x=month, y=diff, color=Veg)) + geom_point(size=2) +geom_smooth(aes(group=Veg), method="loess", se=FALSE)+theme_few() +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ ylab("FluxGPP - JungGPP")+ scale_x_continuous(breaks=pretty_breaks())
Fo <- ggplot(Forest, aes(x=month, y=diff, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() +geom_line(aes(y=0), linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+geom_line(aes(y=(formax)), linetype="dotted", color="black")+geom_line(aes(y=(formin)), linetype="dotted", color="black")
Gra <- ggplot(Grassland, aes(x=month, y=diff, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(grassmax)), linetype="dotted", color="black")+geom_line(aes(y=(grassmin)), linetype="dotted", color="black")
Sav <- ggplot(Savanna, aes(x=month, y=diff, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(savmax)), linetype="dotted", color="black")+geom_line(aes(y=(savmin)), linetype="dotted", color="black")
Shr <- ggplot(Shrubland, aes(x=month, y=diff, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(shrubmax)), linetype="dotted", color="black")+geom_line(aes(y=(shrubmin)), linetype="dotted", color="black")
#apply (and write out)

ggarrange(Fo, Gra, Sav, Shr + rremove("x.text"), 
          labels = c("Forest", "Grass", "Savanna", "Shrub"), vjust=1.5, hjust=-0.4, 
          legend="none",
          ncol = 2, nrow = 2)

#Correion plot of differences against GPP
ggplot(Residual_plot, (aes(x=GPP, y=diff, color=Veg)))+geom_point(size=2)+ geom_smooth(method="lm")+ theme_few()+ylab("FluxGPP - JungGPP")+xlab("FluxGPP")
cor(Residual_plot$GPP, Residual_plot$diff, use="complete.obs")


library(stringr)
str(RMSSD_to_plot)
RMSSD_to_plot$site <- rownames(RMSSD_to_plot)
RMSSD_to_plot$site <- as.factor(str_replace_all(RMSSD_to_plot$site, "us_ray", "mx_ray"))
RMSSD_to_plot$site <- as.factor(str_replace_all(RMSSD_to_plot$site, "us_tes", "mx_tes"))

Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
str(Sites)
levels(Sites$site)
Sites$site <- str_replace_all(Sites$site, "-", "_")
levels(RMSSD_to_plot$site)

merged_to_plot <- merge(RMSSD_to_plot, Sites, by="site")
RMSSD_to_plot$diff <- (RMSSD_to_plot$RMSSDGPP - RMSSD_to_plot$RMSSDJungGPP)
RMSSD_to_plot

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", regions=c("Mexico", "USA"), colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
?borders
str(RMSSD_to_plot)
summary(RMSSD_to_plot)

mp_plot <- mp+ geom_point(aes(x=merged_to_plot$long, y=merged_to_plot$, colour=merged_to_plot$diff), position= position_jitter(w=0.3, h=0.3), size=2.5)+
  scale_color_gradientn(colours=c("blue", "red"), name="RMSSDflux - RMSSDJung")+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))

mp_plot+ theme_few(base_size=14)+ labs(title = "RMSSD comparison", x="Longitude", y="itude") 

#ddply to get seasonal sums of GPP (Summer vs. spring vs. winter vs. fall)
#IAV Plots: corplots and bubble plots

IAVplot_func <- function(xx){
  require(ggplot2)
  require(stringr)
  require(plyr)
  require(psych)
  
  Sites <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
  Sites$site <- str_replace_all(Sites$site, "-", "_")
  

  names(xx)[names(xx) == 'site.x'] <- 'site'
  xx$site <- as.factor(str_replace_all(as.character(xx$site), "us_ray", "mx_ray"))
  xx$site <- as.factor(str_replace_all(as.character(xx$site), "us_tes", "mx_tes"))
  
  levels(xx$site)
  levels(Sites$site)
  
  xxmerge <- merge(xx, Sites, by="site")

  corfunc <- function(gg){
    require(plyr)
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabue(match(v, uniqv)))]
    }
    
    return(data.frame(COR = cor(gg$GPP, gg$Jung_GPP, use="complete.obs"), pval=cor.test(gg$GPP, gg$Jung_GPP)$p.value, MAP=getmode(gg$MAP), RMSSD=rmssd(gg$GPP)))
  }
  xxplot <- ddply(xxmerge, .(site), corfunc)
  summary(xxplot)
  xxplot$sig <- ifelse(xxplot$pval <0.05, "Significant", "nonsignificant")
  
  print(summary(xxplot))
  
  p <- ggplot(xxplot, aes(x=MAP, y=COR, color=sig)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few() 
  plot(p)
  }

IAVplot_func(IAV_to_plot)
IAVplot_func(spring) 
IAVplot_func(summer) 
IAVplot_func(fall) 
IAVplot_func(winter) 

IAVBubblePlot<- function(xx){
  require(ggplot2)
  require(stringr)
  require(plyr)
  require(psych)
  
  Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
  Sites$site <- str_replace_all(Sites$site, "-", "_")
  
  
  names(xx)[names(xx) == 'site.x'] <- 'site'
  xx$site <- as.factor(str_replace_all(as.character(xx$site), "us_ray", "mx_ray"))
  xx$site <- as.factor(str_replace_all(as.character(xx$site), "us_tes", "mx_tes"))
  
  xxmerge <- merge(xx, Sites, by="site")
  print(str(xxmerge))
  
  corfunc <- function(gg){
    require(plyr)
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabue(match(v, uniqv)))]
    }
    
    return(data.frame(COR = cor(gg$GPP, gg$Jung_GPP, use="complete.obs"), pval=cor.test(gg$GPP, gg$Jung_GPP)$p.value, RMSSD=rmssd(gg$GPP)))
  }
  
  xxplot <- ddply(xxmerge, .(site), corfunc)
  summary(xxplot)
  xxplot$sig <- ifelse(xxplot$pval <0.05, "Significant", "nonsignificant")
  xxbubble <- merge(xxmerge, xxplot, by="site")
  print(str(xxbubble))

  print(summary(xxbubble))
  
  p <- ggplot(xxbubble, aes(x=GPP, y=Jung_GPP, color=sig)) + geom_point(size=2) +geom_smooth(aes(group=site), method="lm", se=FALSE)+theme_few() 
  plot(p)
}

  
IAVBubblePlot(IAV_to_plot)

#8. Now looking at the Barnes et al. 2018 upscaled flux data-----------------------------
library(raster)
library(plyr)
library(tibble)
#Function should: 
#Read all files together from given site

format_Barnesoutput <- function(xx){
  require("raster")
  require("lubridate")
  #get function filename
  filename <- tools::file_path_sans_ext(basename(xx))
  #read in site 
  xx <- raster(xx)
  print(filename)
  site <- substr(filename,12,15)
  site_df <- data.frame(site=substr(filename,13,15))
  #some sort of lookup table is needed 
  lookup <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
  lookup$sitechar <- substr(lookup$site, 4,6)
  point <- data.frame("site" = lookup$sitechar, 
                      "lat" = lookup$lat, 
                      "long" = lookup$long)
  merged <- merge(site_df, point, by="site")
  sitepoint <- cbind(as.numeric(merged$long), as.numeric(merged$lat))
  print(sitepoint)
  merged$result <- extract(xx, sitepoint)
  merged$stats <- cellStats(xx,stat='mean', na.rm=TRUE)
  merged$file <- filename
  merged$month <- tolower(substr(merged$file, 1,3))
  merged$year<-substr(merged$file, 5,8)
  merged$site <- substr(merged$file, 10,15)
  merged$monthyear <- paste(merged$month, merged$year, sep="_")
  merged$date <- as.Date(paste("01", merged$monthyear, sep="_"), format="%d_%b_%Y")
  print(merged)
  return(merged)
}

setwd("F:/Upscaling_Project/Upscaled_GPP/RF_F3/")
RFF3Files <- list.files(pattern="*.tif", all.files = TRUE)
RF_F3 <- do.call("rbind", lapply(RFF3Files, format_Barnesoutput))
RF_F3[with(RF_F3, order(site, date)), ]
str(RF_F3)
write.csv(RF_F3, "F:/Upscaling_Project/Upscaled_GPP/RF_F3_2001_2013.csv")

setwd("F:/Upscaling_Project/Upscaled_GPP/RF_F4/")
RFF4Files <- list.files(pattern="*.tif", all.files = TRUE)
RF_F4 <- do.call("rbind", lapply(RFF4Files, format_Barnesoutput))
RF_F4[with(RF_F4, order(site, date)), ]
str(RF_F4)
write.csv(RF_F4, "F:/Upscaling_Project/Upscaled_GPP/RF_F4_2001_2013.csv")

setwd("F:/Upscaling_Project/Upscaled_GPP/RF_T3/")
RFT3Files <- list.files(pattern="*.tif", all.files = TRUE)
RF_T3 <- do.call("rbind", lapply(RFT3Files, format_Barnesoutput))
RF_T3[with(RF_T3, order(site, date)), ]
str(RF_T3)
write.csv(RF_T3, "F:/Upscaling_Project/Upscaled_GPP/RF_T3_2001_2013.csv")


setwd("F:/Upscaling_Project/Upscaled_GPP/RF_T4/")
RFT4Files <- list.files(pattern="*.tif", all.files = TRUE)
RF_T4 <- do.call("rbind", lapply(RFT4Files, format_Barnesoutput))
RF_T4[with(RF_T4, order(site, date)), ]
str(RF_T4)
write.csv(RF_T4, "F:/Upscaling_Project/Upscaled_GPP/RF_T4_2001_2013.csv")

#9. Compare my models to real flux data ----------------------------------
RF_F3 <- read.csv("F:/Upscaling_Project/Upscaled_GPP/RF_F3_2001_2013.csv")
RF_F3 <- plyr::rename(RF_F3, c("result"="RFF3result", "stats"="RFF3mean"))
RF_T3 <- read.csv("F:/Upscaling_Project/Upscaled_GPP/RF_T3_2001_2013.csv")
RF_T3 <- plyr::rename(RF_T3, c("result"="RFT3result", "stats"="RFT3mean"))
RF_F4<- read.csv("F:/Upscaling_Project/Upscaled_GPP/RF_F4_2001_2013.csv")
RF_F4 <- plyr::rename(RF_F4, c("result"="RFF4result", "stats"="RFF4mean"))
RF_T4<- read.csv("F:/Upscaling_Project/Upscaled_GPP/RF_T4_2001_2013.csv")
RF_T4 <- plyr::rename(RF_T4, c("result"="RFT4result", "stats"="RFT4mean"))

RFint <- merge(RF_F3, RF_F4[, c("RFF4result", "RFF4mean", "file")], by="file")
RFint2<- merge(RFint, RF_T3[, c("RFT3result", "RFT3mean", "file")], by="file")
RFall <- merge(RFint2,RF_T4[, c("RFT4result", "RFT4mean", "file")], by="file")
head(RFall)
str(RFall)
RFall <- subset(RFall, select=-c(X))
#Merge with fluxfiles 
RFall <- read.csv("F:/Upscaling_Project/Upscaled_GPP/myRFmodels.csv")
Flux_files <- read.csv("F:/Upscaling_Project/Biederman_Flux/Fixed_flux_vars_3_2_2018.csv")
#Merge by "site_date" column
str(Flux_files)
Flux_files$site <- str_replace_all(Flux_files$site, "us-soy", "us-so3")
Flux_files$site <- str_replace_all(Flux_files$site, "us-sob", "us-so2")
Flux_files$site <- str_replace_all(Flux_files$site, "us-son", "us-so4")
Flux_files$site <- str_replace_all(Flux_files$site, "us-soo", "us-so2")
Flux_files$monthyear <- (gsub('([[:punct:]])|\\-','_',Flux_files$monthyear))
Flux_files$site <- (gsub('([[:punct:]])|\\-','_',Flux_files$site))
Flux_files$site <- as.factor(Flux_files$site)
Flux_files$file <- as.factor(paste(Flux_files$monthyear, Flux_files$site, sep="_"))
levels(Flux_files$file)
levels(RFall$file)
Flux_files_merge <- Flux_files[,c("file", "GPP")]
RFall2 <- merge.with.order(RFall, Flux_files_merge, by="file", keep_order=1)
head(RFall2)
str(RFall2)
write.csv(RFall2, "F:/Upscaling_Project/Upscaled_GPP/myRFmodelswithfluxGPP_3_9.csv")

#Starting with RFall2 (saved version)
RFall2 <- read.csv("F:/Upscaling_Project/Upscaled_GPP/myRFmodelswithfluxGPP.csv")
str(RFall2)
toBeRemoved<-which(RFall2$site=="mx_ray" | RFall2$site=="mx_tes" | RFall2$site == "us_lpa") 
RFall2<-RFall2[-toBeRemoved,] 

IAV_func2 <- function(x){
  require(plyr)
  require(lubridate)
  df <- ddply(x, .(year, site), summarize, RFF3result=sum(RFF3result, na.rm=TRUE), RFF4result=sum(RFF4result, na.rm=TRUE), RFT4result=sum(RFT4result, na.rm=TRUE), 
              RFT3result=sum(RFT3result,na.rm=TRUE), RFF3mean=sum(RFF3mean, na.rm=TRUE),RFT3mean=sum(RFT3mean, na.rm=TRUE),RFF4mean=sum(RFF4mean, na.rm=TRUE),RFT4mean=sum(RFT4mean, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
  df$date <- as.Date(paste("01_01", as.character(df$year), sep="_"), format="%d_%m_%Y")
  return(df)
}

IAVplot_myRF_func <- function(xx, yy){
  require(ggplot2)
  require(stringr)
  require(plyr)
  require(psych)
  require(ggthemes)
  
  Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
  Sites$site <- as.factor(str_replace_all(Sites$site, "-", "_"))
  
  result <-paste(yy, "result", sep="")
  scene <- paste(yy, "mean", sep="")
  
  xxtomerge <- xx[, c(eval(as.character(result)), eval(as.character(scene)), "GPP", "site", "date", "year")]
  
  merge.with.order <- function(x,y, ..., sort = T, keep_order)
  {
    #Function from here: https://www.r-statistics.com/2012/01/merging-two-data-frame-objects-while-preserving-the-rows-order/
    # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
    add.id.column.to.data <- function(DATA)
    {
      data.frame(DATA, id... = seq_len(nrow(DATA)))
          }
    # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
    order.by.id...and.remove.it <- function(DATA)
    {
      # gets in a data.frame with the "id..." column.  Orders by it and returns it
      if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
      
      ss_r <- order(DATA$id...)
      ss_c <- colnames(DATA) != "id..."
      DATA[ss_r, ss_c]
    }
    
    # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
    # tmp()
    
    if(!missing(keep_order))
    {
      if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
      if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
      # if you didn't get "return" by now - issue a warning.
      warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
    } else {return(merge(x=x,y=y,..., sort = sort))}
  }
  
  
  xxmerge <- merge.with.order(xxtomerge, Sites, by="site", all.x=TRUE, keep_order = 1)

  
  corfunc <- function(gg){
    require(plyr)
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
   return(data.frame(CORpoint = cor(gg$GPP, gg[,2], use="complete.obs"), pvalpoint=cor.test(gg$GPP, gg[,2])$p.value, 
                    CORscene=cor(gg$GPP, gg[,3], use="complete.obs"), pvalscene=cor.test(gg$GPP, gg[,3])$p.value, MAP=getmode(gg$MAP), RMSSD=rmssd(gg$GPP)))
  }
  
  
  xxplot <- ddply(xxmerge, .(site), corfunc)
  summary(xxplot)
  xxplot$sigpoint <- ifelse(xxplot$pvalpoint <0.05, "Significant", "nonsignificant")
  xxplot$sigscene <- ifelse(xxplot$pvalscene <0.05, "Significant", "nonsignificant")
  
  print(summary(xxplot))
  
  p <- ggplot(xxplot, aes(x=MAP, y=CORpoint, color=sigpoint)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few()+ggtitle(paste(eval(yy), "point", sep=" ")) 
  plot(p)
  
  q <- ggplot(xxplot, aes(x=MAP, y=CORscene, color=sigscene)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few()+ggtitle(paste(eval(yy), "scene", sep=" ")) 
  plot(q)
}

RFIAV <- IAV_func2(RFall2)

toBeRemoved2 <- which(RFIAV$GPP == 0)
RFIAV<-RFIAV[-toBeRemoved2,] 


IAVplot_myRF_func(RFIAV, "RFF3")

IAVplot_myRF_func(RFIAV, "RFF4")

IAVplot_myRF_func(RFIAV, "RFT3")

IAVplot_myRF_func(RFIAV, "RFT4")

#RF model seasonal comparisons-------------------------------------------

Merged_RF_Comp <- read.csv("F:/Upscaling_Project/Upscaled_GPP/myRFmodelswithfluxGPP_3_9.csv")
Merged_RF_Comp <- Merged_RF_Comp[!(is.na(Merged_RF_Comp$year)),] 
Merged_RF_Comp$date <- as.Date(Merged_RF_Comp$date, format="%Y-%m-%d")
Merged_RF_Comp$month <- month(Merged_RF_Comp$date)
Merged_RF_Comp$year <- year(Merged_RF_Comp$date)
Merged_RF_Comp$site <- as.character(Merged_RF_Comp$site)
#Delete all files after 2013 (Fluxcom only goes through 2013)
toBeRemoved3<-which(Merged_RF_Comp$site=="mx_ray" | Merged_RF_Comp$site=="mx_tes" |Merged_RF_Comp$site== "us_lpa") 
Merged_RF_Comp<-Merged_RF_Comp[-toBeRemoved3,] 
Merged_RF_Comp<-Merged_RF_Comp[!(Merged_RF_Comp$year > 2013 | Merged_RF_Comp$year < 1999),]
new_DF <- Merged_RF_Comp[is.na(Merged_RF_Comp$GPP),]
#Check there's no NAs
new_DF
summary(Merged_RF_Comp)
Merged_RF_Comp$site <- as.factor(Merged_RF_Comp$site)
levels(Merged_RF_Comp$site)
#Split - apply - combine 
#split
out <- split(Merged_RF_Comp, Merged_RF_Comp$site)
str(out)

#Apply
seasonal_func <- function(x){
  df <- ddply(x, .(month, site), summarize, RFF3point_se=sd(RFF3result, na.rm=TRUE)/sqrt(length(RFF3result[!is.na(RFF3result)])) , RFF3point=mean(RFF3result, na.rm=TRUE),  RFF3scene=mean(RFF3mean, na.rm=TRUE), RFF3scene_se=sd(RFF3mean, na.rm=TRUE)/sqrt(length(RFF3mean[!is.na(RFF3mean)])) , 
              RFF4point_se=sd(RFF4result, na.rm=TRUE)/sqrt(length(RFF4result[!is.na(RFF4result)])) , RFF4point=mean(RFF4result, na.rm=TRUE),  RFF4scene=mean(RFF4mean, na.rm=TRUE), RFF4scene_se=sd(RFF4mean, na.rm=TRUE)/sqrt(length(RFF4mean[!is.na(RFF4mean)])) , 
              RFT3point_se=sd(RFT3result, na.rm=TRUE)/sqrt(length(RFT3result[!is.na(RFT3result)])) , RFT3point=mean(RFT3result, na.rm=TRUE),  RFT3scene=mean(RFT3mean, na.rm=TRUE), RFT3scene_se=sd(RFT3mean, na.rm=TRUE)/sqrt(length(RFT3mean[!is.na(RFT3mean)])) , 
              RFT4point_se=sd(RFT4result, na.rm=TRUE)/sqrt(length(RFT4result[!is.na(RFT4result)])) , RFT4point=mean(RFT4result, na.rm=TRUE),  RFT4scene=mean(RFT4mean, na.rm=TRUE), RFT4scene_se=sd(RFT4mean, na.rm=TRUE)/sqrt(length(RFT4mean[!is.na(RFT4mean)])) , 
              GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])), GPP=mean(GPP, na.rm=TRUE))
  return(df)
  
}

lapply(out, seasonal_func)

#Combine
seasonal_to_plot <- do.call(rbind, lapply(out, seasonal_func))

#Plotting function that plots all sites serparately and writes out graphs
#Where to write out .png files
setwd("F:/Upscaling_Project/Upscaled_GPP/")
list_seasonsRF <- split(seasonal_to_plot, seasonal_to_plot$site)
str(seasonal_to_plot)
plot_seasonal_cycle_RF <- function(x){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  require("ggpubr")
  print(str(x))
  droplevels(x)
  r<- as.character(round(cor(x$GPP, x$RFF3point, use="complete.obs"), 3))
  s<- as.character(round(cor(x$GPP, x$RFF3scene, use="complete.obs"), 3))
  t<- as.character(round(cor(x$GPP, x$RFT3point, use="complete.obs"), 3))
  u<- as.character(round(cor(x$GPP, x$RFT3scene, use="complete.obs"), 3))
  v<- as.character(round(cor(x$GPP, x$RFF4point, use="complete.obs"), 3))
  w<- as.character(round(cor(x$GPP, x$RFF4scene, use="complete.obs"), 3))
  y<- as.character(round(cor(x$GPP, x$RFT4point, use="complete.obs"), 3))
  z<- as.character(round(cor(x$GPP, x$RFT4scene, use="complete.obs"), 3))
  
  print("calculated cors")
  rmssdGPP <- as.character(round(rmssd(x$GPP), 3))
  rmssdR <- as.character(round(rmssd(x$RFF3point), 3))
  rmssdS <- as.character(round(rmssd(x$RFF3scene), 3))
  rmssdT <- as.character(round(rmssd(x$RFT3point), 3))
  rmssdU <- as.character(round(rmssd(x$RFT3scene), 3))
  rmssdV <- as.character(round(rmssd(x$RFF4point), 3))
  rmssdW <- as.character(round(rmssd(x$RFF4scene), 3))
  rmssdY <- as.character(round(rmssd(x$RFT4point), 3))
  rmssdZ <- as.character(round(rmssd(x$RFT4scene), 3))
  
  print("got RMSSD")
  lblGPP <- paste("rmssdFlux =", rmssdGPP)
  lblR <- paste("rmssdRFpoint =", rmssdR)
  lblS <- paste("rmssdRFscene =", rmssdS)
  lblT <- paste("rmssdRFpoint =", rmssdT)
  lblU <- paste("rmssdRFscene =", rmssdU)
  lblV <- paste("rmssdRFpoint =", rmssdV)
  lblW <- paste("rmssdRFscene =", rmssdW)
  lblY <- paste("rmssdRFpoint =", rmssdY)
  lblZ <- paste("rmssdRFscene =", rmssdZ)
  
  filename <- paste(x$site[1], "seasonal_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(paste(x$site, "RFF3", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = RFF3point, color = I("blue")), size=2) +
    geom_line(data = x, aes(x = month, y = RFF3scene, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=RFF3point+ RFF3point_se, ymax=RFF3point+RFF3point_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=RFF3scene+ RFF3scene_se, ymax=RFF3scene+RFF3scene_se),colour="green")+
    annotate("text", label = lblGPP, parse=FALSE, x =3, y = 4.5, size = 5, colour = "Black")+
    annotate("text", label = lblR, parse=FALSE, x = 3, y = 5, size = 5, colour = "Black")+
    annotate("text", label = lblS, parse=FALSE, x = 3, y = 5.5, size = 5, colour = "Black")+
    
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  
  c <- ggplot() +
    ggtitle(paste(x$site, "RFF4", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = RFF4point, color = I("blue")), size=2) +
    geom_line(data = x, aes(x = month, y = RFF4scene, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=RFF4point+ RFF4point_se, ymax=RFF4point+RFF4point_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=RFF4scene+ RFF4scene_se, ymax=RFF4scene+RFF4scene_se),colour="green")+
    annotate("text", label = lblGPP, parse=FALSE, x =3, y = 4.5, size = 5, colour = "Black")+
    annotate("text", label = lblV, parse=FALSE, x = 3, y = 5, size = 5, colour = "Black")+
    annotate("text", label = lblW, parse=FALSE, x = 3, y = 5.5, size = 5, colour = "Black")+
    
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))

  
  a <- ggplot() +
    ggtitle(paste(x$site, "RFT3", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = RFT3point, color = I("blue")), size=2) +
    geom_line(data = x, aes(x = month, y = RFT3scene, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=RFT3point+ RFT3point_se, ymax=RFT3point+RFT3point_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=RFT3scene+ RFT3scene_se, ymax=RFT3scene+RFT3scene_se),colour="green")+
    annotate("text", label = lblGPP, parse=FALSE, x =3, y = 4.5, size = 5, colour = "Black")+
    annotate("text", label = lblT, parse=FALSE, x = 3, y = 5, size = 5, colour = "Black")+
    annotate("text", label = lblU, parse=FALSE, x = 3, y = 5.5, size = 5, colour = "Black")+
    
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))

  b <- ggplot() +
    ggtitle(paste(x$site, "RFT4", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = RFT4point, color = I("blue")), size=2) +
    geom_line(data = x, aes(x = month, y = RFT4scene, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=RFT4point+ RFT4point_se, ymax=RFT4point+RFT4point_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=RFT4scene+ RFT4scene_se, ymax=RFT4scene+RFT4scene_se),colour="green")+
    annotate("text", label = lblGPP, parse=FALSE, x =3, y = 4.5, size = 5, colour = "Black")+
    annotate("text", label = lblY, parse=FALSE, x = 3, y = 5, size = 5, colour = "Black")+
    annotate("text", label = lblZ, parse=FALSE, x = 3, y = 5.5, size = 5, colour = "Black")+
    
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))

  ggarrange(q,a,b,c, ncol=2, nrow=2)
  filename <- paste(x$site[1], "seasonalRF_comparison.png", sep="_")
  ggsave(filename, device='png', width=16, height=16, dpi = 300, units = "cm")
}
#split
#apply (and write out)
lapply(list_seasonsRF, plot_seasonal_cycle_RF)

