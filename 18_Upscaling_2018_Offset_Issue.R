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
All <- do.call("rbind", lapply(l.df, SPEI_calc))
str(All)


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
IGBP_lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
str(IGBP_lookup)
IGBP_lookup
All_inc_IGBP <- merge(Merged_all, IGBP_lookup, by="site", all.x=T)
str(All_inc_IGBP)
All_inc_IGBP <- subset(All_inc_IGBP, select = -c(IGBP.x))
ALL_inc_IGPB <- plyr::rename(All_inc_IGBP, c("IGBP.y"="IGBP"))
str(ALL_inc_IGPB)

#Need to get tmed
ALL_inc_IGPB$tmed <- ((ALL_inc_IGPB$tmax + ALL_inc_IGPB$tmin)/2)
write.csv(ALL_inc_IGPB, "C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_2_14_2018.csv")

#Calculate 1-month SPEI --------------------------------------------
#1 Calculate water balance using 'spei' package for site-based RF------

library(SPEI)
Allsites <- rename(Allsites, c("lat"="Latitude", "long"="Longitude"))
#probably should do 'split-apply-combine'
Allsites$site
#split 

X <- split(Allsites, Allsites$site)

A  <- X[[1]]
B  <- X[[2]]
C  <- X[[3]]
D  <- X[[4]]
E  <- X[[5]]
FF  <- X[[6]]
G  <- X[[7]]
H  <- X[[8]]
I  <- X[[9]]
J  <- X[[10]]
K  <- X[[11]]
L  <- X[[12]]
M  <- X[[13]]
N  <- X[[14]]
O  <- X[[15]]
P  <- X[[16]]
Q  <- X[[17]]
R  <- X[[18]]
S  <- X[[19]]
TT  <- X[[20]]
U  <- X[[21]]
V<- X[[22]]
W<- X[[23]]
XX<- X[[24]]

#Apply

A$PET <- thornthwaite(A$tmed, A$Latitude[1], na.rm=TRUE)
A$BAL <- A$precip - A$PET
A$spei1 <-spei(A[,'BAL'],1, na.rm=TRUE) 

B$PET <- thornthwaite(B$tmed, B$Latitude[1], na.rm=TRUE)
B$BAL <- B$precip - B$PET
B$spei1$ <-spei(B[,'BAL'],1, na.rm=TRUE)
C$PET <- thornthwaite(C$tmed, C$Latitude[1], na.rm=TRUE)
C$BAL <- C$precip - C$PET
C$spei1 <-spei(C[,'BAL'],1) 

D$PET <- thornthwaite(D$tmed, D$Latitude[1], na.rm=TRUE)
D$BAL <- D$precip - D$PET
D$spei1 <-spei(D[,'BAL'],1) 

E$PET <- thornthwaite(E$tmed, E$Latitude[1], na.rm=TRUE)
E$BAL <- E$precip - E$PET
E$spei1 <-spei(E[,'BAL'],1) 

FF$PET <- thornthwaite(FF$tmed, FF$Latitude[1], na.rm=TRUE)
FF$BAL <- FF$precip - FF$PET
FF$spei1 <-spei(FF[,'BAL'],1) 

G$PET <- thornthwaite(G$tmed, G$Latitude[1], na.rm=TRUE)
GBAL <- G$precip - G$PET
G$spei1 <-spei(GG[,'BAL'],1) 

H$PET <- thornthwaite(H$tmed, H$Latitude[1], na.rm=TRUE)
H$BAL <- H$precip - H$PET
H$spei1 <-spei(H[,'BAL'],1) 

I$PET <- thornthwaite(H$tmed, H$Latitude[1], na.rm=TRUE)
I$BAL <- I$precip - I$PET
I$spei1 <-spei(I[,'BAL'],1) 

J$PET <- thornthwaite(J$tmed, J$Latitude[1], na.rm=TRUE)
J$BAL <- J$precip - J$PET
J$spei1 <-spei(J[,'BAL'],1) 

K$PET <- thornthwaite(K$tmed, K$Latitude[1], na.rm=TRUE)
K$BAL <- K$precip - K$PET
K$spei1 <-spei(K[,'BAL'],1) 

L$PET <- thornthwaite(L$tmed, L$Latitude[1], na.rm=TRUE)
L$BAL <- L$precip - L$PET
L$spei1 <-spei(L[,'BAL'],1) 

M$PET <- thornthwaite(M$tmed, M$Latitude[1], na.rm=TRUE)
M$BAL <- M$precip - M$PET
M$spei1 <-spei(M[,'BAL'],1) 

N$PET <- thornthwaite(N$tmed, N$Latitude[1], na.rm=TRUE)
N$BAL <- N$precip - N$PET
N$spei1 <-spei(N[,'BAL'],1) 

O$PET <- thornthwaite(O$tmed, O$Latitude[1], na.rm=TRUE)
O$BAL <- O$precip - O$PET
O$spei1 <-spei(O[,'BAL'],1) 

P$PET <- thornthwaite(P$tmed, P$Latitude[1], na.rm=TRUE)
P$BAL <- P$precip - P$PET
P$spei1 <-spei(P[,'BAL'],1) 

Q$PET <- thornthwaite(Q$tmed, Q$Latitude[1], na.rm=TRUE)
Q$BAL <- Q$precip - Q$PET
Q$spei1 <-spei(Q[,'BAL'],1) 

R$PET <- thornthwaite(R$tmed, R$Latitude[1], na.rm=TRUE)
R$BAL <- R$precip - R$PET
R$spei1 <-spei(R[,'BAL'],1) 

S$PET <- thornthwaite(S$tmed, S$Latitude[1], na.rm=TRUE)
S$BAL <- S$precip - S$PET
S$spei1 <-spei(S[,'BAL'],1) 

TT$PET <- thornthwaite(TT$tmed, TT$Latitude[1], na.rm=TRUE)
TT$BAL <- TT$precip - TT$PET
TT$spei1 <-spei(TT[,'BAL'],1) 

U$PET <- thornthwaite(U$tmed, U$Latitude[1], na.rm=TRUE)
U$BAL <- U$precip - U$PET
U$spei1 <-spei(U[,'BAL'],1) 

V$PET <- thornthwaite(V$tmed, V$Latitude[1], na.rm=TRUE)
V$BAL <- V$precip - V$PET
V$spei1 <-spei(V[,'BAL'],1) 

W$PET <- thornthwaite(W$tmed, W$Latitude[1], na.rm=TRUE)
W$BAL <- W$precip - W$PET
W$spei1 <-spei(W[,'BAL'],1) 

XX$PET <- thornthwaite(XX$tmed, XX$Latitude[1], na.rm=TRUE)
XX$BAL <- XX$precip - XX$PET
XX$spei1 <-spei(XX[,'BAL'],1) 

All_sites2 <- rbind(A,B,C,D,E,FF,G,H,I,J,K,L,M,N,O,P,Q,R,S,TT,U,V,W,XX)
str(All_sites2)
write.csv(All_sites2, "D:/Upscaling_Project/Site_based_RF/Upscaling_All_Sites_2_14.csv")

