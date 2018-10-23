library(R.matlab)
library(lubridate)
library(plyr)
setwd("C:/Users/rsstudent/Upscaling_Data/Biederman_Flux/")
fluxlist <-list.files(pattern= "^.*\\.(mat)$")

format_flux <- function(file){
 filename<-paste("us", substr(file, 7,9), sep="-")
 filename <- paste(filename, ".csv", sep="")
 file <- readMat(file)
 print(filename)
 data = lapply(file, unlist, use.names=FALSE)
 df <- as.data.frame(data)
 names(df) <- c("year", "dohy", "NEP", "GEP", "Reco", "ET", "Precip", "Tair", "VPD", "Rnet", "Rsolar")
 df$datewy <- strptime(paste(df$dohy, df$year, sep="-"), format="%j-%Y")
 #Substract 7884000 because that's the number of seconds in 3 months
 df$date <- as.Date(df$datewy - 7884000)
 #A little funny because of differing #s of days in months but ok for now. 
 df[394:399,]
 #get rid of datewy, year, dohy columsn do avoid confusion
 df <- subset(df, select=-c(year, dohy, datewy))
 #Ddply by month and year to get monthly fluxes
 df$month <- month.abb[month(df$date)]
 df$year <- year(df$date)
 df$monthyear <- paste(df$month, df$year, sep="-")
 #Get Monthly GPP (sum), monthly Reco(sum), NEP(sum), ET (sum), precip (sum), Tair, VPD, Rnet, Rsolar
 cleaned_df <- ddply(df, .(monthyear), summarize, GPP=sum(GEP, na.rm=TRUE), Reco=sum(Reco, na.rm=TRUE), ET=sum(ET, na.rm=TRUE), Precip=sum(Precip, na.rm=TRUE), Tair=mean(Tair, na.rm=TRUE), VPD=mean(VPD, na.rm=TRUE))
 write.csv(cleaned_df, file=filename)
 return(cleaned_df)}


lapply(fluxlist, format_flux)
?lapply
