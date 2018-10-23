## Preapre Upscaling by site
## Load Daymet files and aggregate to monthlhy
## Then combine daymet files with flux files
## Then append all in one big .csv for upscaling purposes 

setwd("C:/Users/Mallory/odrive/UA_Google_Drive/Daymet/")

daylist=list.files(getwd(), pattern=".*\\.csv$", full.names=FALSE)

test <- read.csv("us-aud.csv", skip=6)
str(test)


Aggregate_monthly <- function(x) {

  
  out <- Aggregate_monthly(x)
}

Aggregate_monthly(test)

Aggregate_monthly(daylist[1])

lapply(daylist, function(x){
  t <- read.csv(x, skip=6)
  
  out <- function(t)
  write.csv(out, "path/to/output", sep="\t", quote=F, row.names=F, col.names=T)
  
})

#Functon from my previous code 
Formatday <- function(x) {
  x$Date <- format(strptime(x$yday, format="%j"), format="%m-%d")
  x$Date <-paste(x$year, x$Date, sep = "_")
  print(str(x))
}

Formatday(test)
