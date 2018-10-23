library(ggplot2)
library(plyr)
library(lubridate)

file <- read.csv("C:/Users/Mallory/Documents/Flux_Plus_Jung/Flux_Plus_Jung/Merged_to_plot/us-wkg_merged.csv")
file$date <- as.Date(paste("01", file$monthyear, sep="_"), format="%d_%b_%Y")
str(file)
file[10:20,]
#Initial TS plot

p <- ggplot() +
  geom_line(data = file, aes(x = date, y = GPP, color =I("red"))) +
  geom_line(data = file, aes(x = date, y = Jung_2011, color = I("blue"))) +
  geom_line(data = file, aes(x = date, y = Jung_2017, color = I("green"))) +
  xlab('data_date') +
  ylab('GPP')

p

#get mean seasonal cycle
file$month <- month(file$date)
file$year <- year(file$date)

seasonal <- ddply(file, ~month, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))

q <- ggplot() +
  geom_line(data = seasonal, aes(x = month, y = GPP, color =I("red"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2011, color = I("blue"))) +
  geom_line(data = seasonal, aes(x = month, y = Jung_2017, color = I("green"))) +
  xlab('month') +
  ylab('GPP')

q

#Get bar graph (inset)
mean_annual <- ddply(file, ~year, summarize, Jung_2011=mean(Jung_2011, na.rm=TRUE), Jung_2017=mean(Jung_2017, na.rm=TRUE), GPP=mean(GPP, na.rm=TRUE))
str(mean_annual)
mean_annual[-1,]
mean_annual[mean_annual == 0] <- NA
mean <- colMeans(mean_annual, na.rm=TRUE)
mean <- as.data.frame(mean)
colnames(mean) <- c("data","mean")
mean
r<- ggplot()+
  geom_bar(data = mean, aes(x=year, y = mean, fill =I("red"))) +
  xlab('month') +
  ylab('GPP')
r
