#Code to Open Flux Data 
library(raster)
library(dismo)
library(ggplot2)
library(hexbin)
library(rworldmap)
library(ggmap)
library(jpeg)
library(reshape2)
library(ggplot2)

setwd("C:/Users/Mallory/Dropbox/Upscaling_Project/")
sites <- read.csv("Tower_Sites.csv")
str(sites)
sites

sites$Data_Cat <- as.factor(sites$Data_Cat)

#qplot(factor(IGBP), data=sites, geom="bar", fill=factor(LENGTH_CAT))+
        #scale_fill_manual(values=c("blue", "green", "yellow", "red"), name="Record Length", labels=c("0-5 years", "5-10 years", "10-15 years", "15+ years"))+
        #theme_bw(base_size=14)+labs(title = "IGBP Distribution of Flux Sites by Record Length", x="IGBP Vegetation Classification", y="count")

#ggplot(sites, aes(Data_Cat)) + geom_bar() +
        #facet_wrap(~ Data_Cat)
#scale_fill_manual(values=c("blue", "green", "yellow", "red"), name="Record Length", labels=c("0-5 years", "5-10 years", "10-15 years", "15+ years"))+
        #theme_bw(base_size=14)+labs(title = "IGBP Distribution of Flux Sites by Record Length", x="IGBP Vegetation Classification", y="count")


#img <- readJPEG("PrecipitationTempBiomes.jpg")
#image <- apply(img, 1:2, function(v) rgb(v[1], v[2], v[3]))
#image <- melt(image)
#ggplot(image, aes(row, -column, fill=fill)) + geom_tile() + scale_fill_identity()

ggplot(sites, aes(y=Mat, x=Map)) + 
        geom_point(aes(color=Data_Cat), size=4)+
        #scale_y_reverse()+
        xlim(0,1000)+
        scale_color_manual(values=c("green", "yellow", "red"), name="Processing Level", labels=c("1 - in hand", "2 - minimal processing", "3 - significant processing"))+
        theme_bw(base_size=14)+labs(title = "Climate Distribution of Flux Sites by Processing Level", y="Mean Annual Temperature (C)", x="Mean Annual Precipitation (mm)")

#Using GGPLOT, plot the Base World Map
mp <- NULL
mapWorld <- borders("world", regions=c("Mexico", "USA"), colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
?borders

#Now Layer the cities on top - jittered to show more sites but remove jitter if this is going in a paper

mp_plot <- mp+ geom_point(aes(x=sites$Long, y=sites$Lat,colour=sites$Data_Cat), position= position_jitter(w=0.3, h=0.3), size=2.5)+
        scale_color_manual(values=c("green", "yellow", "red"), name="Processing Level", labels=c("1 - in hand", "2 - minimal processing", "3 - significant processing"))+
        coord_map(xlim = c(-123,-103), ylim = c(23,41))
mp_plot+ theme_bw(base_size=14)+ labs(title = "Flux Sites by Data Availability", x="Longitude", y="Latitude") 

