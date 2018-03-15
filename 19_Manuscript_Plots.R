#Manuscript plots for upscaling project
#Fig 1: 
setwd("D:/Upscaling_Project/Upscaled_GPP/RF_C3/")
lst <- list.files("D:/Upscaling_Project/Upscaled_GPP/RF_C3/", pattern=".csv")
#Format output from RFC3 flux file 
format_Barnesoutput <- function(xx){
  print("read file")
  filename <- tools::file_path_sans_ext(basename(xx))
  xx <- read.csv(xx)
  require(stringr)
  require(lubridate)
  #get function filename
  #read in site 
  print(filename)
  xx$year <- substr(filename,10,13)
  #some sort of lookup table is needed 
  xx$monthyear <- paste(xx$month, xx$year, sep="_")
  xx$date <- as.Date(paste("01", xx$monthyear, sep="_"), format="%d_%m_%Y")
  xx$monthyear <- format(xx$date,"%b_%Y")
  return(xx)
}

RF_C3 <- do.call("rbind", lapply(lst, format_Barnesoutput))
RF_C3$X <- NULL
#Name sites properly
names(RF_C3) <- c("us_aud", "us_cop", "us_fuf", "us_mpj", "us_scc", "us_scf", 
                  "us_scw", "us_seg", "us_sen", "us_ses", "us_so2", "us_so3", 
                  "us_so4", "us_srg", "us_srm", "us_vcm", "us_vcp", "us_whs", 
                  "us_wjs", "us_wkg", "month", "year", "monthyear", "date")
str(RF_C3)
#Put in long format for plotting
RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_aud:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("D:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
merged <- merge(RFC3_long, FluxJung, all=T)

#Merge with SPEI Going to use site-based SPEI 12 for now
getspei <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv")
str(getspei)
getspei$site <- (gsub('([[:punct:]])|\\-','_', getspei$site))
getspei$monthyear <- (gsub('([[:punct:]])|\\-','_', getspei$monthyear))
getspei$sitedate <- as.factor(paste(getspei$monthyear, getspei$site, sep="_"))
spei <- getspei[,c("sitedate","spei12","spei1", "spei6", "spei3")]
str(spei)
levels(getspei$sitedate)
levels(merged$sitedate)
#Function to calculate correlation between spei and monthly GPP 
corfunc <- function(gg){
  require(plyr)
  return(data.frame(CORFlux12 = cor(gg$GPP, gg$spei1, use="complete.obs"), pvalFlux12=cor.test(gg$GPP, gg$spei1)$p.value, 
                    CORBarnes12= cor(gg$Barnes_GPP, gg$spei1, use="complete.obs"), pvalBarnes12=cor.test(gg$Barnes_GPP, gg$spei1)$p.value,
                    CORJung12= cor(gg$Jung_GPP, gg$spei1, use="complete.obs"), pvalJung12=cor.test(gg$Jung_GPP, gg$spei1)$p.value))
}

#Merge SPEI with Flux, Barnes, and Jung
for_fig_1 <- merge(merged, spei, all.x=T)
for_fig_1$X <- NULL
for_fig_1$site.x <- NULL
#write.csv(for_fig_1, "D:/Upscaling_Project/For_Fig_1.csv")
#Testing out the corfunc
corfunc(for_fig_1)
#Checking out IAV--------------
for_fig_1<-for_fig_1[!(for_fig_1$year > 2013 | for_fig_1$year < 1999),]
#Subset values where GPP is a value
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
summary(for_fig_1)
for_fig_1[100:150,]

#Get IAV for IAV plots
IAV_to_plot <-ddply(for_fig_1, .(year, site), summarize, Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
#Get figure 1 formatting data ready (corrs with SPEI)
Fig1_to_plot <-ddply(Subs1, .(year, site), summarize, SPEI_12max=max(abs(spei12)), SPEI_12mean=mean(spei12, na.rm=TRUE),
                     SPEI_6max=max(abs(spei6)), SPEI_6mean=mean(spei6, na.rm=TRUE),
                     SPEI_1max=max(abs(spei1)), SPEI_1mean=mean(spei1, na.rm=TRUE),
                     Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))

#Just mean annual SPEI12
corfunc2 <- function(gg){
  require(plyr)
  return(data.frame(FluxGPP = cor(gg$GPP, gg$SPEI_12mean, use="complete.obs"), 
                    #pvalFlux=cor.test(gg$GPP, gg$SPEI_12mean)$p.value,
                    BarnesGPP = cor(gg$Barnes_GPP, gg$SPEI_12mean, use="complete.obs"), 
                    #pvalBarnes=cor.test(gg$Barnes_GPP, gg$SPEI_12max)$p.value,
                    JungGPP = cor(gg$Jung_GPP, gg$SPEI_12mean, use="complete.obs"))) 
                    #pvalJung=cor.test(gg$Jung_GPP, gg$SPEI_12max)$p.value))
  
}
list_sites <- split(Fig1_to_plot, Fig1_to_plot$site)
Plot_12m_IAV <- do.call("rbind", lapply(list_sites, corfunc2))
Plot_12m_IAV <- tibble::rownames_to_column(Plot_12m_IAV)
#Using GGPLOT, plot the Base World Map
library(ggplot2)
library(ggthemes)
mp <- NULL
mapWorld <- borders("world", regions=c("Mexico", "USA"), colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
?borders
str(Plot_12m_IAV)
SPEI12_IAV_long <- tidyr::gather(Plot_12m_IAV, rowname, GPP, factor_key=TRUE)
str(SPEI12_IAV_long)
names(SPEI12_IAV_long) <- c("site", "GPP", "cor")
#Need to match lat and long now 
lookup <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
lookup$sitechar <- substr(lookup$site, 4,6)
point <- data.frame("site" = paste("us_", lookup$sitechar, sep=""), 
                    "lat" = lookup$lat, 
                    "long" = lookup$long)
merged <- merge(SPEI12_IAV_long, point, by="site")

split <- split(merged, merged$GPP)
FluxGPP <- split$FluxGPP
JungGPP <- split$JungGPP
BarnesGPP <- split$BarnesGPP

JungGPP$JungGPP_dif <- abs(FluxGPP$cor - JungGPP$cor)
BarnesGPP$BarnesGPP_dif <- abs(FluxGPP$co - BarnesGPP$cor)
range(FluxGPP$cor)
range(BarnesGPP$cor)
range(JungGPP$cor)
mp_plot1 <- mp+ geom_point(aes(x=FluxGPP$long, y=FluxGPP$lat, colour=FluxGPP$cor), position= position_jitter(w=0.5, h=0.5), size=2)+
  scale_color_gradientn(colours=c("blue", "white","red"), name="Correlation", limits=c(-.75,1))+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))+ 
  theme_few(base_size=8)+ labs(title = "Flux GPP vs SPEI12", x="Longitude", y="Latitude") 


mp_plot2 <- mp+ geom_point(aes(x=BarnesGPP$long, y=BarnesGPP$lat, colour=BarnesGPP$cor), position= position_jitter(w=0.5, h=0.5), size=2)+
  scale_color_gradientn(colours=c("blue", "white","red"), name="Correlation", limits=c(-0.75,1))+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))+
  theme_few(base_size=8)+ labs(title = "BarnesGPP vs SPEI12", x="Longitude", y="Latitude") 


mp_plot3 <- mp+ geom_point(aes(x=JungGPP$long, y=JungGPP$lat, colour=JungGPP$cor), position= position_jitter(w=0.5, h=0.5), size=2)+
  scale_color_gradientn(colours=c("blue", "white","red"), name="Correlation", limits=c(-0.75,1))+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))+ theme_few(base_size=8)+ labs(title = "Jung GPP vs SPEI12", x="Longitude", y="Latitude") 

ggpubr::ggarrange(mp_plot1, mp_plot2, mp_plot3, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")


mp_plot5 <- mp+ geom_point(aes(x=BarnesGPP$long, y=BarnesGPP$lat, colour=BarnesGPP$BarnesGPP_dif), position= position_jitter(w=0.3, h=0.3), size=2.5)+
  scale_color_gradientn(colours=c("red", "blue"), name="Correlation Difference", limits=c(-0,.75))+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))

mp_plot5+ theme_few(base_size=14)+ labs(title = "Relationship strength with SPEI12: Barnes vs. Ground Flux", x="Longitude", y="Latitude") 


  mp_plot4 <- mp+ geom_point(aes(x=JungGPP$long, y=JungGPP$lat, colour=JungGPP$JungGPP_dif), position= position_jitter(w=0.3, h=0.3), size=2.5)+
  scale_color_gradientn(colours=c("red", "blue"), name="Correlation Difference", limits=c(0,.75))+
  coord_map(xlim = c(-123,-103), ylim = c(23,41))

mp_plot4+ theme_few(base_size=14)+ labs(title = "Relationship strength with SPEI12: Jung vs. Ground Flux", x="Longitude", y="Latitude") 


#Plotting Figure 1----------------------


  
  
#Split, apply combine to find which corr is best !
list_sites <- split(Fig1_to_plot, Fig1_to_plot$site)

corfunc1 <- function(gg){
  require(plyr)
  return(data.frame(corG12 = cor(gg$GPP, gg$SPEI_12max, use="complete.obs"), pvalG12=cor.test(gg$GPP, gg$SPEI_12max)$p.value,
    corB12 = cor(gg$SPEI_12max, gg$Barnes_GPP, use="complete.obs"), pvalB12=cor.test(gg$SPEI_12max, gg$Barnes_GPP)$p.value,
    corJ12 = cor(gg$SPEI_12max, gg$Jung_GPP, use="complete.obs"), pvalJ12=cor.test(gg$SPEI_12max, gg$Jung_GPP)$p.value, 
    corG12m = cor(gg$GPP, gg$SPEI_12mean, use="complete.obs"), pvalG12m=cor.test(gg$GPP, gg$SPEI_12mean)$p.value,
    corB12m = cor(gg$Barnes_GPP, gg$SPEI_12mean, use="complete.obs"), pvalB12m=cor.test(gg$Barnes_GPP, gg$SPEI_12max)$p.value,
    corJ12m = cor(gg$Jung_GPP, gg$SPEI_12mean, use="complete.obs"), pvalJ12m=cor.test(gg$Jung_GPP, gg$SPEI_12max)$p.value,
    corG6 = cor(gg$GPP, gg$SPEI_12max, use="complete.obs"), pvalG6=cor.test(gg$GPP, gg$SPEI_12max)$p.value,
    corB6 = cor(gg$SPEI_6max, gg$Barnes_GPP, use="complete.obs"), pvalB6=cor.test(gg$SPEI_6max, gg$Barnes_GPP)$p.value,
    corJ6 = cor(gg$SPEI_6max, gg$Jung_GPP, use="complete.obs"), pvalJ6=cor.test(gg$SPEI_6max, gg$Jung_GPP)$p.value, 
    corG6m = cor(gg$GPP, gg$SPEI_6mean, use="complete.obs"), pvaB6m=cor.test(gg$GPP, gg$SPEI_6mean)$p.value,
    corB6m = cor(gg$Barnes_GPP, gg$SPEI_6mean, use="complete.obs"), pvalB6m=cor.test(gg$Barnes_GPP, gg$SPEI_6mean)$p.value,
    corJ6m = cor(gg$Jung_GPP, gg$SPEI_6mean, use="complete.obs"), pvalJ6m=cor.test(gg$Jung_GPP, gg$SPEI_6mean)$p.value))
  
}
checkcorrs <- do.call("rbind", lapply(list_sites, corfunc1))
round(checkcorrs,2)
#write.csv(checkcorrs, "D:/Upscaling_Project/Checkcorrs.csv")

#MY IAV IS BETTER WOOHOO!--------------------------
IAV_to_plot
IAVplot_func1(IAV_to_plot)

IAVplot_func1 <- function(xx){
  require(ggplot2)
  require(stringr)
  require(plyr)
  require(psych)
  require(ggpubr)
  require(ggthemes)
  
  Sites <- read.csv("C:/Users/Mallory/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
  Sites$site <- str_replace_all(Sites$site, "-", "_")
  
  levels(xx$site)
  levels(Sites$site)
  
  xxmerge <- merge(xx, Sites, by="site")
  
  corfunc <- function(gg){
    require(plyr)
    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    gg[gg == 0] <- NA
    
    
    return(data.frame(corJ = cor(gg$GPP, gg$Jung_GPP, use="complete.obs"), pvalJ=cor.test(gg$GPP, gg$Jung_GPP)$p.value,
                      corB = cor(gg$GPP, gg$Barnes_GPP, use="complete.obs"), pvalB=cor.test(gg$GPP, gg$Barnes_GPP)$p.value,
                      MAP=getmode(gg$MAP), RMSSD=rmssd(gg$GPP)))
  }
  
  xxplot <- ddply(xxmerge, .(site), corfunc)
  summary(xxplot)
  xxplot$sigB <- ifelse(xxplot$pvalB <0.05, "Significant", "nonsignificant")
  xxplot$sigJ <- ifelse(xxplot$pvalJ <0.05, "Significant", "nonsignificant")
  
  print(summary(xxplot))
  
  
  bplot <- ggplot(xxplot, aes(x=MAP, y=corB, color=sigB)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few() 
  jplot <- ggplot(xxplot, aes(x=MAP, y=corJ, color=sigJ)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few() 
  plot(bplot)
  plot(jplot)
}


#Creating diagnostic plots-----------------------------------------------------------------------------
#Subsetting only cases with Flux GPP
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
#Create data frame for seasonal cycle plotting
plot_seasonal_cycle <- ddply(Subs1, .(month, site), summarize, Barnes_GPP_se=sd(Barnes_GPP, na.rm=TRUE)/sqrt(length(Barnes_GPP[!is.na(Barnes_GPP)])), Barnes_GPP=mean(Barnes_GPP, na.rm=TRUE),
Barnes_GPP=mean(Barnes_GPP, na.rm=TRUE),  Jung_GPP_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])), Jung_GPP=mean(Jung_GPP, na.rm=TRUE), 
GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])), GPP=mean(GPP, na.rm=TRUE))

#Plotting function that plots all sites serparately and writes out graphs
#Where to write out .png files

setwd("F:/Upscaling_Project/Upscaled_GPP/")
list_seasons <- split(plot_seasonal_cycle, plot_seasonal_cycle$site)
str(list_seasons)
plot_seasonal_cycle_1 <- function(x){
  require("ggplot2")
  require("ggthemes")
  require("scales")
  require("psych")
  require("ggpubr")
  print(str(x))
  droplevels(x)
  B<- as.character(round(cor(x$GPP, x$Barnes_GPP, use="complete.obs"), 3))
  J<- as.character(round(cor(x$GPP, x$Jung_GPP, use="complete.obs"), 3))
  
  print("calculated cors")
  rmssdGPP <- as.character(round(rmssd(x$GPP), 3))
  rmssdB <- as.character(round(rmssd(x$Barnes_GPP), 3))
  rmssdJ <- as.character(round(rmssd(x$Jung_GPP), 3))
  
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 3)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 3)
  
  print("got RMSSD")
  lblGPP <- paste("rmssdFlux =", rmssdGPP)
  lblB <- paste("rmseBarnes =", rmseBarnes, "r"=B, "rmssd"=rmssdB)
  lblJ <- paste("rmseJung =", rmseJung, "r"=J, "rmssd"=rmssdJ)
  
  filename <- paste(x$site[1], "seasonal_comparison.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(paste(x$site, "RFC3", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("red")), size=2) +
    geom_line(data = x, aes(x = month, y = Barnes_GPP, color = I("blue")), size=2) +
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=Barnes_GPP+ Barnes_GPP_se, ymax=Barnes_GPP+Barnes_GPP_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP+ Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="green")+
    annotate("text", label = lblGPP, parse=FALSE, x =3, y = 4.5, size = 5, colour = "Red")+
    annotate("text", label = lblB, parse=FALSE, x = 3, y = 5, size = 5, colour = "Blue")+
    annotate("text", label = lblJ, parse=FALSE, x = 3, y = 5.5, size = 5, colour = "Green")+
      scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  plot(q)
  filename <- paste(x$site[1], "seasonalRF_comparison.png", sep="_")
  #ggsave(filename, device='png', width=16, height=16, dpi = 300, units = "cm")
}
#split
#apply (and write out)
lapply(list_seasons, plot_seasonal_cycle_1)

