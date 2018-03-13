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
str(for_fig_1)

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
for_fig_1$month <- as.factor(for_fig_1$month)

#Testing out the corfunc
corfunc(for_fig_1)

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
list_seasonsRF <- split(plot_seasonal_cycle, plot_seasonal_cycle$site)
str(seasonal_to_plot)
plot_seasonal_cycle_1 <- function(x){
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
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("green")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="red")+
    geom_errorbar(data=x,aes(x=month, ymin=RFF3point+ RFF3point_se, ymax=RFF3point+RFF3point_se),colour="blue")+
    geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP+ Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="green")+
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

