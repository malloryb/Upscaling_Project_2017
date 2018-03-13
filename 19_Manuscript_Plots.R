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

#Testing out the corfunc
corfunc(for_fig_1)


#Checking out IAV--------------

IAV_to_plot <-ddply(for_fig_1, .(year, site), summarize, Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))








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

