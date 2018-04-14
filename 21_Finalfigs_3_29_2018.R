#Final Plots: 
#Figure 1 is: 
#Bar graph showing SPEI (geom_point) and geom_bar as cors between SPEI 12 and barnes
#The three interannual variability graphs 

#Fig 1: 
setwd("F:/Upscaling_Project/Upscaled_GPP/RF_C3/")
lst <- list.files("F:/Upscaling_Project/Upscaled_GPP/RF_C3/", pattern=".csv")
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
RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_auF:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("D:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
levels(FluxJung$site.x)
levels(RFC3_long$site)
merged <- merge(RFC3_long, FluxJung, all.x=T)

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
#write.csv(for_fig_1, "F:/Upscaling_Project/For_Fig_1.csv")
#Testing out the corfunc
corfunc(for_fig_1)
#Checking out IAV--------------
for_fig_1<-for_fig_1[!(for_fig_1$year > 2013 | for_fig_1$year < 1999),]
#Subset values where GPP is a value
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
summary(for_fig_1)
for_fig_1[100:150,]
library(plyr)
#Get IAV for IAV plots
IAV_to_plot <-ddply(for_fig_1, .(year, site), summarize, Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))
#Get figure 1 formatting data ready (corrs with SPEI)
Fig1_to_plot <-ddply(Subs1, .(year, site), summarize, SPEI_12max=max(abs(spei12)), SPEI_12mean=mean(spei12, na.rm=TRUE),
                     SPEI_6max=max(abs(spei6)), SPEI_6mean=mean(spei6, na.rm=TRUE),
                     SPEI_1max=max(abs(spei1)), SPEI_1mean=mean(spei1, na.rm=TRUE),
                     Barnes_GPP=sum(Barnes_GPP, na.rm=TRUE), Jung_GPP=sum(Jung_GPP, na.rm=TRUE), GPP=sum(GPP, na.rm=TRUE))

#Plot sensitivity 
library(ggplot2)
library(ggpubr)
library(ggthemes)
#Colors: 
#Barnes: #BD2031
#Flux: #5F20BD
#Jung #7FBD20
Fig1_to_plot
pp1 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#BD2031") +  ylim(0,55)+
  xlab("12 month SPEI")+ ylab("Tower GPP")+
  ggtitle("Observed GPP")+ theme_few(base_size=12)

pp2 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = Barnes_GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#5F20BD")+ ylim(0,55)+
  xlab("12 month SPEI")+ ylab("DryFlux GPP")+
  ggtitle("DryFlux GPP")+ theme_few(base_size=12)

pp3 <- ggplot(Fig1_to_plot, aes(x = SPEI_12mean, y = Jung_GPP,group = site)) +
  stat_smooth(method="lm", se=FALSE, color="#7FBD20")+ylim(0,55)+
  xlab("12 month SPEI")+ ylab("Fluxcom GPP")+
  ggtitle("Fluxcom GPP")+ theme_few(base_size=12)
ggarrange(pp1, pp3, pp2)

#Figure 1b---------------------------------------------------------

For1b <- read.csv("F:/Upscaling_Project/Upscaled_GPP/for_fig_1_3_22_2018.csv")
modeled_GPP <- subset(For1b, GPP=="BarnesGPP" | GPP=="JungGPP")
tower_GPP <- subset(For1b, GPP=="FluxGPP")
#Getting correlation for ...
tower_GPP <- subset(For1b, GPP=="FluxGPP")
Fluxcom_GPP <- subset(For1b, GPP=="JungGPP")
DryFlux_GPP<- subset(For1b, GPP=="BarnesGPP")

mean(tower_GPP$cor)
mean(Fluxcom_GPP$cor)
mean(DryFlux_GPP$cor)


ggplot(modeled_GPP, aes(x = reorder(site,order_site), y = cor, fill= GPP)) +
  geom_bar(stat="identity", position="dodge")+
  geom_point(data=tower_GPP, aes(site, cor), shape=95, size=20, color="#BD2031")+
  scale_fill_manual("legend", values =c("#5F20BD", "#BD2031", "#7FBD20"))+
  xlab("Flux Sites")+ ylab("Correlation between GPP and 12-month SPEI")+
  theme_few(base_size=13)+
  theme(axis.text.x = element_text(angle = -90))+
  theme(legend.position="none")

#Figure 2-----------------------------
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
#Create data frame for seasonal cycle plotting
plot_seasonal_cycle <- ddply(Subs1, .(month, site), summarize, Barnes_GPP_se=sd(Barnes_GPP, na.rm=TRUE)/sqrt(length(Barnes_GPP[!is.na(Barnes_GPP)])), Barnes_GPP=mean(Barnes_GPP, na.rm=TRUE),  Jung_GPP_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])), Jung_GPP=mean(Jung_GPP, na.rm=TRUE), 
                             GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])), GPP=mean(GPP, na.rm=TRUE))
str(Subs1)
setwd("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Final Dissertation/")

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
  B<- as.character(round(cor(x$GPP, x$Barnes_GPP, use="complete.obs"), 2))
  J<- as.character(round(cor(x$GPP, x$Jung_GPP, use="complete.obs"), 2))
  
  print("calculated cors")
  rmssdGPP <- as.character(round(rmssd(x$GPP), 2))
  rmssdB <- as.character(round(rmssd(x$Barnes_GPP), 2))
  rmssdJ <- as.character(round(rmssd(x$Jung_GPP), 2))
  
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  
  print("got RMSSD")
  lblGPP <- paste("RMSSDObserved =", rmssdGPP)
  lblB <- paste("rmseDryFlux =", rmseBarnes, ",r=", B, ",RMSSD=",rmssdB)
  lblJ <- paste("rmseFluxcom =", rmseJung, ",r=", J, ",RMSSD=",rmssdJ)
  
  filename <- paste(x$site[1], "seasonal_comparison_3_29.png", sep="_")
  print(filename)
  q <- ggplot() +
    ggtitle(paste(x$site, "RFC3", sep=" "))+
    geom_line(data = x, aes(x = month, y = GPP, color =I("#BD2031")), size=2) +
    geom_line(data = x, aes(x = month, y = Barnes_GPP, color = I("#5F20BD")), size=2) +
    geom_line(data = x, aes(x = month, y = Jung_GPP, color = I("#7FBD20")), size=2) +
    geom_errorbar(data=x,aes(x=month, ymin=Barnes_GPP- Barnes_GPP_se, ymax=Barnes_GPP+Barnes_GPP_se),colour="#5F20BD")+
    geom_errorbar(data=x,aes(x=month, ymin=GPP-GPP_se,ymax=GPP+GPP_se),colour="#BD2031")+
    geom_errorbar(data=x,aes(x=month, ymin=Jung_GPP- Jung_GPP_se, ymax=Jung_GPP+Jung_GPP_se),colour="#7FBD20")+
    annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "#BD2031")+
    annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "#5F20BD")+
    annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "#7FBD20")+
    scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_few(base_size =12)+
    theme(legend.position = c(0, 0))
  plot(q)
  filename <- paste(x$site[1], "seasonalRF_comparison_3_29.png", sep="_")
  ggsave(filename, device='png', width=16, height=16, dpi = 300, units = "cm")
}

fuf <- list_seasons[[3]]
plot_seasonal_cycle_1(list_seasons[[3]])
lapply(list_seasons, plot_seasonal_cycle_1)
getRMSE <- function(x){
  str(x)
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2) 
  df <- data.frame(rmseBarnes, rmseJung, x$site, x$month)
  colnames(df) <- c("Barnes","Jung", "site", "month")
  return(df)
  
}

RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_auF:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
#Figure 2B
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
merged2 <- merge(RFC3_long, FluxJung, all=T)
Jung <- dplyr::select(merged2, "sitedate", "GPP", "Jung_GPP")
Barnes <- dplyr::select(merged2, "sitedate", "GPP", "Barnes_GPP")
Barnes$model <- "Barnes"
Jung$model <- "Jung"
Jung <- Jung[complete.cases(Jung), ] 
Barnes <- Barnes[complete.cases(Barnes), ]
str(Jung)
colnames(Jung)[3] <- "model_GPP"
colnames(Barnes)[3] <- "model_GPP"
str(Barnes)
str(Jung)
fig_2b_plot <- rbind(Barnes, Jung)
fig_2b_plot$model <- as.factor(fig_2b_plot$model)
levels(fig_2b_plot$model) <- gsub("Barnes", "DryFlux", levels(fig_2b_plot$model))
levels(fig_2b_plot$model) <- gsub("Jung", "Fluxcom", levels(fig_2b_plot$model))

lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 2));
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  as.character(as.expression(eq));                 
}


group.colors <- c(Fluxcom = "#7FBD20", DryFlux = "#5F20BD")
fig_2b_plot
ggplot(fig_2b_plot, aes(x=GPP, y=model_GPP, group=model, colour=model))+
  xlim(0,9)+ylim(0,9.5)+
  annotate("text", x = 3.0, y = 9.2, colour = "#5F20BD", size = 5,
           label = lm_eqn(lm(model_GPP ~ GPP, Barnes)), parse = TRUE)+
  annotate("text", x = 3, y = 9.5, colour = "#7FBD20", size = 5,
           label = lm_eqn(lm(model_GPP ~ GPP, Jung)), parse = TRUE)+
  geom_smooth(method="lm",se=FALSE) +geom_point(alpha=0.5, size=1)+geom_abline(intercept=0,slope=1)+
  scale_color_manual(values=group.colors)+
  xlab("observed GPP")+
  ylab("modeled GPP")+
  theme_few(base_size=14)


head(fig_2b_plot)
str(fig_2b_plot)
version
