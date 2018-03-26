#Manuscript plots for upscaling project
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
RFC3_long <- tidyr::gather(RF_C3, site, Barnes_GPP, us_aud:us_wkg, factor_key=TRUE)
str(RFC3_long)
RFC3_long$sitedate <- as.factor(paste(RFC3_long$monthyear, RFC3_long$site, sep="_"))
#Merge with true flux data and Jung 2017 data
FluxJung <- read.csv("F:/Upscaling_Project/Jung_Comps/Merged_Jung_Comps.csv")
levels(RFC3_long$sitedate)
levels(FluxJung$sitedate)
merged <- merge(RFC3_long, FluxJung, all=T)

#Merge with SPEI Going to use site-based SPEI 12 for now
getspei <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Upscaling_All_Sites_3_3_2018.csv")
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
lookup <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
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
JungGPP[order(JungGPP$cor),]
BarnesGPP[order(BarnesGPP$cor),]
FluxGPP[order(FluxGPP$cor),]

range(JungGPP$cor)
JungGPP
#Plotting Figure 1----------------------
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


#Just the global GPP part now
library(lattice)
library(rasterVis)
library(grid)
lst2010 <- list.files("F:/Upscaling_Project/Test_Global_Upscaling/", pattern="2010_.tif", full.names=TRUE)
stack2010 <- stack(lst2010)
mean2010 <- calc(stack2010, mean, na.rm=TRUE) 
plot <- plot(mean2010)

# coerce to a SpatialPolygons object
ext <- extent(c(-123,-103, 23,41))
p <- as(ext, 'SpatialPolygons') 


proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
worldSHP <- shapefile("F:/Upscaling_Project/Test_Global_Upscaling/TM_WORLD_BORDERS-0.3.shp")
mapTheme <- rasterTheme(region = brewer.pal(8, "RdYlGn"))
plot <- levelplot(mean2010, margin=F, par.settings=mapTheme, at=seq(0, 6, length.out=40), main="Mean Daily GPP")
plot + layer(sp.lines(worldSHP, lwd=0.8, col='gray'))+
  layer(sp.polygons(p, pch=4, alpha=1))
trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid.text(("daily GPP \n (gCm-2day-1) uptake"), 0.2, 0, hjust=0.5, vjust=1)
trellis.unfocus()



  
  
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
#write.csv(checkcorrs, "F:/Upscaling_Project/Checkcorrs.csv")
#For Figure 2--------------------
str(merged)
merged <- merge(RFC3_long, FluxJung, all=T)
Jung <- dplyr::select(merged, "sitedate", "GPP", "Jung=_GPP")
Barnes <- dplyr::select(merged, "sitedate", "GPP", "Barnes_GPP")
Jung$model <- "Jung"
Jung <- Jung[complete.cases(Jung), ] 
Barnes <- Barnes[complete.cases(Barnes), ]
str(Jung)
colnames(Jung)[3] <- "model_GPP"
colnames(Barnes)[3] <- "model_GPP"
fig_2b_plot <- rbind(Barnes, Jung)
fig_2b_plot$model <- as.factor(fig_2b_plot$model)

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

ggplot(fig_2b_plot, aes(x=GPP, y=model_GPP, group=model, colour=model))+
  geom_smooth(method="lm",se=FALSE) +geom_point(alpha=0.5, size=1)+ theme_few() +geom_abline(intercept=0,slope=1)+
  xlim(0,9)+ylim(0,9)+
  annotate("text", x = 2, y = 8.5, colour = "#F8766D", size = 4,
           label = lm_eqn(lm(model_GPP ~ GPP, Barnes)), parse = TRUE)+
  annotate("text", x = 2, y = 9, colour = "#00BFC4", size = 4,
           label = lm_eqn(lm(model_GPP ~ GPP, Jung)), parse = TRUE)

#Want: pair flux GPP & jung GPP, barnes GPP & jung GPP, rbind, then make a geom_point that's colored by "model"

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
  
  Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
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
  
  
  bplot <- ggplot(xxplot, aes(x=MAP, y=corB, color=sigB)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few(base_size = 10)+
    ggtitle("Correlation BarnesIAV, FluxIAV")
  jplot <- ggplot(xxplot, aes(x=MAP, y=corJ, color=sigJ)) + geom_point(size=2) + ylim(-1,1) +geom_line(aes(y=0),linetype="dotted")+ theme_few(base_size = 10) +
    ggtitle("Correlation JungIAV, FluxIAV")
  plot(bplot)
  plot(jplot)
  ggarrange(bplot, jplot, common.legend = TRUE, ncol = 2)
}


#Creating diagnostic plots-----------------------------------------------------------------------------
#Subsetting only cases with Flux GPP
Subs1<-subset(for_fig_1, (!is.na(for_fig_1[,9])))
str(Subs1)
#Create data frame for seasonal cycle plotting
plot_seasonal_cycle <- ddply(Subs1, .(month, site), summarize, Barnes_GPP_se=sd(Barnes_GPP, na.rm=TRUE)/sqrt(length(Barnes_GPP[!is.na(Barnes_GPP)])), Barnes_GPP=mean(Barnes_GPP, na.rm=TRUE),  Jung_GPP_se=sd(Jung_GPP, na.rm=TRUE)/sqrt(length(Jung_GPP[!is.na(Jung_GPP)])), Jung_GPP=mean(Jung_GPP, na.rm=TRUE), 
GPP_se=sd(GPP, na.rm=TRUE)/sqrt(length(GPP[!is.na(GPP)])), GPP=mean(GPP, na.rm=TRUE))

#Plotting function that plots all sites serparately and writes out graphs
#Where to write out .png files

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
  lblGPP <- paste("RMSSDFlux =", rmssdGPP)
  lblB <- paste("rmseBarnes =", rmseBarnes, ",r=", B, ",RMSSD=",rmssdB)
  lblJ <- paste("rmseJung =", rmseJung, ",r=", J, ",RMSSD=",rmssdJ)
  
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
    annotate("text", label = lblGPP, parse=FALSE, x =6, y = 6, size = 7, colour = "Red")+
    annotate("text", label = lblB, parse=FALSE, x = 6, y = 7, size = 7, colour = "Blue")+
    annotate("text", label = lblJ, parse=FALSE, x = 6, y = 8, size = 7, colour = "Green")+
      scale_x_continuous(breaks=pretty_breaks())+
    xlab('month')+
    ylab('GPP')+
    theme_classic()+
    theme(legend.position = c(0, 0))
  plot(q)
  filename <- paste(x$site[1], "seasonalRF_comparison.png", sep="_")
  ggsave(filename, device='png', width=16, height=16, dpi = 300, units = "cm")
}
#split
#apply (and write out)
#Figure 2c-----------------------
lapply(list_seasons, plot_seasonal_cycle_1)
list_seasons[[1]]
getRMSE <- function(x){
  str(x)
  rmseBarnes =round(sqrt( mean((x$Barnes_GPP-x$GPP)^2 , na.rm = TRUE )), 2)
  rmseJung =round(sqrt( mean((x$Jung_GPP-x$GPP)^2 , na.rm = TRUE )), 2) 
  df <- data.frame(rmseBarnes, rmseJung, x$site, x$month)
  colnames(df) <- c("Barnes","Jung", "site", "month")
  return(df)

}

str(merged)
Sites <- read.csv("C:/Users/rsstudent/Dropbox (Dissertation Dropbox)/Site_Lookup_2018.csv")
Sites$site <- str_replace_all(Sites$site, "-", "_")
merged$diff1 <- merged$GPP - merged$Jung_GPP
merged$diff2 <- merged$GPP - merged$Barnes_GPP

Sites <- Sites[,c("site", "Vegtype")]
Veg_merge <- plyr::rename(Sites, c("site"="site", "Vegtype"="Veg"))
Residual_plot <- merge(merged, Veg_merge, by="site")

#Split apply combine plots
Y <- split(Residual_plot, Residual_plot$Veg)
Forest <- Y[[1]]
Grassland <- Y[[2]]
Savanna <- Y[[3]]
Shrubland <- Y[[4]]
str(residplot)

stdev <- ddply(Residual_plot, .(Veg), summarize, std_dev1=sd(diff1,na.rm=TRUE), std_dev2=sd(diff2,na.rm=TRUE))
str(stdev)
formax1 <- (stdev[1,2]*1.96)
formin1 <- -(stdev[1,2]*1.96)
grassmax1 <- (stdev[2,2]*1.96)
grassmin1 <- -(stdev[2,2]*1.96)
shrubmax1 <- (stdev[4,2]*1.96)
shrubmin1 <- -(stdev[4,2]*1.96)
savmax1 <- (stdev[3,2]*1.96)
savmin1 <- -(stdev[3,2]*1.96)


formax2 <- (stdev[1,3]*1.96)
formin2 <- -(stdev[1,3]*1.96)
grassmax2 <- (stdev[2,3]*1.96)
grassmin2 <- -(stdev[2,3]*1.96)
shrubmax2 <- (stdev[4,3]*1.96)
shrubmin2 <- -(stdev[4,3]*1.96)
savmax2 <- (stdev[3,3]*1.96)
savmin2 <- -(stdev[3,3]*1.96)

library(ggplot2)
library(scales)
library(ggthemes)
library(ggpubr)

str(Residual_plot)
B2 <- ggplot(Residual_plot, aes(x=month, y=diff2, color=Veg)) + geom_point(size=2, alpha=0.5) +geom_smooth(aes(group=Veg), method="loess", se=FALSE)+theme_few() + ylim(-5,5) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ ylab("FluxGPP - BarnesGPP")+ scale_x_continuous(breaks=pretty_breaks())
Fo2 <- ggplot(Forest, aes(x=month, y=diff2, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-5,5) +geom_line(aes(y=0), linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - BarnesGPP")+geom_line(aes(y=(formax1)), linetype="dotted", color="black")+geom_line(aes(y=(formin1)), linetype="dotted", color="black")
Gra2 <- ggplot(Grassland, aes(x=month, y=diff2, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-3,5) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - BarnesGPP")+ geom_line(aes(y=(grassmax1)), linetype="dotted", color="black")+geom_line(aes(y=(grassmin1)), linetype="dotted", color="black")
Sav2 <- ggplot(Savanna, aes(x=month, y=diff2, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-4,4) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - BarnesGPP")+ geom_line(aes(y=(savmax1)), linetype="dotted", color="black")+geom_line(aes(y=(savmin1)), linetype="dotted", color="black")
Shr2 <- ggplot(Shrubland, aes(x=month, y=diff2, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-4,4) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - BarnesGPP")+ geom_line(aes(y=(shrubmax1)), linetype="dotted", color="black")+geom_line(aes(y=(shrubmin1)), linetype="dotted", color="black")
#apply (and write out)

ggpubr::ggarrange(Fo2, Gra2, Sav2, Shr2 + rremove("x.text"), 
          labels = c("Forest", "Grass", "Savanna", "Shrub"), vjust=1.5, hjust=-0.4, 
          legend="none",
          ncol = 2, nrow = 2)

J2 <- ggplot(Residual_plot, aes(x=month, y=diff1, color=Veg)) + geom_point(size=2, alpha=0.5) +geom_smooth(aes(group=Veg), method="loess", se=FALSE)+theme_few() + ylim(-5,5) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ ylab("FluxGPP - JungGPP")+ scale_x_continuous(breaks=pretty_breaks())

ggarrange(B2, J2, common.legend = TRUE, ncol=2, nrow=1)
Fo <- ggplot(Forest, aes(x=month, y=diff1, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-3,5) +geom_line(aes(y=0), linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+geom_line(aes(y=(formax1)), linetype="dotted", color="black")+geom_line(aes(y=(formin1)), linetype="dotted", color="black")
Gra <- ggplot(Grassland, aes(x=month, y=diff1, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-3,3) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(grassmax1)), linetype="dotted", color="black")+geom_line(aes(y=(grassmin1)), linetype="dotted", color="black")
Sav <- ggplot(Savanna, aes(x=month, y=diff1, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-4,4) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(savmax1)), linetype="dotted", color="black")+geom_line(aes(y=(savmin1)), linetype="dotted", color="black")
Shr <- ggplot(Shrubland, aes(x=month, y=diff1, color=site)) + geom_point(size=2) +geom_smooth(aes(group=site), method="loess", se=FALSE)+theme_few() + ylim(-3,3) +geom_line(aes(y=0),linetype="twodash", size=1, color="black")+ scale_x_continuous(breaks=pretty_breaks())+ ylab("FluxGPP - JungGPP")+ geom_line(aes(y=(shrubmax1)), linetype="dotted", color="black")+geom_line(aes(y=(shrubmin1)), linetype="dotted", color="black")
#apply (and write out)

ggpubr::ggarrange(Fo, Gra, Sav, Shr + rremove("x.text"), 
                  labels = c("Forest", "Grass", "Savanna", "Shrub"), vjust=1.5, hjust=-0.4, 
                  legend="none",
                  ncol = 2, nrow = 2)


#Correion plot of differences against GPP
B <- ggplot(Residual_plot, (aes(x=GPP, y=diff1, color=Veg)))+geom_point(size=2)+ ylim(-5,5) + geom_smooth(method="lm")+ theme_few()+ylab("FluxGPP - JungGPP")+xlab("FluxGPP")+annotate("text", label="r = 0.84", y=5,x=1)
J <- ggplot(Residual_plot, (aes(x=GPP, y=diff2, color=Veg)))+geom_point(size=2)+ ylim(-5,5) +geom_smooth(method="lm")+ theme_few()+ylab("FluxGPP - BarnesGPP")+xlab("FluxGPP")+ annotate("text", label="r=0.09", y=5,x=1)
ggpubr::ggarrange(B,J, common.legend = TRUE, ncol=2, nrow=1)

cor(Residual_plot$GPP, Residual_plot$diff1, use="complete.obs")
cor(Residual_plot$GPP, Residual_plot$diff2, use="complete.obs")
