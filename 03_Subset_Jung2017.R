library(reshape2)

#Jung_2017
Jung_2017 <- read.csv("C:/Users/rsstudent/odrive/UA_Google_Drive/MARS_GPP_all_years_3_22_20117.csv")
#Jung_2011
Jung_2011 <- read.csv("C:/Users/rsstudent/Upscaling_Data/Jung_2011/MR_GPP_all.csv")
names(Jung_2011) <- gsub("\\.", "_", names(Jung_2011))
str(Jung_2011)
str(Jung_2017)

us_aud <- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2017, Jung_2011$lon==-110.75 & Jung_2011$lat==31.75))
us_cop <- bind_rows(subset(Jung_2017, Jung_2017$lon==-109.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-109.75 & Jung_2011$lat==31.75))
us_fuf <- bind_rows(subset(Jung_2017, Jung_2017$lon==-111.75 & Jung_2017$lat==35.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-111.75 & Jung_2011$lat==35.25))
us_lpa <- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.25))
us_mpj <- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.25 & Jung_2017$lat==34.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.25 & Jung_2011$lat==34.25))
us_ray <- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==29.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==29.75))
us_scc <- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.25 & Jung_2017$lat==33.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.25 & Jung_2011$lat==33.75))
us_scf <- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.25))
us_scw<- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.75 & Jung_2017$lat==33.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.75 & Jung_2011$lat==33.75))
us_seg<- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.75 & Jung_2017$lat==34.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.75 & Jung_2011$lat==34.25))
us_sen<- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.75 & Jung_2017$lat==34.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.75 & Jung_2011$lat==34.25))
us_ses<- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.75 & Jung_2017$lat==34.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.75 & Jung_2011$lat==34.25))
us_sob<- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.75 & Jung_2017$lat==33.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.75 & Jung_2011$lat==33.25))
us_son<- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.75 & Jung_2017$lat==33.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.75 & Jung_2011$lat==33.25))
us_soo<- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.75 & Jung_2017$lat==33.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.75 & Jung_2011$lat==33.25))
us_soy<- bind_rows(subset(Jung_2017, Jung_2017$lon==-116.75 & Jung_2017$lat==33.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-116.75 & Jung_2011$lat==33.25))
us_src<- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.75))
us_srg<- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.75))
us_srm<- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.75))
us_tes<- bind_rows(subset(Jung_2017, Jung_2017$lon==-109.25 & Jung_2017$lat==27.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-109.25 & Jung_2011$lat==27.75))
us_vcp<- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.75 & Jung_2017$lat==35.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.75 & Jung_2011$lat==35.75))
us_vcm<- bind_rows(subset(Jung_2017, Jung_2017$lon==-106.75 & Jung_2017$lat==35.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-106.75 & Jung_2011$lat==35.75))
us_whs<- bind_rows(subset(Jung_2017, Jung_2017$lon==-110.75 & Jung_2017$lat==31.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-110.75 & Jung_2011$lat==31.25))
us_wjs<- bind_rows(subset(Jung_2017, Jung_2017$lon==-105.75 & Jung_2017$lat==34.25), 1000*86400*subset(Jung_2011, Jung_2011$lon==-105.75 & Jung_2011$lat==34.25))
us_wkg<- bind_rows(subset(Jung_2017, Jung_2017$lon==-109.75 & Jung_2017$lat==31.75), 1000*86400*subset(Jung_2011, Jung_2011$lon==-109.75 & Jung_2011$lat==31.75))

setwd("C:/Users/rsstudent/odrive/UA_Google_Drive/Upscaling_Data/")

d.list <- list(us_aud, us_cop, us_fuf, us_lpa, us_mpj, us_ray, us_scc, us_scf, us_scw, us_seg, us_sen, us_ses, us_sob, us_son,
               us_soy, us_src, us_srg, us_srm, us_tes, us_vcp, us_vcm, us_whs, us_wjs, us_wkg)


name_my_rows <- function(df){
    row.names(df) <- c("Jung_2011", "Jung_2017")
    x <-t(df)
    return(x)
}

d.list <- lapply(d.list, name_my_rows)

write.csv(d.list[1], "us_aud.csv")
write.csv(d.list[2], "us_cop.csv")
write.csv(d.list[3], "us_fuf.csv")
write.csv(d.list[4], "us_lpa.csv")
write.csv(d.list[5], "us_mpj.csv")
write.csv(d.list[6], "us_ray.csv")
write.csv(d.list[7], "us_scc.csv")
write.csv(d.list[8], "us_scf.csv")
write.csv(d.list[9], "us_scw.csv")
write.csv(d.list[10], "us_seg.csv")
write.csv(d.list[11], "us_sen.csv")
write.csv(d.list[12], "us_ses.csv")
write.csv(d.list[13], "us_sob.csv")
write.csv(d.list[14], "us_son.csv")
write.csv(d.list[15], "us_soy.csv")
write.csv(d.list[16], "us_src.csv")
write.csv(d.list[17], "us_srg.csv")
write.csv(d.list[18], "us_srm.csv")
write.csv(d.list[19], "us_tes.csv")
write.csv(d.list[20], "us_vcp.csv")
write.csv(d.list[21], "us_vcm.csv")
write.csv(d.list[22], "us_whs.csv")
write.csv(d.list[23], "us_wjs.csv")
write.csv(d.list[24], "us_wkg.csv")



