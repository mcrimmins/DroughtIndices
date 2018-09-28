# zonal time series of SPI/SPEI
# MAC 8/3/18

library(sp)
library(rgdal)
library(raster)

# get filenames 
shps <- dir("/home/crimmins/RProjects/USDM/regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps))
# set region
#region <- do.call(rbind, lapply(shps[5], rgdal::readOGR))
#region<- spTransform(region, CRS("+proj=longlat +datum=WGS84"))

# get SPI/SPEI
#spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI6_1915_2015.grd")
spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI6_1915_2015.grd")
# zonal stats
allRegionsLLGrid<-raster("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd")
spiTS<-as.data.frame(t(zonal(spi, allRegionsLLGrid, 'mean')))

 # colnames(spiTS)<-c("CALIFORNIA GRASSLAND","CALIFORNIA MIXED EVERGREEN","DESERT GRASSLAND","DESERT SHRUB",
 #   "DESERT STEPPE","GREAT BASIN GRASSLAND","GREAT BASIN SHRUB","GREAT BASIN SHRUB/STEPPE",
 #   "GREAT BASIN/SOUTHWEST FOREST", "NORTH MIXED GRASS PRAIRIE","SHORTGRASS PRAIRIE",
 #   "SOUTH MIXED GRASS PRARIE")
 colnames(spiTS)<-regionName
 spiTS <- spiTS[-c(1), ]
 spiTS$date<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
 
# filter by month
 library(lubridate)
 spiTS<- spiTS[which(month(spiTS$date)==10),]
 
# rolling sdev
library(zoo)
co.var <- function(x,na.rm=TRUE) 100*(sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm))
#test<-rollapply(data = spiTS$`DESERT SHRUB`,width=20,FUN=sd, fill=NA)
spiTSsd<-as.data.frame(rollapply(data = spiTS,width=10,FUN=sd, fill=NA))
spiTSsd$date<-spiTS$date

# melt and use ggplot
library(reshape2)
library(ggplot2)

spiTSLong<-melt(spiTS,measure.vars = 1:8)
spiTSLong$pos<-spiTSLong$value >=0

ggplot(spiTSLong, aes(x=date,y=value, fill=pos))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
  facet_wrap(~ variable, ncol = 4, nrow = 3, strip.position="top")+
  labs(x='month/year',y='SPI', title="Standardized Precip Index - October 6 month")+
  theme_bw()

spiTSsdLong<-melt(spiTSsd,measure.vars = 1:8)
ggplot(spiTSsdLong, aes(x=date, y=value))+
  geom_line()+
  facet_wrap(~ variable, ncol = 4, nrow = 3, strip.position="top")+
  labs(x='month/year',y='SPI SDev', title="October SPI-6 10yr Moving Std Dev")+
  theme_bw()

# add error ribbon to SPI plots
spiTSzonalSD<-as.data.frame(t(zonal(spi, allRegionsLLGrid, 'sd')))
#colnames(spiTSzonalSD)<-c("CALIFORNIA GRASSLAND","CALIFORNIA MIXED EVERGREEN","DESERT GRASSLAND","DESERT SHRUB",
#                   "DESERT STEPPE","GREAT BASIN GRASSLAND","GREAT BASIN SHRUB","GREAT BASIN SHRUB/STEPPE",
#                   "GREAT BASIN/SOUTHWEST FOREST", "NORTH MIXED GRASS PRAIRIE","SHORTGRASS PRAIRIE",
#                   "SOUTH MIXED GRASS PRARIE") 
colnames(spiTSzonalSD)<-regionName
spiTSzonalSD <- spiTSzonalSD[-c(1), ]
spiTSzonalSD$date<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
spiTSzonalSD<- spiTSzonalSD[which(month(spiTSzonalSD$date)==10),]
spiTSLongSD<-melt(spiTSzonalSD,measure.vars = 1:8)
# get sdev intervals
spiTSLong$sdevPos<-spiTSLong$value+spiTSLongSD$value
spiTSLong$sdevNeg<-spiTSLong$value-spiTSLongSD$value

# ggplot(spiTSLong, aes(x=date,y=value, fill=pos))+
#   geom_bar(stat = "identity", position = "identity")+
#   scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
#   geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
#   facet_wrap(~ variable, ncol = 4, nrow = 3, strip.position="top")+
#   labs(x='month/year',y='SPI', title="SPI-6")+
#   theme_bw()

ggplot(spiTSLong, aes(x=date,y=value))+
  geom_line(color="red", size=0.1)+
  geom_ribbon(aes(ymax = sdevPos, ymin = sdevNeg,linetype=NA), alpha = 0.3)+
  facet_wrap(~ variable, ncol = 4, nrow = 3, strip.position="top")+
  labs(x='month/year',y='SPI', title="Standardized Precip Index - October 6 month")+
  theme_bw()