# zonal time series of SPI/SPEI
# MAC 8/3/18

library(sp)
library(rgdal)
library(raster)

# get filenames 
shps <- dir("/home/crimmins/RProjects/USDM/regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
# set region
region <- do.call(rbind, lapply(shps[5], rgdal::readOGR))
region<- spTransform(region, CRS("+proj=longlat +datum=WGS84"))

# get SPI/SPEI
spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI6_1915_2015.grd")
# zonal stats
allRegionsLLGrid<-raster("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd")
spiTS<-as.data.frame(t(zonal(spi, allRegionsLLGrid, 'mean')))

 colnames(spiTS)<-c("CALIFORNIA GRASSLAND","CALIFORNIA MIXED EVERGREEN","DESERT GRASSLAND","DESERT SHRUB",
   "DESERT STEPPE","GREAT BASIN GRASSLAND","GREAT BASIN SHRUB","GREAT BASIN SHRUB/STEPPE",
   "GREAT BASIN/SOUTHWEST FOREST", "NORTH MIXED GRASS PRAIRIE","SHORTGRASS PRAIRIE",
   "SOUTH MIXED GRASS PRARIE") 
 spiTS <- spiTS[-c(1), ]
 spiTS$date<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
 # melt and use ggplot
 
 
# rolling sdev
 library(zoo)
 co.var <- function(x,na.rm=TRUE) 100*(sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm))
test<-rollapply(data = spiTS$`DESERT SHRUB`,width=20,FUN=sd, fill=NA)
