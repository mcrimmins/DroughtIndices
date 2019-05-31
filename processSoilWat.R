# Mess with SoilWat data from J. Bradford
# MAC 12/13/18

library(raster)
library(plyr)
library(maptools)

#rasterOptions(tmpdir="/home/crimmins/RProjects/TopoWx/tmpFiles")
rasterOptions(progress = 'text')

regions <-  c( "CaliforniaAnnual", "ColdDeserts", "HotDeserts", "NorthernMixedSubset", "SGS")

#Specify data directory
dir.AFRI <- "./soilwat"
fileNames<-(list.files("./soilwat"))

#Load raster (can name this whatever you want)
AFRI_RegionSite_Raster <- raster(file.path(dir.AFRI, "RasterbySiteID3.tif"))

#Quick look at the structure of the raster
#AFRI_RegionSite_Raster

#create a dataframe with values from the raster (so we can link data from the response variables to it)
rastvals <- as.data.frame(values(AFRI_RegionSite_Raster))
names(rastvals) <- "RegionSite"

#Load a dataset and plot - showing soilwat output
varName<-load(file.path(dir.AFRI, fileNames[1])) #loads file annualprecip
swatData<-get(varName)
#str(fileNames[1])
#rownames(swatData)

#add label to dataset based on row names
swatData$label <- row.names(swatData)
  sitenumENDpos = as.integer(regexpr('_', swatData$label) )
  Site <- as.integer(substr(swatData$label, 1, sitenumENDpos-1) )
  Regionname <- substr(swatData$label, 8, 9)
  Regionnum <- unlist(sapply(Regionname, FUN= function(x) grep(x, regions)) )
  swatData$RegionSite <- Regionnum*1000000 + Site

swatData_joindat <- join(rastvals, swatData, by="RegionSite")
# dim(annualprecip_joindat)
# str(annualprecip_joindat)

#use joined data to populate values for a raster
# AnnualPrecip2002 <- AFRI_RegionSite_Raster
# values(AnnualPrecip2002) <- annualprecip_joindat[, "2002"]
# plot(AnnualPrecip2002)

# put all into one stack
swatDataStack <- stack(replicate(length(seq(1915,2015,1)), AFRI_RegionSite_Raster))
for (i in 1:length(seq(1915,2015,1))){
  values(swatDataStack[[i]]) <- swatData_joindat[, i+1]
}

# correlate soilWat and SPI grids - look at topoWx data
spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI12_1915_2015.grd")
#spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI3_1915_2015.grd")
# subset to 1 month per year
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
  subSPI<- subset(spi,which(format(dates,"%m")=="12"))
# change extent of SPI to match SoilWat data
  subSPI<-crop(subSPI, extent(swatDataStack))
# correlate grids
corrGrid<-corLocal(subSPI,swatDataStack, test=TRUE, method="pearson")  
plot(corrGrid, main=paste0("SPI-12 vs ", varName))
#

# extract time series
# map layers
states <- getData('GADM', country='United States', level=1)
# get boundary
county<-getData('GADM', country='USA', level=2)
# load MLRA
mlra <- readShapePoly(paste0("/home/crimmins/RProjects/LivnehDrought/shapes/mlra/mlra_v42.shp"))
# extract time series for MLRA
mlraSub<-subset(mlra,MLRA_NAME=="Mojave Desert")
# extract USDM from grid
mlraTS <- (extract(swatDataStack, mlraSub, df=TRUE))
mlraTS<-(colMeans(mlraTS, na.rm=TRUE))
  mlraTS<-mlraTS[2:length(mlraTS)]
# try out SCI standardization package
# library(SCI)
## SAMPLE CODE
  ## create artificial data, resembling precipitation
  # set.seed(101)
  # n.years <- 60
  # date <- rep(1:n.years,each=12) + 1950 + rep((0:11)/12,times=n.years)
  # PRECIP <- (0.25*sin( 2 * pi * date) + 0.3)*rgamma(n.years*12, shape = 3, scale = 1)
  # PRECIP[PRECIP<0.1] <- 0
  # 
  # ## apply SCI transformation
  # spi.para <- fitSCI(PRECIP,first.mon=1,time.scale=6,distr="gamma",p0=TRUE)
  # spi <- transformSCI(PRECIP,first.mon=1,obj=spi.para)
  # plot(date,spi,t="l")
  
  
