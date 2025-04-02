# SPI/SPEI drought frequency analysis
# MAC 11/9/18

# - Patterns of drought by MLRA
#- gridded depiction of frequency of <-1 <-2 in last 20 years
# try 3 & 12
#- do with PRISM too
#- also SPEI?
#  - lowest SPI in each pixel

library(raster)
library(maptools)
# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)
# get boundary
county<-getData('GADM', country='USA', level=2)
# load MLRA
mlra <- readShapePoly(paste0("/home/crimmins/RProjects/LivnehDrought/shapes/mlra/mlra_v42.shp"))


# load drought indices data
#spi3<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI3_1915_2015.grd")
spi12<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI12_1915_2015.grd")
#spei3<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI3harg_1915_2015.grd")
#spei12<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI12harg_1915_2015.grd")

# PRISM
#spi12<-stack("/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_SPI12_1915_2015.grd")

#writeRaster(spi12, "WESTmonthlyLivneh_SPI12_1915_2015.nc", overwrite=TRUE)


# limit to last twenty years
# set names
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
which(dates>"1995-01-01")
subSPI<- subset(spi12, which(dates>"1995-01-01"))
numMonths<-length(which(dates>"1995-01-01"))

# http://r-sig-geo.2731867.n2.nabble.com/Count-values-quot-greater-than-quot-in-rasterbrick-layers-td7586746.html
# count frequencies of values past threshold
#subSPI<-reclassify(subSPI,c(-Inf,-1,1, -1,Inf,0)) # -1 threshold
subSPI<-reclassify(subSPI,c(-Inf,-1,1, -1,Inf,0)) # -1 threshold



library(rasterVis)
myTheme<-rasterTheme(region=(brewer.pal(9, "YlOrRd")))

#my.at<- seq(0, 180, 20) # -1 SPI
my.at<- c(1,seq(5, 50, 5)) # -2 SPI
levelplot((sum(subSPI)/numMonths)*100, at=my.at, par.settings = myTheme,margin=FALSE, main="Percent of months with <-1 12mo-SPI, 1995-2015 - Livneh")+
  layer(sp.polygons(mlra,col = 'black', lwd=0.3))+
  layer(sp.polygons(states,col = 'gray40', lwd=0.3))
