# SPI/SPEI for monthly PRISM
# MAC 08/08/18

library(SPEI)
library(raster)
library(zoo)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)

# load data
prec<-stack("/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_prec_1895_2017.grd")

# calculate SPI
funSPI <- function(x, scale=12, na.rm=TRUE,...) as.numeric((spi(x, scale=scale, na.rm=na.rm, ...))$fitted)
#rstSPEI <- calc(wtrBal, fun = funSPEI)
# parallell calc
ptm <- proc.time()
beginCluster(7)
  spiTemp <- clusterR(prec, calc, args=list(fun=funSPI))
endCluster()
proc.time() - ptm
# plot grids
my.at <- seq(-3, 3, 0.5)
mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
levelplot(spiTemp[[1212]], par.settings = mapTheme, at=my.at, margin=FALSE, main="SPI", colorkey = list(space='right'))+
  layer(sp.polygons(states))
# # check stats of dist fitting
# meanSPI<-calc(spiTemp, mean, na.rm=TRUE)
# sdSPI<-calc(spiTemp, sd, na.rm=TRUE) 

# write out files
writeRaster(spiTemp,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI12_1915_2015.grd", overwrite=TRUE )

