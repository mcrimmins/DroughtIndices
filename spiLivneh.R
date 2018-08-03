# SPI/SPEI for monthly Livneh
# MAC 08/02/18

library(SPEI)
library(raster)
library(zoo)

# set rasteroptions
rasterOptions(progress = 'text')

# load data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
tmin<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd")
tmax<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd")

# set names
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
  prec<- setZ(prec,dates)
  names(prec) <- as.yearmon(getZ(prec))
  tmin<- setZ(tmin,dates)
  names(tmin) <- as.yearmon(getZ(tmin))
  tmax<- setZ(tmax,dates)
  names(tmax) <- as.yearmon(getZ(tmax))

# calculate monthly hargreaves
  har <- function(Tmin, Tmax, lat) {
    SPEI::hargreaves(Tmin, Tmax, lat, na.rm=TRUE)
  } 
  
  lat <- setValues(tmin, coordinates(tmin)[, "y"])
  harg <- raster::overlay(tmin, tmax, lat, fun = har)
  rm(lat)
  writeRaster(harg,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd", overwrite=TRUE )
  # reload harg if necessary
  harg<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd")
  harg<- setZ(harg,dates)
  names(harg) <- as.yearmon(getZ(harg))
  
# calculate SPEI 
# https://gis.stackexchange.com/questions/277344/using-spei-function-on-time-series-from-rasterstack-in-r
  wtrBal<-prec-harg
  writeRaster(wtrBal,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd", overwrite=TRUE )
# reload harg if necessary
  wtrBal<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd")  
    wtrBal<- setZ(wtrBal,dates)
    names(wtrBal) <- as.yearmon(getZ(wtrBal))

  funSPEI <- function(x, scale=1, na.rm=TRUE,...) as.numeric((spei(x, scale=scale, na.rm=na.rm, ...))$fitted)
  #rstSPEI <- calc(wtrBal, fun = funSPEI)
  # parallell calc
  ptm <- proc.time()
    beginCluster(7)
    tempGrid <- clusterR(wtrBal, calc, args=list(fun=funSPEI))
    endCluster()
  proc.time() - ptm
  
