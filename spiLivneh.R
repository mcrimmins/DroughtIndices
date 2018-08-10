# SPI/SPEI for monthly Livneh
# MAC 08/02/18

library(SPEI)
library(raster)
library(zoo)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)

# load data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
#tmin<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd")
#tmax<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd")
tave<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tave_1915_2015.grd")

# set names
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
# set dates to grids
  # prec<- setZ(prec,dates)
  # names(prec) <- as.yearmon(getZ(prec))
  # tmin<- setZ(tmin,dates)
  # names(tmin) <- as.yearmon(getZ(tmin))
  # tmax<- setZ(tmax,dates)
  # names(tmax) <- as.yearmon(getZ(tmax))

# calculate monthly hargreaves
  # har <- function(Tmin, Tmax, lat) {
  #   SPEI::hargreaves(Tmin, Tmax, lat, na.rm=TRUE)
  # }
  # 
  # lat <- setValues(tmin, coordinates(tmin)[, "y"])
  # harg <- raster::overlay(tmin, tmax, lat, fun = har)
  # rm(lat)
  # writeRaster(harg,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd", overwrite=TRUE )
  # # reload harg if necessary
  # harg<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd")
  # harg<- setZ(harg,dates)
  # names(harg) <- as.yearmon(getZ(harg))
  
# # calculate monthly thornthwaite
#   tw <- function(Tave, lat) {
#     SPEI::thornthwaite(Tave, lat, na.rm=TRUE)
#   }
# 
#   a <- raster(tave)
#   lat <- init(a, "y")
#   thorn <- raster::overlay(tave, lat, fun = Vectorize(tw)) # add filename
  
  # VECTORIZED thornthwaite ET ->
  # https://stackoverflow.com/questions/50452135/thornthwaite-evapotranspiration-on-a-raster-dataset-error-formula-not-vectoris
  th <- function(Tave, lat) {
    as.vector(SPEI::thornthwaite(as.vector(Tave), lat, na.rm=TRUE))
  } 
  a <- raster(tave)
  lat <- init(a, "y")
  out <- brick(tave, values=FALSE)
# for (i in 1:ncell(tave)) {
#    out[i] <- th(tave[i], lat[i])
#  }
  
  # use parallel 
  library(parallel)
  f2<-function(i){return(th(tave[i], lat[i]))}
 # cores <- detectCores() - 1
#  cl <- makeCluster(7, type='FORK')
#    out<-mclapply(1:ncell(tave), f2, mc.cores=7)
#  stopCluster(cl)
  library(pbmcapply)
  cl <- makeCluster(7, type='FORK')
    out<-pbmclapply(1:ncell(tave), f2, mc.cores=7)
  stopCluster(cl)
  
  
  writeRaster(harg,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_thornthwaite_1915_2015.grd", overwrite=TRUE )
  # reload harg if necessary
  harg<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd")
  harg<- setZ(harg,dates)
  names(harg) <- as.yearmon(getZ(harg))
  
  
# calculate SPEI 
# https://gis.stackexchange.com/questions/277344/using-spei-function-on-time-series-from-rasterstack-in-r
  # wtrBal<-prec-harg
  # writeRaster(wtrBal,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd", overwrite=TRUE )
# reload harg if necessary
  # set names
  dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
  wtrBal<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd")  
    wtrBal<- setZ(wtrBal,dates)
    names(wtrBal) <- as.yearmon(getZ(wtrBal))

  funSPEI <- function(x, scale=12, na.rm=TRUE,...) as.numeric((spei(x, scale=scale, na.rm=na.rm, ...))$fitted)
  #rstSPEI <- calc(wtrBal, fun = funSPEI)
  # parallell calc
  ptm <- proc.time()
    beginCluster(7)
    speiTemp <- clusterR(wtrBal, calc, args=list(fun=funSPEI))
    endCluster()
  proc.time() - ptm
# plot grids
  my.at <- seq(-3, 3, 0.5)
  #mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
  levelplot(speiTemp[[1]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="SPEI")+
    layer(sp.polygons(states))
# check stats of dist fitting
  meanSPEI<-calc(speiTemp, mean)
  sdSPEI<-calc(speiTemp, sd)

# write out files
  writeRaster(speiTemp,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI12_1915_2015.grd", overwrite=TRUE )
  
  
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
  
  