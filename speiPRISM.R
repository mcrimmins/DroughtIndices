# SPEI for monthly PRISM
# MAC 08/08/18

library(SPEI)
library(raster)
library(zoo)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2017-12-01"), by="month")

# load RESAMPLED data
#harg<-stack("/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_hargreaves_1895_2017.grd")
#prec<-stack("/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_prec_1895_2017.grd")
# prec<-prec[[which(dates=="1915-01-01"):which(dates=="2015-12-01")]]
# harg<-harg[[which(dates=="1915-01-01"):which(dates=="2015-12-01")]]
# wtrBal<-prec-harg
# writeRaster(wtrBal,filename="/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_hargreavesWaterBal_1915_2015.grd", overwrite=TRUE )


# set names
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-01"), by="month")
wtrBal<-stack("/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_hargreavesWaterBal_1915_2015.grd")  
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
# my.at <- seq(-3, 3, 0.5)
# #mapTheme <- rasterTheme(region=brewer.pal(11,"Spectral"))
# levelplot(speiTemp[[1212]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="SPEI")+
#   layer(sp.polygons(states))
# check stats of dist fitting
#meanSPEI<-calc(speiTemp, mean)
#sdSPEI<-calc(speiTemp, sd)

# write out files
writeRaster(speiTemp,filename="/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_SPEI12harg_1915_2015.grd", overwrite=TRUE )




# libary Evapotranspiration
# Use processed existing data set and constants from kent Town, Adelaide
data("processeddata")
data("constants")

# Call ET.HargreavesSamani under the generic function ET
results <- ET.HargreavesSamani(data, constants, ts="daily", message="yes", save.csv="yes")

