# SPEI for monthly Livneh
# adjusts precip to total monthly 
# MAC 11/14/18

library(SPEI)
library(raster)
library(zoo)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)

# # dates sequence
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-01"), by="month")

# ADJUST LIVNEH to MONTHLY TOTALS
# days in month
# library(lubridate)
# daysINmo<-days_in_month(dates)
# # load prec data
# prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
# # multiply by days in month
# precTotal<-prec*daysINmo
# # load hargreaves, save corrected water balance
# harg<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreaves_1915_2015.grd")
# wtrBal<-precTotal-harg
# writeRaster(wtrBal,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd", overwrite=TRUE )
# # thornthwaite
# thorn<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_thornthwaite_1915_2015.grd")
# wtrBal<-precTotal-thorn
# writeRaster(wtrBal,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_thornthwaiteWaterBal_1915_2015.grd", overwrite=TRUE )
# #### 

# set names
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-01"), by="month")
#wtrBal<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_hargreavesWaterBal_1915_2015.grd")  
wtrBal<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_thornthwaiteWaterBal_1915_2015.grd")  
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
levelplot(speiTemp[[1212]], par.settings = RdBuTheme, at=my.at, margin=FALSE, main="SPEI")+
  layer(sp.polygons(states))
# check stats of dist fitting
#meanSPEI<-calc(speiTemp, mean)
#sdSPEI<-calc(speiTemp, sd)

# write out files
#writeRaster(speiTemp,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI6harg_1915_2015.grd", overwrite=TRUE )
writeRaster(speiTemp,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI12thorn_1915_2015.grd", overwrite=TRUE )



# libary Evapotranspiration
# Use processed existing data set and constants from kent Town, Adelaide
# data("processeddata")
# data("constants")
# 
# # Call ET.HargreavesSamani under the generic function ET
# results <- ET.HargreavesSamani(data, constants, ts="daily", message="yes", save.csv="yes")

