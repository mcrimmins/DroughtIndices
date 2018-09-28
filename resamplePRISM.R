# Resample PRISM to Livneh grid resolution
# MAC 08/09/18

library(raster)
library(zoo)
library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
#states <- getData('GADM', country='United States', level=1)

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2017-12-31"), by="month")

# load data
tmean<-stack("/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmean_1895_2017.grd")
#prec<-prec[[which(dates=="1980-01-01"):which(dates=="2017-12-01")]]

# resample to Livneh grid
livneh<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
livnehGrid<-livneh[[1212]]; rm(livneh); gc()
precResample <- resample(tmean,livnehGrid,method='bilinear', filename="/scratch/crimmins/PRISM/monthly/processed/west/resampled/resampledWESTmonthlyPRISM_tmean_1895_2017.grd")