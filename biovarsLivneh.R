# DISMO Biovars from gridded climate
# 10/30/18 MAC 

library(raster)
library(rasterVis)
library(dismo)

# set rasteroptions
rasterOptions(progress = 'text')
# map layers
states <- getData('GADM', country='United States', level=1)

# load data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
tmin<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd")
tmax<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd")

test<-biovars(prec[[1:12]], tmin[[1:12]], tmax[[1:12]])
