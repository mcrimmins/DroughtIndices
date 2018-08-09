# CYVERSE Version - PRISM Download, processing and clipping
# MAC 08/08/2018

# install PRISM package from github - Do once
#library(devtools)
#install_github("ropensci/prism")

# load libraries
library(prism)
library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

# set directories - only needed for CyVerse instances
# need to open up scratch folder 'sudo chmod a+rwx /scratch'
# create output directory
dir.create("/scratch/processed", recursive = TRUE)

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2017-12-31"), by="month")

# PRECIP
dir.create("/scratch/PRISM/monthly/precip/", recursive = TRUE)
options(prism.path = "/scratch/PRISM/monthly/precip") 
  # download daily data
  get_prism_monthlys(type="ppt", year = 1895:2017, mon = 1:12, keepZip=F)
  # form into raster
  options(prism.path = "/scratch/PRISM/monthly/precip") 
  prec <- prism_stack(ls_prism_data())
  # western US only
  e <- extent(-125, -97, 25, 49)
  prec <- crop(prec, e)	
  # write to file 
  writeRaster(prec,filename="/scratch/processed/WESTmonthlyPRISM_prec_1895_2017.grd", overwrite=TRUE )
  rm(prec) # cleanup
  gc() # collect garbage

# TMAX
dir.create("/scratch/PRISM/monthly/tmax/", recursive = TRUE)
options(prism.path = "/scratch/PRISM/monthly/tmax") 
  # download daily data
  get_prism_monthlys(type="tmax", year = 1895:2017, mon = 1:12, keepZip=F)
  # form into raster
  options(prism.path = "/scratch/PRISM/monthly/tmax") 
  tmax <- prism_stack(ls_prism_data())
  # western US only
  e <- extent(-125, -97, 25, 49)
  tmax <- crop(tmax, e)	
  # write to file 
  writeRaster(tmax,filename="/scratch/processed/WESTmonthlyPRISM_tmax_1895_2017.grd", overwrite=TRUE )
  rm(tmax) # cleanup
  gc() # collect garbage

# TMIN
dir.create("/scratch/PRISM/monthly/tmin/", recursive = TRUE)
options(prism.path = "/scratch/PRISM/monthly/tmin") 
  # download daily data
  get_prism_monthlys(type="tmin", year = 1895:2017, mon = 1:12, keepZip=F)
  # form into raster
  options(prism.path = "/scratch/PRISM/monthly/tmin") 
  tmin <- prism_stack(ls_prism_data())
  # western US only
  e <- extent(-125, -97, 25, 49)
  tmin <- crop(tmin, e)	
  # write to file 
  writeRaster(tmin,filename="/scratch/processed/WESTmonthlyPRISM_tmin_1895_2017.grd", overwrite=TRUE )
  rm(tmin) # cleanup
  gc() # collect garbage    

# TMEAN
dir.create("/scratch/PRISM/monthly/tmean/", recursive = TRUE)
options(prism.path = "/scratch/PRISM/monthly/tmean") 
  # download daily data
  get_prism_monthlys(type="tmean", year = 1895:2017, mon = 1:12, keepZip=F)
  # form into raster
  options(prism.path = "/scratch/PRISM/monthly/tmean") 
  tmean <- prism_stack(ls_prism_data())
  # western US only
  e <- extent(-125, -97, 25, 49)
  tmean <- crop(tmean, e)	
  # write to file 
  writeRaster(tmean,filename="/scratch/processed/WESTmonthlyPRISM_tmean_1895_2017.grd", overwrite=TRUE )
  rm(tmean) # cleanup
  gc() # collect garbage   


