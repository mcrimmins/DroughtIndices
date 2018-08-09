# combine monthly PRISM data in raster stacks
# MAC 08/6/18

# make sure precip in mm, temp in C

library(raster)
library(prism)

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2017-12-31"), by="month")

# precip
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip") 
prec <- prism_stack(ls_prism_data())
writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_prec_1895_2017.grd", overwrite=TRUE )

# tmean
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmean") 
tmean <- prism_stack(ls_prism_data())
writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_tmean_1895_2017.grd", overwrite=TRUE )

# tmax
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmax") 
tmax <- prism_stack(ls_prism_data())
writeRaster(tmax,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_tmax_1895_2017.grd", overwrite=TRUE )

# tmin
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmin") 
tmin <- prism_stack(ls_prism_data())
writeRaster(tmin,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_tmin_1895_2017.grd", overwrite=TRUE )

# western US only
e <- extent(-125, -97, 25, 49)
prec <- crop(prec, e)	
tmean <- crop(tmean, e)	
tmax <- crop(tmax, e)	
tmin <- crop(tmin, e)	

writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_prec_1895_2017.grd", overwrite=TRUE )
writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmean_1895_2017.grd", overwrite=TRUE )
writeRaster(tmax,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmax_1895_2017.grd", overwrite=TRUE )
writeRaster(tmin,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmin_1895_2017.grd", overwrite=TRUE )

#showTmpFiles()
#removeTmpFiles(h=0) # in hours
