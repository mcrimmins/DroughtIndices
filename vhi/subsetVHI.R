# subset rasters
# adapted from processVHI.R
# MAC 06/20/19

library(raster)
#library(stringr)
#library(lubridate)
# library(plyr)
# library(grid)
# library(cowplot)

# LCNCA extent
# -110.722275,31.642275,-110.422897,32.030781
# AZ Extent
# -114.938965,31.240985,-108.940430,37.099003

# process with list of filenames
fileNames <- dir("/scratch/crimmins/vhi/smNDVI", "*.tif", full.names = TRUE)
#fileNames <- dir("/scratch/crimmins/vhi/VHItiff", "*.tif", full.names = TRUE)
# 
# read layers into stack, by=1 for all weeks
l<-1
  smNDVI <- stack()
  for (i in seq(1, length(fileNames), by=1)) {
    tempRast<-raster(fileNames[i])
    e <- extent(-114.938965,-108.940430,31.240985,37.099003)
    tempRast <- crop(tempRast, e)
    smNDVI <- stack(smNDVI, tempRast)
    print(names(tempRast))
    l<-l+1
  }
# fix values
smNDVI[smNDVI < 0] <- NA

# write Raster to file
writeRaster(smNDVI,filename='/scratch/crimmins/vhi/processed/NDVI_1982-2018_AZ.grd', overwrite=TRUE)