# combine monthly livneh data in raster stacks
# MAC 08/2/18


# precip in mm, temp in C

library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

fileNames<-as.data.frame(list.files("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly"))
  colnames(fileNames)<-"files"
  fileNames$date<-as.Date(paste0(substr(fileNames$files, 26,29),substr(fileNames$files, 30,31),"01"), "%Y%m%d")

for(i in 1:nrow(fileNames)){
  monthFile<-paste0("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly/",fileNames$files[i])
  #precTemp <- raster(monthFile, varname="Prec",  ncdf=TRUE)
  tminTemp <- raster(monthFile, varname="Tmin",  ncdf=TRUE)
  tmaxTemp <- raster(monthFile, varname="Tmax",  ncdf=TRUE)
  #windTemp <- raster(monthFile, varname="wind",  ncdf=TRUE)
  taveTemp<-overlay(tminTemp, tmaxTemp, na.rm=TRUE, fun=mean)
  
  
  if (i==1){
    #prec<- precTemp
    #tmin<- tminTemp
    #tmax<- tmaxTemp
    #wind<- windTemp
    tave<- taveTemp
  }else{
    #prec <- stack(prec, precTemp)
    #tmin <- stack(tmin, tminTemp) 
    #tmax <- stack(tmax, tmaxTemp) 
    #wind <- stack(wind, windTemp)
    tave <- stack(tave, taveTemp)
  }
  print(i)
}
  
# write to file
  # write out tempGrid2 stack -- large file and may not be necessary
  #writeRaster(prec,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_prec_1915_2015.grd", overwrite=TRUE )
  #writeRaster(tmin,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_tmin_1915_2015.grd", overwrite=TRUE )
  #writeRaster(tmax,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_tmax_1915_2015.grd", overwrite=TRUE )
  # TAVE created a 5.6 GB file, wouldn't write to disk
  #writeRaster(tave,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_tave_1915_2015.grd", overwrite=TRUE )
  #writeRaster(wind,filename="/scratch/crimmins/livneh/processed/monthlyLivneh_wind_1915_2015.grd", overwrite=TRUE )
  
  # try to write locally
  writeRaster(tave,filename="/home/crimmins/RProjects/LivnehDrought/monthlyLivneh_tave_1915_2015.grd", overwrite=TRUE )
  
# western US only
  
  e <- extent(-125, -97, 25, 49)
  #prec <- crop(prec, e)	
  #tmin <- crop(tmin, e)	
  #tmax <- crop(tmax, e)	
  tave <- crop(tave, e)	
  #wind <- crop(wind, e)	
  
  #writeRaster(prec,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd", overwrite=TRUE )
  #writeRaster(tmin,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd", overwrite=TRUE )
  #writeRaster(tmax,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd", overwrite=TRUE )
  writeRaster(tave,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tave_1915_2015.grd", overwrite=TRUE )
  #writeRaster(wind,filename="/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_wind_1915_2015.grd", overwrite=TRUE )
  
  # evaluate units of data
  library(ncdf4)
  fileNames<-as.data.frame(list.files("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly"))
  colnames(fileNames)<-"files"
  fileNames$date<-as.Date(paste0(substr(fileNames$files, 26,29),substr(fileNames$files, 30,31),"01"), "%Y%m%d")
  i=1; monthFile<-paste0("/scratch/crimmins/livneh/public/Livneh.2016.Dataset/Meteorology.netCDF.monthly/",fileNames$files[i])
  print(nc_open(monthFile))
   