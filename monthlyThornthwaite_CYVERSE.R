# Thornthwaite Calc on Livneh data for CYVERSE
# MAC 08/08/2018

# installing parts of SPEI
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/goftest/goftest_1.0-3.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")

library(SPEI)
library(raster)
library(zoo)
#library(rasterVis)

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1915-2015 PRISM data
dates=seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")

# load data from local
tave<-stack("/scratch/processed/WESTmonthlyLivneh_tave_1915_2015.grd")
tave<-tave[[which(dates=="2015-01-01"):which(dates=="2015-03-01")]] 

# # calculate monthly thornthwaite
#   tw <- function(Tave, lat) {
#     SPEI::thornthwaite(Tave, lat, na.rm=TRUE)
#   }
# 
# a <- raster(tave)
# lat <- init(a, "y")
# thorn <- raster::overlay(tave, lat, fun = Vectorize(tw), filename="/scratch/processed/WESTmonthlyLivneh_thornthwaite_1915_2015.grd") # add filename

# VECTORIZED thornthwaite ET ->
# https://stackoverflow.com/questions/50452135/thornthwaite-evapotranspiration-on-a-raster-dataset-error-formula-not-vectoris
th <- function(Tave, lat) {
  as.vector(SPEI::thornthwaite(as.vector(Tave), lat, na.rm=TRUE))
} 
a <- raster(tave)
lat <- init(a, "y")
#lon <- init(a, "x")
out <- brick(tave, values=FALSE)
# for (i in 1:ncell(tave)) {
#   out[i] <- th(tave[i], lat[i])
#   print(i/ncell(tave)*100)
#  }

# use parallel 
library(parallel)
#f2<-function(i){return(th(tave[i], lat[i]))}
f2<-function(i){out[i] <- th(tave[i], lat[i])}

# cores <- detectCores() - 1
#  cl <- makeCluster(7, type='FORK')
#    out<-mclapply(1:ncell(tave), f2, mc.cores=7)
#  stopCluster(cl)
library(pbmcapply)
cl <- makeCluster(7, type='FORK')
out<-pbmclapply(1:ncell(tave), f2, mc.cores=7)
stopCluster(cl)

# use setValues to assign back to out brick
# use setValues to assign back to out brick
outMat<-matrix(unlist(out), ncol = nlayers(tave), byrow = TRUE) # ncol number of months
tempGrid <- brick(tave, values=FALSE)
tempGrid<-setValues(tempGrid, outMat)

# coerce back into raster
# df.raster <- as.data.frame(matrix(unlist(out), ncol = 72, byrow = TRUE))
# df.raster$lat<-getValues(lat)
# df.raster$lon<-getValues(lon)
# thornGrid <- rasterFromXYZ(df.raster, res=c(0.0625,0.0625))

writeRaster(harg,filename="/scratch/processed/WESTmonthlyLivneh_thornthwaite_1915_2015.grd", overwrite=TRUE )