# process NOAA climGrid data into rasters - DOWNLOAD VERSION
# from ftp://ftp.ncdc.noaa.gov/pub/data/climgrid/
# MAC 10/03/18

# load libraries, set dir ----
library(RCurl)
library(sp)
library(rgdal)
library(raster)

# Download data ----
# get directory listing and find most recent prcp file
url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/climgrid/'
filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filelist<-(unlist(strsplit(filenames,"\n")))

filelistExt<-as.data.frame(sapply(strsplit(filelist, "_monthly_"), "[", 2), stringsAsFactors=FALSE)
  colnames(filelistExt)<-"filename"
  filelistExt$year<-substr(filelistExt$filename,1,4)
  filelistExt$month<-substr(filelistExt$filename,5,6)
  filelistExt$filenameLong<-filelist
  filelistExt <- filelistExt[order(filelistExt$year, filelistExt$month),]
  filelistExt$date<-as.Date(paste(filelistExt$year, filelistExt$month, 1, sep='-'))
# remove readme file
filelistExt<-filelistExt[-nrow(filelistExt), ]
# remove duplicates
filelistExt<-filelistExt[!rev(duplicated(rev(filelistExt$date))),]
# thin file list to date range of interest
filelistExt<-filelistExt[which(filelistExt$date<=as.Date("2019-12-1")),]

# empty stack
tempStack <- stack()

# set the variable 
# .prcp.conus.pnt; .tave.conus.pnt; .tmax.conus.pnt; .tmin.conus.pnt
climVar=".prcp.conus.pnt"

# which rows for update time period
minRow<-min(which(filelistExt$year>=2018))

# download precip file and format into data frame
ptm <- proc.time()
for(i in minRow:nrow(filelistExt)){
  urlFile<-paste0(url,filelistExt$filenameLong[i])
  download.file(urlFile,"./temp/temp.tar.gz")
  tarList<-untar("./temp/temp.tar.gz", list=TRUE)
  untar("./temp/temp.tar.gz", exdir ="./temp")
  unlink("./temp/temp.tar.gz")
  filesTemp<-list.files("./temp")
  getYear<-substr(filesTemp[1], 1, 4)
  getMonth<-substr(filesTemp[1], 5, 6)
  
  tempAve<- read.table(paste0("./temp/",getYear,getMonth,climVar))
  tempAve<-tempAve[,c(2,1,3)]
  colnames(tempAve)<-c("x","y","z")
  #tempAve$month<-as.numeric(getMonth)
  #tempAve$year<-as.numeric(getYear)
  
  # convert to raster
  e <- extent(tempAve[,1:2])
  # get resolutions
  latDist<-pointDistance(c(e@xmin, e@ymax), c(e@xmin, e@ymin), lonlat=TRUE)
  lonDist<-pointDistance(c(e@xmin, e@ymax), c(e@xmax, e@ymax), lonlat=TRUE)
  #emptyGrid <- raster(e, nrows=round(latDist/5000), ncols=round(lonDist/5000))
  emptyGrid <- raster(e, resolution=0.0417)
  tempMont <- rasterize(tempAve[, 1:2], emptyGrid, tempAve[,3], fun=mean, na.rm=TRUE)
  #proj4string(x)=CRS("+init=epsg:4326")
  #plot(x)
  tempStack <- stack(tempStack,tempMont)
  
  unlink(paste0("./temp/",filesTemp))
  print(paste0("Extracting: ", getMonth,"-",getYear))
}
proc.time() - ptm
# assign names to layers
names(tempStack)<-filelistExt$date[minRow:nrow(filelistExt)]

# crop exten to Western US
e <- extent(-125, -97, 25, 49)
tempStack <- crop(tempStack, e)	

# open existing file to append to
grid<-stack("/scratch/crimmins/climgrid/processed/WESTmonthly.prcp.conus.pnt_1895_2017.grd")
# stack together
grid<-stack(grid,tempStack)

# write out new stack
nameFile<-paste0("/scratch/crimmins/climgrid/processed/WESTmonthly",climVar,"_",min(filelistExt$year),"_",max(filelistExt$year),".grd")
#writeRaster(tempStack,filename=nameFile, overwrite=TRUE )
writeRaster(grid,filename=nameFile, overwrite=TRUE )

