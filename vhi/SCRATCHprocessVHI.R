# process VHI into rasters
# MAC 02/26/19

library(raster)
library(stringr)
library(lubridate)

AllfileNames<-list()
l<-1

# create sequence of filenames
for (i in 1:52) {
  for (j in 1982:2018) {
    AllfileNames[l]<-paste0("VHP.G04.C07.npp.P",j,str_pad(i, 3, pad = "0"),".SM.SMN.tif")
    l<-l+1
  }
}

# read into stack
smNDVI <- stack()
for(i in 1:length(fileNames)){
  tempRast<-raster(paste0('/scratch/crimmins/vhi/smNDVI/',fileNames[i]))
  e <- extent(-125, -97, 25, 49)
  tempRast <- crop(tempRast, e)	
  smNDVI <- stack(smNDVI, tempRast)
  print(i)
}
names(smNDVI) <- seq(as.Date("2017/1/1"), as.Date("2018/12/23"), "weeks")

# county time series
# labels 
countyName<-"San Luis Obispo"
# get boundary
us<-getData('GADM', country='USA', level=2)
county<-subset(us,NAME_2==countyName)
#county<-subset(us,NAME_2==countyName & NAME_1=="Utah")
#precMonthly <- t(extract(prec, county, fun=mean, df=TRUE, na.rm=TRUE))

# extract time series

temp<-list()
for(i in 1:length(fileNames)){
  tempRast<-raster(paste0('/scratch/crimmins/vhi/smNDVI/',fileNames[i]))
  tempRast[tempRast < 0] <- NA
  temp[i] <- (extract(tempRast, county, fun=mean, df=FALSE, na.rm=TRUE))
  print(i)
}

# extract using mask
#rasCty<-rasterize(county, tempRast)
#test <- cellStats(mask(tempRast,county,inverse=FALSE),stat = "mean" ,na.rm=TRUE)
#tempRast[tempRast < 0] <- NA

#fileNames<-list()
l<-1
# read layers into stack
smNDVI <- stack()
for (j in 2017:2018) { # seq(<start>, <end>, by=2) or 1:52
  for (i in seq(1, 52, by=2) ) {
    tempRast<-raster(paste0('/scratch/crimmins/vhi/smNDVI/VHP.G04.C07.npp.P',j,str_pad(i, 3, pad = "0"),".SM.SMN.tif"))
    e <- extent(-125, -100, 25, 49)
    tempRast <- crop(tempRast, e)	
    smNDVI <- stack(smNDVI, tempRast)
    print(l)
    l<-l+1
  }
}

smNDVI[smNDVI < 0] <- NA
#names(smNDVI)
names(smNDVI)<-seq(ymd('2017-01-01'),ymd('2018-12-23'), by = '2 week')
# add in names
#dates <- seq(as.Date("2017/1/1"), as.Date("2018/12/23"), "weeks")

test <- t(extract(smNDVI, county, fun=mean, df=FALSE, na.rm=TRUE))
plot(test[2:467,], ylim=c(0.1,0.5), type="l")


# process with list of filenames
fileNames <- dir("/scratch/crimmins/vhi/smNDVI", "*.tif", full.names = TRUE)
 
  fileString<-as.data.frame(t(as.data.frame(strsplit(substr(fileNames,30,63),"[.]"))))
  rownames(fileString) <- c()
  colnames(fileString)<-c("prefix","res","sat","comPeriod","yearWeek","prod","type","fileExt")
  fileString$year<-as.numeric(substr(fileString$yearWeek, 2,5))   
  fileString$week<-as.numeric(substr(fileString$yearWeek, 6,8))
  fileString$date<-as.Date(paste(fileString$year, fileString$week, 1, sep="-"), "%Y-%U-%u")
  
# figure out what is missing 
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5  
#weekDates<-seq(ymd('1982-01-01'),ymd('2018-12-23'), by = '1 week')
  library(tidyr)
  complete(fileString, seq.Date(min(fileString$date), max(fileString$date), by="week"))
  
  
l<-1
# read layers into stack
smNDVI <- stack()
for (i in seq(1, length(fileNames), by=4)) {
  tempRast<-raster(fileNames[i])
  e <- extent(-125, -100, 25, 49)
  tempRast <- crop(tempRast, e)	
  smNDVI <- stack(smNDVI, tempRast)
  print(names(tempRast))
  l<-l+1
}

# fix values
smNDVI[smNDVI < 0] <- NA

#names(smNDVI)<-seq(ymd('1982-01-01'),ymd('2018-12-23'), by = '1 week')


#showTmpFiles()
#removeTmpFiles(h=0) # in hours
#gc()
