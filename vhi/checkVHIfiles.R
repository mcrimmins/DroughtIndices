# check VHI file list for completeness
# MAC 02/28/2019

library(raster)
library(stringr)
library(lubridate)


AllfileNames<-list()
allYears<-list()
allWeeks<-list()
l<-1

# create full file list
for (j in 1982:2019) {
  for (i in 1:52) {
    AllfileNames[l]<-paste0(j,str_pad(i, 3, pad = "0"))
    allYears[l]<-j
    allWeeks[l]<-i
    l<-l+1
  }
}
# full dates
fullDates<-as.data.frame(cbind(unlist(allWeeks),unlist(allYears)))
fullDates$date<-as.Date(paste(fullDates$V2, fullDates$V1, 1, sep="-"), "%Y-%U-%u")

# file list
# process with list of filenames
#fileNames <- dir("/scratch/crimmins/vhi/smNDVI", "*.tif", full.names = TRUE)
fileNames <- dir("/scratch/crimmins/vhi/VHItiff", "*.tif", full.names = TRUE)

fileString<-as.data.frame(t(as.data.frame(strsplit(substr(fileNames,30,63),"[.]"))))
  rownames(fileString) <- c()
  colnames(fileString)<-c("prefix","res","sat","comPeriod","yearWeek","prod","type","fileExt")
  fileString$year<-as.numeric(substr(fileString$yearWeek, 2,5))   
  fileString$week<-as.numeric(substr(fileString$yearWeek, 6,8))
  fileString$date<-as.Date(paste(fileString$year, fileString$week, 1, sep="-"), "%Y-%U-%u")
# merge with full dates
mergedList<-merge(fileString,fullDates,by="date", all.y=TRUE)
# find missing days
missingWeeks<-mergedList[which(is.na(mergedList$prefix)==TRUE),c("V1","V2")]



  