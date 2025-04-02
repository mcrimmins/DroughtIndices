# VHI data testing
# https://www.star.nesdis.noaa.gov/smcd/emb/vci/VH/vh_ftp.php
# MAC 02/25/2019

library(raster)

# get VHI
#URL <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/VH/VHP.G04.C07.npp.P2018001.VH.nc"
#download.file(URL, destfile = "./vhi/vh.nc", method="curl")

URL<-"ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/VHP.G04.C07.npp.P2018052.VH.VHI.tif"
download.file(URL, destfile = "./vhi/vh.tif", method="curl")

# # get SM
# URL <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/VH/VHP.G04.C07.npp.P2018052.SM.nc"
# download.file(URL, destfile = "./vhi/sm.nc", method="curl")

# get SM
URL <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/VHP.G04.C07.npp.P2018050.SM.SMN.tif"
download.file(URL, destfile = '/scratch/crimmins/vhi/smNDVI/VHP.G04.C07.npp.P2018050.SM.SMN.tif', method="curl")
#sm<-raster("./vhi/sm.tif")

# get SM
URL <- "ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/VH/VHP.G04.C07.npp.P2018052.ND.nc"
download.file(URL, destfile = "./vhi/nd.nc", method="curl")

VH<-raster("./vhi/vh.nc", var="VHI")
VHtif<-raster("./vhi/vh.tif")
# fix CRS https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
#crs(VH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(VH) <- "+proj=longlat +datum=WGS84 +lat_1=75.024 +lat_2=-55.152 +lon_0=0 +ellps=WGS84"
bb <- extent(-180, 180, -55.152, 75.024)
test<-setExtent(VH,bb)
test<-flip(test, direction='y')

SM<-raster("./vhi/sm.nc")
ND<-raster("./vhi/nd.nc")

# # get 16km VHI
# URL<-"ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_16km/VH/VHP.G16.C07.npp.P2018052.ND.nc"
# download.file(URL, destfile = "./vhi/vh16.nc", method="curl")
# 
# test<-raster("./vhi/vh16.nc")
plot(test)


library(stringr)
library(RCurl)
fileNames<-list()
l<-1

# create sequence of filenames
for (i in 1:52) {
  for (j in 2022:2022) {
    fileNames[l]<-paste0("VHP.G04.C07.npp.P",j,str_pad(i, 3, pad = "0"),".SM.SMN.tif")
    l<-l+1
  }
}

for (i in 1:length(fileNames)) {
  URL<-paste0("ftp://ftp.star.nesdis.noaa.gov/pub/corp/scsb/wguo/data/Blended_VH_4km/geo_TIFF/",fileNames[i])
  download.file(URL, destfile = paste0('/scratch/crimmins/vhi/smNDVI/',fileNames[i]), method="auto")
  print(i)
  #Sys.sleep(3)
}

download.file(URL, destfile = "temp.tif")
