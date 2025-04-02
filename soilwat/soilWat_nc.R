# SOILWAT NETCDF files
# MAC 04/24/20
# ftp://ftpext.usgs.gov/pub/wr/az/flagstaff/candrews/AFRInetCDFs/WatYrWDD/

library(raster)
library(RCurl)
library(ncdf4)

# get elevation grid
URL <- "ftp://ftpext.usgs.gov/pub/wr/az/flagstaff/candrews/AFRInetCDFs/WatYrWDD/WatYrWDD_L_annual_RangeResil_SOILWAT2_CESM1-CAM5_RCP45_r1i1p1f1_gn_20191001_20990930.nc"
download.file(URL, destfile = "./soilwat/files/test.nc", method="curl")

test<-stack("./soilwat/files/test.nc")

nc<-nc_open("./soilwat/files/test.nc")
print(nc)

us<-getData('GADM', country='USA', level=2)
county<-subset(us, NAME_2=="Cochise")

ts <- t(extract(test, county, fun=mean, df=TRUE, na.rm=TRUE))

plot(ts, type="b")