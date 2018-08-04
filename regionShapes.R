# combine region shapefiles and create mask
# MAC 08/03/18

library(sp)
library(rgdal)

# get filenames 
shps <- dir("/home/crimmins/RProjects/USDM/regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]

# get all shapes
p1 <- shapefile(shps[1])
p2 <- shapefile(shps[2])
p3 <- shapefile(shps[3])
p4 <- shapefile(shps[4])
p5 <- do.call(rbind, lapply(shps[5], rgdal::readOGR))
p6 <- shapefile(shps[6])
p7 <- do.call(rbind, lapply(shps[7], rgdal::readOGR))
p8 <- shapefile(shps[8])
# bind together
allRegions<-bind(p1,p2,p3,p4,p5,p6,p7,p8)
allRegionsLL<- spTransform(allRegions, CRS("+proj=longlat +datum=WGS84"))
# write to file
writeOGR(obj=allRegionsLL, dsn="/home/crimmins/RProjects/USDM/regions/AllRegions", layer = "allRegionsLL", driver="ESRI Shapefile")

# load shapefiles
allRegionsLL <- do.call(rbind, lapply("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLL.shp", rgdal::readOGR))

# rasterize grid
spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI6_1915_2015.grd")
allRegionsGrid <- rasterize(allRegionsLL, spi[[1212]], 'FORM_LAB', fun='last')
writeRaster(allRegionsGrid,filename="/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd", overwrite=TRUE )


#levelplot(prec[[1212]], par.settings = GrTheme, at=my.at, margin=FALSE, main="SPI", colorkey = list(space='right'))+
#  layer(sp.polygons(allRegionsLL))



plot(mask(prec[[1212]], allRegionsLL))