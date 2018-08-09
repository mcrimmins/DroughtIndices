# combine region shapefiles and create mask
# MAC 08/03/18

library(sp)
library(rgdal)
library(raster)

# get filenames 
shps <- dir("/home/crimmins/RProjects/USDM/regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps))

# get all shapes
p1 <- shapefile(shps[1])
  p1$region<-regionName[1]
  p1$regionID<-1
p2 <- shapefile(shps[2])
  p2$region<-regionName[2]
  p2$regionID<-2
p3 <- shapefile(shps[3])
  p3$region<-regionName[3]
  p3$regionID<-3
p4 <- shapefile(shps[4])
  p4$region<-regionName[4]
  p4$regionID<-4
p5 <- do.call(rbind, lapply(shps[5], rgdal::readOGR))
  p5$region<-regionName[5]
  p5$regionID<-5
p6 <- shapefile(shps[6])
  p6$region<-regionName[6]
  p6$regionID<-6
p7 <- do.call(rbind, lapply(shps[7], rgdal::readOGR))
  p7$region<-regionName[7]
  p7$regionID<-7
p8 <- shapefile(shps[8])
  p8$region<-regionName[8]
  p8$regionID<-8
# bind together
allRegions<-bind(p1,p2,p3,p4,p5,p6,p7,p8)
allRegionsLL<- spTransform(allRegions, CRS("+proj=longlat +datum=WGS84"))
# write to file
writeOGR(obj=allRegionsLL, dsn="/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLL.shp", layer = "allRegionsLL", driver="ESRI Shapefile",
         overwrite_layer = TRUE)

# load shapefiles
allRegionsLL <- do.call(rbind, lapply("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLL.shp", rgdal::readOGR))

# rasterize grid
spi<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI6_1915_2015.grd")
allRegionsGrid <- rasterize(allRegionsLL, spi[[1212]], allRegionsLL@data[,"regionID"], fun='last')
writeRaster(allRegionsGrid,filename="/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd", overwrite=TRUE )

# load and map grid
library(rasterVis)
states <- getData('GADM', country='United States', level=1)
allRegionsGrid<-raster("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd")
# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#darkcols<-sample(col_vector, 8)
darkcols <- brewer.pal(8, "Set2")
classMap<-as.factor(allRegionsGrid)
rat <- levels(classMap)[[1]]
# cluster names
#rat[["Name"]]<-c("CALIFORNIA GRASSLAND","CALIFORNIA MIXED EVERGREEN","DESERT GRASSLAND","DESERT SHRUB",
#               "DESERT STEPPE","GREAT BASIN GRASSLAND","GREAT BASIN SHRUB","GREAT BASIN SHRUB/STEPPE",
#               "GREAT BASIN/SOUTHWEST FOREST", "NORTH MIXED GRASS PRAIRIE","SHORTGRASS PRAIRIE",
#               "SOUTH MIXED GRASS PRARIE")
rat[["Name"]]<-regionName
levels(classMap) <- rat 
# plot classified map
levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="Rangeland Study Regions")+
  layer(sp.polygons(states))

#levelplot(prec[[1212]], par.settings = GrTheme, at=my.at, margin=FALSE, main="SPI", colorkey = list(space='right'))+
#  layer(sp.polygons(allRegionsLL))



plot(mask(prec[[1212]], allRegionsLL))