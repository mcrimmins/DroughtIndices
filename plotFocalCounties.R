# combine region shapefiles and create mask
# MAC 08/03/18

library(sp)
library(rgdal)
library(raster)

# get filenames 
shps <- dir("/home/crimmins/RProjects/USDM/regions/AllRegions", "*.shp", full.names = TRUE)
shps<-shps[-grep(".xml", shps)]
regionName<-sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(shps))

# get county names
# get boundary
# countyNames<-c("San Luis Obispo", "Santa Barbara", "Humboldt", "Box Elder",
#                "Rich", "Weld","Logan","Morgan","Luna","Sierra","Grant","Hidalgo",
#                "Gila","Cherry","Grant")

#us<-getData('GADM', country='USA', level=2)
# #county<-subset(us,NAME_2==countyNames)
# idx<-match(countyNames, us$NAME_2)
# idx<-idx[1:14]
# focalCounties<-us[idx,]
# # add in Grant, NM
# grantNM<-subset(us,NAME_2=="Grant" & NAME_1=="New Mexico")
# focalCounties<-rbind(focalCounties,grantNM)

# get counties
us<-getData('GADM', country='USA', level=2)
  c1<-subset(us,NAME_2=="San Luis Obispo" & NAME_1=="California")
  c2<-subset(us,NAME_2=="Santa Barbara" & NAME_1=="California")
  c3<-subset(us,NAME_2=="Humboldt" & NAME_1=="Nevada")
  #c4<-subset(us,NAME_2=="Box Elder" & NAME_1=="Utah")
  c4<-subset(us,NAME_2=="Garfield" & NAME_1=="Utah")
  c5<-subset(us,NAME_2=="Kane" & NAME_1=="Utah")
  c6<-subset(us,NAME_2=="Rich" & NAME_1=="Utah")
  c7<-subset(us,NAME_2=="Weld" & NAME_1=="Colorado")
  c8<-subset(us,NAME_2=="Logan" & NAME_1=="Colorado")
  c9<-subset(us,NAME_2=="Morgan" & NAME_1=="Colorado")
  c10<-subset(us,NAME_2=="Luna" & NAME_1=="New Mexico")
  c11<-subset(us,NAME_2=="Sierra" & NAME_1=="New Mexico")
  c12<-subset(us,NAME_2=="Grant" & NAME_1=="New Mexico")
  c13<-subset(us,NAME_2=="Hidalgo" & NAME_1=="New Mexico")
  c14<-subset(us,NAME_2=="Gila" & NAME_1=="Arizona")
  c15<-subset(us,NAME_2=="Cherry" & NAME_1=="Nebraska")
  c16<-subset(us,NAME_2=="Grant" & NAME_1=="Nebraska")
focalCounties<-rbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16)  
  
# focalRegions 
# SW NM = 9
# S CAL = 2
# NW Utah =4
# NW NV = 3
# NE Utah = 5
# NE Colo = 8
# C AZ = 10

#focalCounties$regions<-c("S CAL (2)","S CAL (2)","NW NEV (3)", "NW UTAH (4)","NE UTAH (5)","NE COLO (8)","NE COLO (8)",
#  "NE COLO (8)","SW NM (9)","SW NM (9)","SW NM (9)","SW NM (9)","C AZ (10)","N CENT (12)","N CENT (12)")
focalCounties$regions<-c("S CAL","S CAL","NW NEV", "S UTAH","S UTAH","NE UTAH","NE COLO","NE COLO",
                         "NE COLO","SW NM","SW NM","SW NM","SW NM","C AZ","N CENT","N CENT")
focalCounties$regions<-as.factor(focalCounties$regions)

# load and map grid
library(rasterVis)
states <- getData('GADM', country='United States', level=1)
allRegionsGrid<-raster("/home/crimmins/RProjects/LivnehDrought/shapes/allRegionsLLgrid.grd")
# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#darkcols<-sample(col_vector, 8)
darkcols <- brewer.pal(8, "Set2")
darkcols2 <- brewer.pal(8, "Set1")
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
          margin=FALSE, main="Rangeland Resilience Study Counties")+
  layer(sp.polygons(states,col = 'gray40', lwd=0.7))+
  layer(sp.polygons(focalCounties, col = 'black', lwd=1))+
  layer(sp.text(coordinates(focalCounties),txt = focalCounties$NAME_2,
                pos = 1,cex=0.8,col="black"))

p1<-levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
          margin=FALSE, main="Rangeland Resilience Study Counties")
  
p<-spplot(focalCounties, "regions", col.regions=darkcols2, main="Rangeland Resilience Study Counties",
          xlim=c(-125,-99), ylim=c(30,45))+
  layer(sp.polygons(states,col = 'gray40', lwd=0.7))+
  layer(sp.polygons(focalCounties, col = 'black', lwd=1))+
  layer(sp.text(coordinates(focalCounties),txt = focalCounties$NAME_2,
                pos = 0,cex=0.6,col="black"))

p1+p

# write to image file
#png(paste0("./",names(precip[[day1]]),"_",names(precip[[day2]]),"_Composite500mbPlot.png"), width = 7, height = 5, units = "in", res = 300L)
png("/home/crimmins/RProjects/LivnehDrought/RRCounties.png", width = 11, height = 8.5, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
## now add the text 
dev.off()

# ggplot version with ggmap background
library(ggplot2)
library(ggmap)
library(plyr)

focalCounties@data$ID_2=rownames(focalCounties@data)
focalCounties.polygons=fortify(focalCounties, region="ID_2")
focalCounties.df=merge(focalCounties.polygons, focalCounties@data, by.x="id", by.y="ID_2")

# add county centroids
centCoords<-as.data.frame(coordinates(focalCounties))
colnames(centCoords)<-c("long","lat")
centCoords$id<-rownames(centCoords)
focalCounties.df<-merge(focalCounties.df, centCoords, by="id")

# API key
source('~/RProjects/RainlogAPI/APIkey.R')
# get map and bounding box
where<-geocode("Utah", source = "dsk")

USMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 5,
                  source = "google", maptype = "roadmap", color="bw")

ggplot(focalCounties.df) + 
  aes(long,lat,group=group,fill=NAME_2) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Focal Counties")

# handout version
image=USMap+
  geom_polygon(data=focalCounties.df, aes(long.x,lat.x,group=group,fill=NAME_1),alpha=0.25, color='grey')+
  geom_text(data=focalCounties.df, aes(label = NAME_2, x =long.y, y =lat.y), size=3)+ #
  scale_fill_discrete(name = "State")+
  ggtitle("Range Resilience Project Study Areas")+ 
  theme(plot.title = element_text(size = 20, face = "bold"))+ # 40, 20, 20  
  theme(legend.text=element_text(size=12), 
        legend.title=element_text(size=12)
         )
# poster size 
image=USMap+
  geom_polygon(data=focalCounties.df, aes(long.x,lat.x,group=group,fill=NAME_1),alpha=0.25, color='grey')+
  geom_text(data=focalCounties.df, aes(label = NAME_2, x =long.y, y =lat.y), size=12)+ #
  scale_fill_discrete(name = "State")+
  ggtitle("Range Resilience Project Study Areas")+ 
  theme(plot.title = element_text(size = 60, face = "bold"))+ # 40, 20, 20  
  theme(legend.text=element_text(size=40), 
        legend.title=element_text(size=40)
  )
 
  #geom_path(color="white") +
  #coord_equal() 


# Write image to file
ggsave(filename="test.png",plot=image,width=11,height=8.5,units="in")

# Write image to file
ggsave(filename="test.png",plot=image,width=42,height=42,units="in")