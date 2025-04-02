# plot focal counties
# MAC 9/21/20

library(raster)
library(ggplot2)
library(ggmap)
library(plyr)

# get counties
# probably better way to subset with multiple counties...
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
  #c10<-subset(us,NAME_2=="Luna" & NAME_1=="New Mexico")
  #c11<-subset(us,NAME_2=="Sierra" & NAME_1=="New Mexico")
  #c12<-subset(us,NAME_2=="Grant" & NAME_1=="New Mexico")
  #c13<-subset(us,NAME_2=="Hidalgo" & NAME_1=="New Mexico")
  c14<-subset(us,NAME_2=="Gila" & NAME_1=="Arizona")
  c15<-subset(us,NAME_2=="Cherry" & NAME_1=="Nebraska")
  c16<-subset(us,NAME_2=="Grant" & NAME_1=="Nebraska")
focalCounties<-rbind(c1,c2,c3,c4,c5,c6,c7,c8,c9,c14,c15,c16) 


# ggplot version with ggmap background
focalCounties@data$ID_2=rownames(focalCounties@data)
focalCounties.polygons=fortify(focalCounties, region="ID_2")
focalCounties.df=merge(focalCounties.polygons, focalCounties@data, by.x="id", by.y="ID_2")

# add county centroids
centCoords<-as.data.frame(coordinates(focalCounties))
colnames(centCoords)<-c("long","lat")
centCoords$id<-rownames(centCoords)
focalCounties.df<-merge(focalCounties.df, centCoords, by="id")

# API key - need to get an API key registered with Google now
# instructions for API key here... https://cran.r-project.org/web/packages/ggmap/readme/README.html
# source('~/RProjects/RainlogAPI/APIkey.R')
# # get map and bounding box
# where<-geocode("Utah", source = "dsk")
# 
# USMap <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 5,
#               source = "google", maptype = "roadmap", color="bw")
# 
# # handout version
# USMap+
#   geom_polygon(data=focalCounties.df, aes(long.x,lat.x,group=group,fill=NAME_1),alpha=0.25, color='grey')+
#   geom_text(data=focalCounties.df, aes(label = NAME_2, x =long.y, y =lat.y), size=3)+ #
#   scale_fill_discrete(name = "State")+
#   ggtitle("Range Resilience Project Study Areas")+ 
#   theme(plot.title = element_text(size = 20, face = "bold"))+ # 40, 20, 20  
#   theme(legend.text=element_text(size=12), 
#         legend.title=element_text(size=12))

# write out shapefile of counties to current directory
#library(rgdal)
#writeOGR(focalCounties, ".", "focalCounties", driver="ESRI Shapefile")

# get areal summaries for each focal areas
# DEM, precip, temp, rangeland productivity
# 

# rangeland coverage from Reeves
# https://data.fs.usda.gov/geodata/rastergateway/rangelands/index.php
  # temp <- tempfile()
  # tempd <- tempdir()
  # #download raster data
  # download.file("https://data.fs.usda.gov/geodata/rastergateway/rangelands/Rangelands_v1.zip",temp)
  # 
  # unzip(temp, exdir=tempd)
  # range_raster <- raster(file.path(tempd, "tmean/tmean_9"))
  # unlink(tempd)
  
  
# worldclim variables
  climate <- getData('worldclim', var='bio', res=2.5)
  
  # recode Rich county Utah
  focalCounties@data$NAME_1<-ifelse(focalCounties@data$NAME_2=="Rich","N Utah",focalCounties@data$NAME_1)
    focalDiss<-aggregate(focalCounties, by="NAME_1", dissolve=TRUE)
  
    focalRas<- rasterize(focalDiss, climate[[1]])
  
    stats<-zonal(climate, focalRas, 'mean')
    stats<-cbind.data.frame(stats,focalDiss@data$NAME_1)
  # subset 
    stats<-stats[,c(2,6,7,13,14,15,21)]
    colnames(stats)<-c("Annual Mean Temp","Max Warmest Month",
                       "Min Coldest Month","Ann Precip",
                       "Precip Wettest Month","Precip Driest Month",
                       "Focal Area")
    stats[,c(1,2,3)]<-stats[,c(1,2,3)]/10
    
  # units - https://worldclim.org/data/v1.4/formats.html  
  # BIO1 = Annual Mean Temperature * 10
  # BIO2 = Mean Diurnal Range (Mean of monthly (max temp – min temp))
  # BIO3 = Isothermality (BIO2/BIO7) (* 100)
  # BIO4 = Temperature Seasonality (standard deviation *100)
  # BIO5 = Max Temperature of Warmest Month
  # BIO6 = Min Temperature of Coldest Month
  # BIO7 = Temperature Annual Range (BIO5-BIO6)
  # BIO8 = Mean Temperature of Wettest Quarter
  # BIO9 = Mean Temperature of Driest Quarter
  # BIO10 = Mean Temperature of Warmest Quarter
  # BIO11 = Mean Temperature of Coldest Quarter
  # BIO12 = Annual Precipitation
  # BIO13 = Precipitation of Wettest Month
  # BIO14 = Precipitation of Driest Month
  # BIO15 = Precipitation Seasonality (Coefficient of Variation)
  # BIO16 = Precipitation of Wettest Quarter
  # BIO17 = Precipitation of Driest Quarter
  # BIO18 = Precipitation of Warmest Quarter
  # BIO19 = Precipitation of Coldest Quarter
  
# MLRAs
  require(rgdal)
  mlra <- readOGR(dsn = "/home/crimmins/RProjects/LivnehDrought/shapes/mlra", layer = "mlra_v42")
  
  mlraFocals<-intersect(focalDiss, mlra)
  mlraFocals<-mlraFocals@data
    namesFocals<-mlraFocals[,c("NAME_1","MLRA_NAME")]
    namesFocals<-unique(namesFocals)
    namesFocals2<-mlraFocals[,c("NAME_1","LRR_NAME")]
    namesFocals2<-unique(namesFocals2)
    
    
# Kuchler ecoregions
# https://databasin.org/datasets/1c7a301c8e6843f2b4fe63fdb3a9fe39
    
    library(rgdal)
    # The input file geodatabase
    fgdb <- "/home/crimmins/RProjects/LivnehDrought/shapes/kuchler/outputgdb_data0_data0.gdb"
    # List all feature classes in a file geodatabase
    subset(ogrDrivers(), grepl("GDB", name))
    fc_list <- ogrListLayers(fgdb)
    print(fc_list)
    # Read the feature class
    fc <- readOGR(dsn=fgdb,layer="kuchler_original_export_redefine_NAD27_project_AlbersNAD83_RubberSheet")
    # Determine the FC extent, projection, and attribute information
    summary(fc)
    # View the feature class
    plot(fc)
    # reproject
    fcWGS <- spTransform(fc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    # intersect
    KuchFocal<-intersect(focalDiss, fcWGS)
      KuchFocal<-KuchFocal@data
      namesKuch<-KuchFocal[,c("NAME_1","VEGTYP_LAB")]
      namesKuch<-unique(namesKuch)
      namesKuch2<-KuchFocal[,c("NAME_1","FORM_LAB")]
      namesKuch2<-unique(namesKuch2)
      
    
    # summary tables
      library(dplyr)
      test<- KuchFocal %>%  
              group_by(NAME_1,VEGTYP_LAB) %>%
              summarise(totalArea = sum(Shape_Area, na.rm = TRUE))
   
      KuchVeg<- KuchFocal %>%  
        group_by(NAME_1,VEGTYP_LAB) %>%
        summarise(subArea = sum(Shape_Area, na.rm = TRUE)) %>%
        group_by(NAME_1) %>%
        mutate(totalArea = sum(subArea, na.rm = TRUE)) %>%
        mutate(percArea = round(subArea/totalArea,2)*100)
    
      KuchForm<- KuchFocal %>%  
        group_by(NAME_1,FORM_LAB) %>%
        summarise(subArea = sum(Shape_Area, na.rm = TRUE)) %>%
        group_by(NAME_1) %>%
        mutate(totalArea = sum(subArea, na.rm = TRUE)) %>%
        mutate(percArea = round(subArea/totalArea,2)*100)            
      

  # focal area climographs from worldclim data
      tmin <- getData('worldclim', var='tmin', res=2.5)
      tmax <- getData('worldclim', var='tmax', res=2.5)
      prec <- getData('worldclim', var='prec', res=2.5)
      
      # recode Rich county Utah
      focalCounties@data$NAME_1<-ifelse(focalCounties@data$NAME_2=="Rich","N Utah",focalCounties@data$NAME_1)
      focalDiss<-aggregate(focalCounties, by="NAME_1", dissolve=TRUE)
      
      focalRas<- rasterize(focalDiss, tmin[[1]])
      # extract climate vars
        tmean<-((zonal(tmin, focalRas, 'mean')/10)+(zonal(tmax, focalRas, 'mean')/10))/2
        tmean<-cbind.data.frame(tmean,focalDiss@data$NAME_1)
        prec<-zonal(prec, focalRas, 'mean')
        prec<-cbind.data.frame(prec,focalDiss@data$NAME_1)
        tmean$var<-"tmean"
        prec$var<-"prec"
        tmean<-tmean[,-1]
        prec<-prec[,-1]
        colnames(tmean)<-c(month.abb,"focalArea","var")
        colnames(prec)<-c(month.abb,"focalArea","var")
      # combine
      climate<-rbind(tmean, prec)
      
      # wide to long
      library(tidyr)
      library(ggplot2)
      library(plotly)
      climate<-gather(climate, month, value, Jan:Dec)
      climate<-spread(climate, var, value)
      climate$month<-factor(climate$month, levels = month.abb)
      
      # climograph
      p<-climate %>% ggplot() + 
        geom_bar(mapping = aes(x = month, y = prec * 30 / 100), stat = "identity", colour = gray(0.5), fill = gray(0.5)) + 
        geom_line(mapping = aes(x = month, y = tmean), group=climate$focalArea) + 
        geom_point(mapping = aes(x = month, y = tmean), size = 3, shape = 21, fill = "white") + 
        facet_wrap(~focalArea, ncol = 1)+
        #scale_x_date(name = "Month", breaks = seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = "1 month"), labels = function(date){return(month(date, label = TRUE))}) + 
        scale_y_continuous(
          name = expression("Temperature ("~degree~"C)"), 
          sec.axis = sec_axis(~ . * 100 / 30 , name = "Precipitation (mm)"), 
          limits = c(-10, 30)) + 
        theme_bw() + 
        theme(
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
        )
    
      ggplotly(p)
      
      
 