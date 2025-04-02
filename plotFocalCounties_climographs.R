# plot climographs from focal counties
# MAC 12/1/2020

library(raster)
#library(ggplot2)
#library(ggmap)
library(plyr)
library(tidyr)
library(ggplot2)

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

# interactive, but doesn't work with second y axis
library(plotly)
ggplotly(p)
