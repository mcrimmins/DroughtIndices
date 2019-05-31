# get USDM by county
# code from plotAllUSDM.R
# MAC 11/5/18

library(sp)
library(rgdal)
#library(rgeos)
#library(maptools)
library(raster)
library(plyr)

# load data
load("/home/crimmins/RProjects/USDM/USDMRaster_2001_2018_16km.RData")

# labels 
countyName<-"Grant"

# get boundary
us<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)
county<-subset(us,NAME_2==countyName & NAME_1=="Nebraska")

# extract USDM from grid
extractUSDM <- (extract(tempGrid2, county, df=TRUE))

# build table of results
tempDF<-data.frame(key=integer()) 
tempDF<-as.data.frame(c(NA,seq(0,4,1)))
colnames(tempDF)[1]<-"key"
# loop through cols
for(i in 2:ncol(extractUSDM)){ 
  tempCol<-count(extractUSDM[,i])
  tempCol$freq<-tempCol$freq/sum(tempCol$freq)
  tempDF<-merge(tempDF,tempCol, by.x="key", by.y="x", all.x=TRUE)
}
# clean up data frame
tempDF<-as.data.frame(t(tempDF))
tempDF<-tempDF[-1,] 
colnames(tempDF)<-c("Dry","Moderate","Severe","Extreme","Exceptional","None")
# add dates
tempDF$week<-fileNames$date
tempDF[is.na(tempDF)] <- 0
#tempDF$region<-regionName

# make stacked bar
library(reshape2)
library(ggplot2)
library(cowplot)
# melt data frame
tempDFmelt<-melt(tempDF,measure.vars = c("Dry","Moderate","Severe","Extreme","Exceptional","None"))
tempDFmelt$variable <- factor(tempDFmelt$variable, levels = c("None","Dry","Moderate","Severe","Extreme","Exceptional"))
tempDFmelt$value<-tempDFmelt$value*100
# plot
p<-ggplot(tempDFmelt, aes(x=week, y=value, fill=variable))+
  #facet_wrap(~region, ncol = 1)+
  geom_area(alpha = 0.6)+
  scale_fill_manual(values = c("white","yellow", "burlywood", "orange","red","firebrick"), name = "Category")+
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")+
  labs(title=(paste0(countyName," County % area coverage by USDM category")),
       x ="Date", y = "% of region covered")+
  theme_bw()

p

# use save_plot() instead of ggsave() when using cowplot
save_plot("./climateProfiles/USDMplot.png", p,
          base_height = 3, base_width = 8 # make room for figure legend
)

