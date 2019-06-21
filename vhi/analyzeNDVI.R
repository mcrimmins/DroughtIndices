# process VHI adn NDVI data into rasters, time series
# adapted from processVHI.R
# MAC 06/21/19

library(raster)
library(leaflet)


# CHANGE NDVI PRODUCT TYPE
rasterData<-stack('/scratch/crimmins/vhi/processed/smNDVI_1982-2018_LCNCA.grd') # NDVI
#rasterData<-stack('/scratch/crimmins/vhi/processed/VHI_1982-2018_LCNCA.grd')     # Vegetation Health Index

# get layer names and create date time series
fileString<-as.data.frame(t(as.data.frame(strsplit((names(rasterData)),"[.]"))))
  rownames(fileString) <- c()
  colnames(fileString)<-c("prefix","res","sat","comPeriod","yearWeek","prod","type")
  fileString$year<-as.numeric(substr(fileString$yearWeek, 2,5))   
  fileString$week<-as.numeric(substr(fileString$yearWeek, 6,8))
  fileString$date<-as.Date(paste(fileString$year, fileString$week, 1, sep="-"), "%Y-%U-%u")

## EXAMPLE: create time series of mean of each layer
  meanTimeSeries<-as.data.frame(cellStats(rasterData, mean))
    colnames(meanTimeSeries)<-c("meanValue")
    meanTimeSeries$date<-fileString$date
    
## EXAMPLE: Extract time series for one pixel based on lat/lon in grid extent for LCNCA
    lat<-31.812430 
    lon<--110.589071
    
  pixelTimeSeries<-as.data.frame(t(raster::extract(rasterData, cellFromXY(rasterData, c(lon,lat)))))
    colnames(pixelTimeSeries)<-c("pixelValue")
    pixelTimeSeries$date<-fileString$date
    
    
## EXAMPLE: Plot a layer on a leaflet map    
    layer<-1
    
    pal <- colorNumeric(c("goldenrod4", "lightgoldenrod1", "springgreen4"), values(rasterData),
                        na.color = "transparent")
    
    leaflet() %>% addTiles() %>%
      addRasterImage(rasterData[[layer]], colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(rasterData),
                title = "NDVI or VHI")
    
    
    
    