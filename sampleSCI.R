# sample code to process monthly precipitation data into standardized time series like SPI
# MAC 12/20/18
# 

library(raster)
library(SPEI)
library(SCI)
library(maptools)

# set rasteroptions
rasterOptions(progress = 'text')

# load raw gridded climate data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")

# load MLRA
mlra <- readShapePoly(paste0("/home/crimmins/RProjects/LivnehDrought/shapes/mlra/mlra_v42.shp"))
# extract time series for MLRA
mlraSub<-subset(mlra,MLRA_NAME=="Mojave Desert")
# extract USDM from grid
climData <- (extract(prec, mlraSub, df=TRUE))
  climData<-(colMeans(climData, na.rm=TRUE))
  climData<-climData[2:length(climData)]
  
# convert to SPI using SPEI package
  window=12
climTS <- ts(climData, end=c(2015,12), frequency=12)
  spi12<-spi(climTS, window)
# convert to SPI using SCI
spi.para <- fitSCI(climTS,first.mon=1,time.scale=window,distr="gamma",p0=TRUE)
spi12sci <- transformSCI(climTS,first.mon=1,obj=spi.para)  
# plot both
dates<-seq(as.Date("1915/1/1"), as.Date("2015/12/31"), "months")
  plot(dates,spi12sci,t="l")
  lines(dates,spi12$fitted, col="red")

# normality diagnostics of a select month
allSPI<-as.data.frame(cbind(dates, as.numeric(format(dates,"%m")),spi12$fitted, spi12sci, spi12$fitted-spi12sci))
colnames(allSPI)<-c("date","month","SPEI-spi","SCI-spi","diff")
# select month
monthSPI<-allSPI[which(allSPI$month==5),]
# diagnostics
  diagTable<-rbind(cbind(shapiro.test(monthSPI$`SPEI-spi`)$p.value, median(monthSPI$`SPEI-spi`,na.rm=TRUE)),
    cbind(shapiro.test(monthSPI$`SCI-spi`)$p.value, median(monthSPI$`SCI-spi`, na.rm=TRUE)))
    
