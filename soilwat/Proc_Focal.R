SpatialMean<-function(Var,county){
	apply(Var[,,which(points_Focal$UniqueStCo==county)],c(1,2),mean, na.rm=T) ##average over all points in county	
}

dir<-'./soilwat/county'
load(file.path(dir,'Focal_Data.Rdata'))

FocalCT<-c( 'San Luis Obispo California', 'Santa Barbara California',
 'Humboldt Nevada',  'Rich Utah',  'Garfield Utah', 'Kane Utah',
 'Weld Colorado', 'Logan Colorado', 'Morgan Colorado', 'Luna New Mexico', 'Sierra New Mexico', 'Grant New Mexico', 'Hidalgo New Mexico', 'Gila Arizona',
 'Cherry Nebraska', 'Grant Nebraska')


###data is formatted as each point in a county, this loops averages over space###
Precip_SpatialMean<-Temp_SpatialMean<-SWPshallow_SpatialMean<-SWPdeep_SpatialMean<-array(NA,c(101,12,length(FocalCT)),dimnames = list(c(1915:2015),
                            c("Jan", "Feb", "Mar","Apr",'May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'),
                            FocalCT))
for (i in 1:length(FocalCT)){
	Precip_SpatialMean[,,i]<-SpatialMean(Precip_Focal,FocalCT[i])
	Temp_SpatialMean[,,i]<-SpatialMean(Temp_Focal,FocalCT[i])
	SWPshallow_SpatialMean[,,i]<-SpatialMean(SWPshallow_Focal,FocalCT[i])
	SWPdeep_SpatialMean[,,i]<-SpatialMean(SWPdeep_Focal,FocalCT[i])
}

# plot some soil water data
library(cowplot)
library(scales)
library(plyr)

countyName<-"Gila Arizona"

#flatData<-as.data.frame.table(SWPshallow_SpatialMean)
#flatData<-as.data.frame.table(SWPdeep_SpatialMean)
flatData<-as.data.frame.table(Precip_SpatialMean)


colnames(flatData)<-c("Year","Month","County","Value")
flatData$date<-as.Date(paste0(flatData$Month,"-01-",flatData$Year), format="%b-%d-%Y")

# time series plots
tempTS<-flatData[which(flatData$County==countyName),]
ggplot(tempTS, aes(date,Value))+
  geom_line()+
  scale_x_date(date_breaks = "2 year", 
               labels=date_format("%Y"),
               limits = as.Date(c('1981-01-01','2015-12-01')))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("VWC (%)")+
  ggtitle("Gila County Average Monthly Shallow Soil Moisture")
  
# seasonal plots
# get monthly percentiles
tempTS<-flatData[which(flatData$County==countyName),]

precQuant<- ddply(tempTS,.(Month),summarise,
                  q05 = quantile(Value,0.05,na.rm='TRUE'),
                  q25 = quantile(Value,0.25,na.rm='TRUE'),
                  q10 = quantile(Value,0.10,na.rm='TRUE'),
                  q50 = quantile(Value,0.50,na.rm='TRUE'),
                  q90 = quantile(Value,0.90,na.rm='TRUE'),
                  q75 = quantile(Value,0.75,na.rm='TRUE'),
                  q95 = quantile(Value,0.95,na.rm='TRUE'),
                  min = min(Value,na.rm='TRUE'),
                  max = max(Value,na.rm='TRUE'),
                  avg = mean(Value,na.rm='TRUE'))
precQuant$fakeDate <- as.Date(strptime(paste("2012", precQuant$Month, "1"), format="%Y %b %d"))

ggplot(precQuant, aes(x=fakeDate, y=avg)) +
  geom_line(size=.5) + 
  geom_ribbon(aes(ymin=min, ymax=max, x=fakeDate, fill = "min/max"), alpha = 0.7) +
  geom_ribbon(aes(ymin=q05, ymax=q95, x=fakeDate, fill = "5th-95th"), alpha = 0.7) +
  geom_ribbon(aes(ymin=q25, ymax=q75, x=fakeDate, fill = "25th-75th"), alpha = 0.7) +
  geom_ribbon(aes(ymin=q50, ymax=q50, x=fakeDate, fill = "Median"), alpha = 0.7) +
  scale_fill_manual(name='Percentiles', values=c("darkslategray1","darkslategray2","black","darkslategray3"))+
  geom_line(size=.5)+
  scale_x_date(date_labels = "%B")+
  ggtitle(paste0("Monthly Average Precip (1915-2015): ",countyName," County"))+
  labs(x = "Month",y="inches")
  #ylim(0.1,0.3)

