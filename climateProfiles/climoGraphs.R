# Climographs for focal counties
# MAC 07/13/20
# adapted from climateProfile.R

library(raster)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(scales)
library(cowplot)


# set rasteroptions
rasterOptions(progress = 'text')

# load raw gridded climate data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
tave<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tave_1915_2015.grd")

# get boundary
us<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)

# county list
countyName<-c("San Luis Obispo","Santa Barbara","Humboldt",
  "Garfield","Kane","Rich","Weld","Logan",
  "Morgan","Luna","Sierra","Grant","Hidalgo",
  "Gila","Cherry","Grant")

stateName<-c("California","California","Nevada","Utah",
  "Utah","Utah","Colorado","Colorado",
  "Colorado","New Mexico","New Mexico",
  "New Mexico","New Mexico","Arizona",
  "Nebraska","Nebraska")
  
for(i in 1:length(countyName)){

  # subset to county    
  county<-subset(us,NAME_2==countyName[i] & NAME_1==stateName[i])
  
  # extract time series
  #pptMean <- t(extract(prec, county, fun=mean, df=TRUE, weights=TRUE, normalizeWeights=TRUE))
  precMonthly <- (t(extract(prec, county, fun=mean, df=TRUE, na.rm=TRUE)))/25.4
  tmeanMonthly <- ((t(extract(tave, county, fun=mean, df=TRUE, na.rm=TRUE)))*9/5)+32  # (10°C × 9/5) + 32
  
  # create dataframe
  monthlyClimate<-as.data.frame(cbind(tmeanMonthly,precMonthly))
  monthlyClimate <- monthlyClimate[-1, ]
  monthlyClimate$dates<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
  names(monthlyClimate)<-c("tave","prec","dates")
  monthlyClimate$year<-as.numeric(format(monthlyClimate$dates, "%Y"))
  monthlyClimate$month<-as.numeric(format(monthlyClimate$dates, "%m"))
  monthlyClimate$precTotal<-monthlyClimate$prec*days_in_month(monthlyClimate$dates)
  
  # get monthly summary stats
  yrs<-max(monthlyClimate$year)-min(monthlyClimate$year)+1
  monthlySummary<-monthlyClimate %>% group_by(month) %>% summarise(
    meanTemp=mean(tave),
    meanPrec=mean(precTotal),
    stdTemp=sd(tave),
    stdPrec=sd(prec))
  
  # plot monthly climograph from https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html
  maxT<-max(monthlySummary$meanTemp)+1
  maxP<-max(monthlySummary$meanPrec)+1
  
  climoPlot<-ggplot() + 
    geom_bar(data=monthlySummary, aes(x = month, y = meanPrec* maxT / maxP), stat = "identity", colour = gray(0.5), fill = gray(0.5)) + 
    geom_line(data=monthlySummary, aes(x = month, y = meanTemp)) + 
    geom_point(data=monthlySummary, aes(x = month, y = meanTemp), size = 3, shape = 21, fill = "white") + 
    scale_x_continuous(name = "Month", breaks = seq(1,12,1),
                       labels = month.abb[monthlySummary$month]) + 
    scale_y_continuous(
      name = expression("Temperature ("~degree~"F)"), 
      sec.axis = sec_axis(~ . * maxP / maxT , name = "Precipitation (in)"))+ #, 
    #limits = c(0, 30)) + 
    theme_bw() + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    )+
    labs(title=paste0("Avg Monthly Precip and Temp - ",countyName[i]," County, ",stateName[i]))
  
  
  png(paste0("~/RProjects/LivnehDrought/climateProfiles/climoPlots/",countyName[i],"-",stateName[i],"_ClimoGraph.png"), width = 7, height = 5, units = "in", res = 300L)
  #png(paste0("./vhi/seasFigs/ClimTS.png"), width = 7, height = 5, units = "in", res = 300L)
  #grid.newpage()
  print(climoPlot, newpage = FALSE)
  dev.off()

}  