# Prototype Study Site Climate Profile
# MAC 09/26/18

# NOTES 
# - Create profile with PRISM data as comparison
# - Don't use SPEI until corrected with monthly total precip
# - add local elevation ranges, area? 
# - table of extreme warm/cold, dry/wet years? annotate on plots
# - USDM figure for county
# - add dataset grids to map
# - create station based profile, best station in county?

# LOGAN CO creates probs, set CO and city of Sterling to get map straight, Fort Morgan for Morgan CO

library(raster)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(scales)
library(cowplot)


# set rasteroptions
rasterOptions(progress = 'text')
# ggmap key
source('~/RProjects/RainlogAPI/APIkey.R')

# labels 
countyName<-"Grant"

# get boundary
us<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)
county<-subset(us,NAME_2==countyName & NAME_1=="Nebraska")

# get ggmap
#where<-geocode(paste0(countyName," County"), source = "dsk")
#where<-geocode(paste0("Grant County, Nebraska"), source = "dsk")
where<-geocode(paste0("Hyannis, Nebraska"), source = "dsk")
map <- qmap(location = c(lon = where[1,1], lat = where[1,2]), zoom = 7,
                  color = "color")

p <- map +
  geom_polygon(data = fortify(county),
               aes(long, lat, group = group),
               fill = "orange", colour = "red", alpha = 0.2) +
  theme_bw()+
  labs(title=paste0(countyName," County - Climate Profile"))

# load raw gridded climate data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
tave<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tave_1915_2015.grd")
tmin<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmin_1915_2015.grd")
tmax<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_tmax_1915_2015.grd")

# drought indices
spi3<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI3_1915_2015.grd")
spi12<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPI12_1915_2015.grd")
# hargreaves
spei3<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI3harg_1915_2015.grd")
spei12<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI12harg_1915_2015.grd")
# thornthwaite
#spei3<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI3thorn_1915_2015.grd")
#spei12<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_SPEI12thorn_1915_2015.grd")

# extract time series
#pptMean <- t(extract(prec, county, fun=mean, df=TRUE, weights=TRUE, normalizeWeights=TRUE))
precMonthly <- (t(extract(prec, county, fun=mean, df=TRUE, na.rm=TRUE)))/25.4
tmeanMonthly <- ((t(extract(tave, county, fun=mean, df=TRUE, na.rm=TRUE)))*9/5)+32  # (10°C × 9/5) + 32
tminMonthly <- ((t(extract(tmin, county, fun=mean, df=TRUE, na.rm=TRUE)))*9/5)+32
tmaxMonthly <- ((t(extract(tmax, county, fun=mean, df=TRUE, na.rm=TRUE)))*9/5)+32
spi3Monthly <- t(extract(spi3, county, fun=mean, df=TRUE, na.rm=TRUE))
spi12Monthly <- t(extract(spi12, county, fun=mean, df=TRUE, na.rm=TRUE))
spei3Monthly <- t(extract(spei3, county, fun=mean, df=TRUE, na.rm=TRUE))
spei12Monthly <- t(extract(spei12, county, fun=mean, df=TRUE, na.rm=TRUE))


# create dataframe
monthlyClimate<-as.data.frame(cbind(tmaxMonthly,tminMonthly,tmeanMonthly,precMonthly, spi3Monthly, spi12Monthly,spei3Monthly, spei12Monthly))
  monthlyClimate <- monthlyClimate[-1, ]
monthlyClimate$dates<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
names(monthlyClimate)<-c("tmax","tmin","tave","prec","spi3","spi12","spei3","spei12","dates")
monthlyClimate$year<-as.numeric(format(monthlyClimate$dates, "%Y"))
monthlyClimate$month<-as.numeric(format(monthlyClimate$dates, "%m"))
monthlyClimate$precTotal<-monthlyClimate$prec*days_in_month(monthlyClimate$dates)

# Walter-Lieth Diagrams
# see https://www.benjaminbell.co.uk/2018/04/walter-and-lieth-climate-diagrams-in-r.html

# Dismo Biovars by year



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
  labs(title="Avg Monthly Precip and Temp")



# group to annual values
# get monthly summary stats
annualClimate<-monthlyClimate %>% group_by(year) %>% summarise(
                                                                  meanTemp=mean(tave),
                                                                  meanPrec=sum(precTotal),
                                                                  stdTemp=sd(tave),
                                                                  stdPrec=sd(prec))
# sort out whether or not mm/day or total mm for each month
# compare to PRISM
tempPlot<-ggplot(annualClimate, aes(x=year, y=meanTemp))+
  geom_bar(stat="identity", fill='red4')+
  scale_y_continuous(limits=c(min(annualClimate$meanTemp), max(annualClimate$meanTemp)),oob = rescale_none)+
  scale_x_continuous(expand = c(0, 0))+
  theme_bw()+
  labs(title="Average Annual Temperature")+
  ylab("deg F")
  
precPlot<-ggplot(annualClimate, aes(x=year, y=meanPrec))+
  geom_bar(stat="identity", fill='green4')+
  scale_y_continuous(limits=c(min(annualClimate$meanPrec), max(annualClimate$meanPrec)),oob = rescale_none)+
  scale_x_continuous(expand = c(0, 0))+
  theme_bw()+
  labs(title="Annual Total Precipitation")+
  ylab("inches")

# SPI plots
monthlyClimate$spi3col<-monthlyClimate$spi3 >=0
monthlyClimate$spi12col<-monthlyClimate$spi12 >=0
monthlyClimate$spei3col<-monthlyClimate$spei3 >=0
monthlyClimate$spei12col<-monthlyClimate$spei12 >=0

# set date range
startYear<-1995
endYear<-2015
startDate<-paste0(startYear,"-01-01")
endDate<-paste0(endYear,"-12-01")

# SPI3
spi3plot<-ggplot(monthlyClimate, aes(x=dates,y=spi3, fill=spi3col))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
  labs(x='month/year',y='SPI', title='Standardized Precipitation Index - 3 month')+
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c(startDate,endDate)), expand = c(0.01, 0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits=c(-2.5,2.5),oob = rescale_none)
# SPI12
spi12plot<-ggplot(monthlyClimate, aes(x=dates,y=spi12, fill=spi12col))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
  labs(x='month/year',y='SPI', title='Standardized Precipitation Index - 12 month')+
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c(startDate,endDate)), expand = c(0.01, 0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits=c(-2.5,2.5),oob = rescale_none)

# SPEI3
spei3plot<-ggplot(monthlyClimate, aes(x=dates,y=spei3, fill=spei3col))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
  labs(x='month/year',y='SPEI', title='Standardized Precipitation ET Index - 3 month')+
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c(startDate,endDate)), expand = c(0.01, 0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits=c(-2.5,2.5),oob = rescale_none)
# SPEI12
spei12plot<-ggplot(monthlyClimate, aes(x=dates,y=spei12, fill=spei12col))+
  geom_bar(stat = "identity", position = "identity")+
  scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
  labs(x='month/year',y='SPEI', title='Standardized Precipitation ET Index - 12 month')+
  scale_x_date(date_breaks = "1 year", 
               labels=date_format("%Y"),
               limits = as.Date(c(startDate,endDate)), expand = c(0.01, 0))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits=c(-2.5,2.5),oob = rescale_none)


# grid of climate plots
climPlots <- plot_grid(p, climoPlot, tempPlot, precPlot,
                      labels=NULL, ncol = 2)

save_plot("./climateProfiles/climPlots.png", climPlots,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)

# grid of SPI plots
spiPlots <- plot_grid(spi3plot, spi12plot, spei3plot, spei12plot,
                       labels=NULL, ncol = 2)

save_plot("./climateProfiles/spiPlots.png", spiPlots,
          ncol = 2, # we're saving a grid plot of 2 columns
          nrow = 2, # and 2 rows
          # each individual subplot should have an aspect ratio of 1.3
          base_aspect_ratio = 1.3
)



