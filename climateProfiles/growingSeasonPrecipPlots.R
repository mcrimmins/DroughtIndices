# make growing season precip plots for focal area counties
# lifted code from climateProfile.R
# MAC 04/29/2019


library(raster)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(scales)
library(cowplot)
library(grid)


# set rasteroptions
rasterOptions(progress = 'text')

# labels 
countyName<-"Grant"

# get boundary
us<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)
county<-subset(us,NAME_2==countyName & NAME_1=="Nebraska")

# growing season specifications
seasLength<-6
endMo<-9

# load raw gridded climate data
prec<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")

# extract time series
#pptMean <- t(extract(prec, county, fun=mean, df=TRUE, weights=TRUE, normalizeWeights=TRUE))
precMonthly <- (t(extract(prec, county, fun=mean, df=TRUE, na.rm=TRUE)))/25.4

# create dataframe
monthlyClimate<-as.data.frame(precMonthly)
monthlyClimate <- as.data.frame(monthlyClimate[-1, ])
monthlyClimate$dates<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")
names(monthlyClimate)<-c("prec","dates")
monthlyClimate$year<-as.numeric(format(monthlyClimate$dates, "%Y"))
monthlyClimate$month<-as.numeric(format(monthlyClimate$dates, "%m"))
monthlyClimate$precTotal<-monthlyClimate$prec*days_in_month(monthlyClimate$dates)

# get total precip over specified seasons
# http://joewheatley.net/wp-content/uploads/2011/10/spi_functions.txt
getPrecOnTimescale <- function(precipitation,k){
  # precipitation is a vector of monthly precipitation values
  # returns monthly precipation averaged over current month and prior k-1 months
  Nt <- length(precipitation)
  prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); sum(as.vector(precipitation[tm:t]))}))
  return(prec.k)
}

# get rolling seasonal precip totals
monthlyClimate$sumPrecip<-getPrecOnTimescale(monthlyClimate$precTotal,seasLength)
# subset to end month of season
seasClimate<-monthlyClimate[which(monthlyClimate$month==endMo),]

# make plot
pPrecip<-ggplot(seasClimate, aes(year, sumPrecip))+
  geom_bar(stat = "identity", fill="darkgreen")+
  geom_hline(yintercept=mean(seasClimate$sumPrecip, na.rm=TRUE), linetype="dashed", color = "gray")+
  ggtitle(paste0(seasLength,"-month Precip Ending in ",month.name[endMo]," (1915-2015): ",countyName," County"))+
  labs(x = "Year",y="precip (in)")

png(paste0("~/RProjects/LivnehDrought/climateProfiles/growSeasPrecip/",countyName,"_NE_growingSeasPrecip_",endMo,".png"), width = 7, height = 5, units = "in", res = 300L)
#png(paste0("./vhi/seasFigs/ClimTS.png"), width = 7, height = 5, units = "in", res = 300L)
#grid.newpage()
print(pPrecip, newpage = FALSE)
# add footer
## now add the text 
grid.text(paste0("Precip: http://ciresgroups.colorado.edu/livneh/data"),
          x=.85, y=.03, 
          gp = gpar(col=1, 
                    fontfamily="Arial", cex=0.4)) 
dev.off()
