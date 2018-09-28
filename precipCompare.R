# PRISM and Livneh Monthly precip
# MAC 09/26/18

library(raster)
library(dplyr)
library(ggplot2)

# set rasteroptions
rasterOptions(progress = 'text')

# get boundary
us<-getData('GADM', country='USA', level=2)
county<-subset(us,NAME_2=="Gila")

# load gridded data
precLivneh<-stack("/scratch/crimmins/livneh/processed/WESTmonthlyLivneh_prec_1915_2015.grd")
precPRISM<-stack("/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_prec_1895_2017.grd")

# extract
LivMonthly <- t(extract(precLivneh, county, fun=mean, df=TRUE))
PRISMMonthly<- t(extract(precPRISM, county, fun=mean, df=TRUE))

# dates
# create dataframe
LivMonthly <- LivMonthly[-1, ]
LivMonthly<-as.data.frame(LivMonthly)
LivMonthly$dates<-seq(as.Date("1915-01-01"), as.Date("2015-12-31"), by="month")

PRISMMonthly <- PRISMMonthly[-1, ]
PRISMMonthly<-as.data.frame(PRISMMonthly)
PRISMMonthly$dates<-seq(as.Date("1895-01-01"), as.Date("2017-12-31"), by="month")

# merge
bothMonthly <- merge(PRISMMonthly, LivMonthly, by= "dates")
# mm/day?
bothMonthly$LivMonthly_30<-bothMonthly$LivMonthly*30

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

test<-as.numeric(format(bothMonthly$dates, "%m"))
bothMonthly$monthDays<-numberOfDays(bothMonthly$dates)

#library(Hmisc)
#monthDays(bothMonthly$dates)

library(lubridate)
test<-days_in_month(bothMonthly$dates)