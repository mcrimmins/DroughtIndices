# winter livestock stress index...get snow and gridmet data
# MAC 03/03/2020

library(raster)
library(ncdf4)
library(ggplot2)
library(dplyr)

# functions
wtr_yr <- function(dates, start_month=9) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

get_waterYearDay <- function(x) {
  
  year_day <- lubridate::yday(x)
  yrs_leap <- lubridate::leap_year(x)
  
  # October 1st (day 1 of water year) is day 274 (or 275 in leap year) of calendar year.
  # (274 + years_leap) == 275 for dates in leap year and 274 for dates not in leap year.
  # This provides a boolean selector for days before (false) / after (true) Oct 1.
  after_waterday <- year_day >= (274 + yrs_leap)
  
  # 273 (or 274 in leap year) days from January 1 to October 1
  # This gives us 1 for October 1st and corrects everything till December 31st.
  year_day[after_waterday] <- year_day[after_waterday] - (273 + yrs_leap[after_waterday])
  
  # 92 days from October 1 to January 1
  # This gives us 93 for January 1 and corrects everything till September 29th.
  year_day[!after_waterday] <- year_day[!after_waterday] + 92
  
  return(year_day)
}
#

# set rasteroptions
rasterOptions(progress = 'text')

# labels for county ----
countyName<-"Grant"
# get boundary
us<-getData('GADM', country='USA', level=2)
#county<-subset(us,NAME_2==countyName)
county<-subset(us,NAME_2==countyName & NAME_1=="Nebraska")
# ----

# get snow data ----
# get info about snow files
pathNCEP<-"/scratch/crimmins/snow"
filesNC <- list.files(pathNCEP,full.names = T) 
#ncin <- nc_open(filesNC[1])
#print(ncin)
#level <- ncvar_get(ncin, "DEPTH")

# extract time series of mean daily snow depth for county
datalist = list()
for(i in 1:length(filesNC)){
  temp<-brick(filesNC[i], var="DEPTH")
  tempDF <- t(extract(temp, county, fun=max, df=TRUE, na.rm=TRUE))
  tempDF <- as.data.frame(tempDF[-1,])
  datalist[[i]] <- tempDF
  print(filesNC[i])
}
snowTS = do.call(rbind, datalist)
colnames(snowTS)<-"snowDepth_mm"

# add dates
firstDate<-rownames(snowTS)[1]
snowTS$date<-seq(as.Date(rownames(snowTS)[1], format="X%Y.%m.%d"),
                 as.Date(rownames(snowTS)[nrow(snowTS)], format="X%Y.%m.%d"),1)
snowTS$year<-format(snowTS$date, "%Y")
snowTS$doy<-format(snowTS$date, "%j")
snowTS$wYr<-wtr_yr(snowTS$date)
snowTS$wdoy<-get_waterYearDay(snowTS$date)
#snowTS$snowDepth_mm[snowTS$snowDepth_mm == 0] <- NA
# perc
snowTS<- snowTS %>%
  group_by(doy) %>% 
  mutate(percrank=rank(snowDepth_mm)/n())

# ----

# get gridmet data ----
# tmin
pathGM<-"/scratch/crimmins/gridmet/update_Aug2019"
filesNC <- list.files(pathGM,full.names = T,pattern="^tmmn") 
# trim to shorter snow record which is 10/1/1981 to 9/30/2017 - 1981-2017
filesNC <- filesNC[3:39]
#ncin <- nc_open(filesNC[11])
#print(ncin)
# extract time series of mean daily snow depth for county
datalist = list()
for(i in 1:length(filesNC)){
  temp<-brick(filesNC[i])
  tempDF <- t(extract(temp, county, fun=mean, df=TRUE, na.rm=TRUE))-273.15
  tempDF <- as.data.frame(tempDF[-1,])
  datalist[[i]] <- tempDF
  print(filesNC[i])
}
tminTS = do.call(rbind, datalist)
tminTS <- as.data.frame(tminTS)
colnames(tminTS)<-"minT_C"
tminTS$date<-seq(as.Date("1981-01-01",format="%Y-%m-%d"),as.Date("2017-12-31",format="%Y-%m-%d"),1)
tminTS$doy<-format(tminTS$date, "%j")
# perc
tminTS<- tminTS %>%
  group_by(doy) %>% 
  mutate(percrank=rank(minT_C)/n())
# ----

# tmax ----
pathGM<-"/scratch/crimmins/gridmet/update_Aug2019"
filesNC <- list.files(pathGM,full.names = T,pattern="^tmmx") 
# trim to shorter snow record which is 10/1/1981 to 9/30/2017 - 1981-2017
filesNC <- filesNC[3:39]
#ncin <- nc_open(filesNC[11])
#print(ncin)
# extract time series of mean daily snow depth for county
datalist = list()
for(i in 1:length(filesNC)){
  temp<-brick(filesNC[i])
  tempDF <- t(extract(temp, county, fun=mean, df=TRUE, na.rm=TRUE))-273.15
  tempDF <- as.data.frame(tempDF[-1,])
  datalist[[i]] <- tempDF
  print(filesNC[i])
}
tmaxTS = do.call(rbind, datalist)
tmaxTS <- as.data.frame(tmaxTS)
colnames(tmaxTS)<-"maxT_C"
tmaxTS$date<-seq(as.Date("1981-01-01",format="%Y-%m-%d"),as.Date("2017-12-31",format="%Y-%m-%d"),1)
tmaxTS$doy<-format(tmaxTS$date, "%j")
# perc
tmaxTS<- tmaxTS %>%
  group_by(doy) %>% 
  mutate(percrank=rank(maxT_C)/n())
# ----

# wind speed ----
pathGM<-"/scratch/crimmins/gridmet/update_Aug2019"
filesNC <- list.files(pathGM,full.names = T,pattern="^vs") 
# trim to shorter snow record which is 10/1/1981 to 9/30/2017 - 1981-2017
filesNC <- filesNC[3:39]
#ncin <- nc_open(filesNC[11])
#print(ncin)
# extract time series of mean daily snow depth for county
datalist = list()
for(i in 1:length(filesNC)){
  temp<-brick(filesNC[i])
  tempDF <- t(extract(temp, county, fun=mean, df=TRUE, na.rm=TRUE))
  tempDF <- as.data.frame(tempDF[-1,])
  datalist[[i]] <- tempDF
  print(filesNC[i])
}
windTS = do.call(rbind, datalist)
windTS <- as.data.frame(windTS)
colnames(windTS)<-"wind_ms"
windTS$date<-seq(as.Date("1981-01-01",format="%Y-%m-%d"),as.Date("2017-12-31",format="%Y-%m-%d"),1)
windTS$doy<-format(windTS$date, "%j")
# perc
windTS<- windTS %>%
  group_by(doy) %>% 
  mutate(percrank=rank(wind_ms)/n())
# ----

#save(tmaxTS,tminTS,snowTS,windTS, file = "./winterLivestock/tsData.RData")

# join into snow table
allGridMet<-cbind.data.frame(tmaxTS$date, tmaxTS$percrank, tminTS$percrank, windTS$percrank)
allVars<-merge(snowTS, allGridMet, by.x="date", by.y="tmaxTS$date")
# combined index
allVars$winterSevIndex<-allVars$percrank+(1-allVars$`tminTS$percrank`)+allVars$`windTS$percrank`
allVars$isSnow<-ifelse(allVars$snowDepth_mm==0, NA, 1)
allVars$winterSevIndex_snow<-allVars$winterSevIndex*allVars$isSnow

# heat map of snow depth
ggplot(data = allVars, mapping = aes(x = as.numeric(wdoy),
                                    y = as.numeric(wYr),
                                    fill = winterSevIndex_snow)) +
  geom_tile()+
  scale_fill_gradient2(low="blue", 
                       mid="yellow",
                       high = "red",
                       na.value="transparent", midpoint = 1.5, limits=c(2.5,3))+
  xlim(1,243)+
  xlab("Oct 1-May 31 (doy)")+
  ylab("year")+
  theme_bw()+
  ggtitle("Grant County, NE - Oct-May Winter Severity Index 1982-2017 ")
