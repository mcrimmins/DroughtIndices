# process Broxton snow data, create county snow summaries
# MAC 03/03/2020

library(raster)
library(ncdf4)
library(ggplot2)

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

# get info about snow files
pathNCEP<-"/scratch/crimmins/snow"
filesNC <- list.files(pathNCEP,full.names = T) 
ncin <- nc_open(filesNC[1])
print(ncin)
#level <- ncvar_get(ncin, "DEPTH")

# extract time series of mean daily snow depth for county
datalist = list()
for(i in 1:length(filesNC)){
  temp<-brick(filesNC[i], var="DEPTH")
  tempDF <- t(extract(temp, county, fun=mean, df=TRUE, na.rm=TRUE))/25.4
  tempDF <- as.data.frame(tempDF[-1,])
  datalist[[i]] <- tempDF
  print(i)
}
snowTS = do.call(rbind, datalist)
colnames(snowTS)<-"depth"
# add dates
firstDate<-rownames(snowTS)[1]
format(firstDate, "X%Y.%m.%d")
as.Date(rownames(snowTS)[1], format="X%Y.%m.%d")
snowTS$date<-seq(as.Date(rownames(snowTS)[1], format="X%Y.%m.%d"),
                 as.Date(rownames(snowTS)[nrow(snowTS)], format="X%Y.%m.%d"),1)
snowTS$year<-format(snowTS$date, "%Y")
snowTS$doy<-format(snowTS$date, "%j")
snowTS$wYr<-wtr_yr(snowTS$date)
snowTS$wdoy<-get_waterYearDay(snowTS$date)
snowTS$depth[snowTS$depth == 0] <- NA

# heat map of snow depth
ggplot(data = snowTS, mapping = aes(x = as.numeric(wdoy),
                                      y = as.numeric(wYr),
                                      fill = depth)) +
  geom_tile()+
  scale_fill_gradient2(low="lightblue2", 
                       mid="royalblue2",
                       high = "violetred2",
                       na.value="transparent")+
  xlim(1,243)+
  xlab("Oct 1-May 31 (doy)")+
  ylab("year")+
  theme_bw()+
  ggtitle("Grant County, NE - Oct-May Daily Snow Depth 1982-2017 ")



