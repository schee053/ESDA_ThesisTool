# Data needs to contain: 
# sum of kWh per minute, 
# for the number of minutes charged, 
# within that hour of that day
# at that charge point.

#-------------------------------------------------------------------------------------------  
# pre-processing (Essent test) --> Later on, add to preProcessing script
#-------------------------------------------------------------------------------------------
write.csv(Week.04.2013, file = "Week.04.2013.csv")
# Continue from TestScript_phase1
EssentRaw02 <- Week.04.2013
# Werkt niet, maar zou moeten werken:
# EssentRaw02 <- JanuaryWeekList[1]
# length(unique(EssentRaw02$pointID))
# length(unique(EssentRaw02$weekID))
# JanuaryWeekList[1]

#Testing lubridate package
if (!require(lubridate)) install.packages('lubridate')
str(EssentRaw02)
# !!! lubridate needs time as string!
  
# Set connectionTime string to time
hms(EssentRaw02$ConnectionTime)
# Create day of the year variable (always unique since data is from one year)
EssentRaw02$YearDay <- yday(EssentRaw02$Begin_CS)
# Extract begin time
EssentRaw02$beginTime <- paste(hour(EssentRaw02$Begin_CS), minute(EssentRaw02$Begin_CS), second(EssentRaw02$Begin_CS), sep = ":")
# Create session interval 
EssentRaw02$sessionInterval <- interval((EssentRaw02$Begin_CS), (EssentRaw02$End_CS))
# Create hour interval (make a loop!)
hourBeginTime <- "20130101_000000"
hourEndTime <- "20130101_005959"
EssentRaw02$hourInterval <- interval(ymd_hms(hourBeginTime), ymd_hms(hourEndTime), tz = 'GMT')
# Shift time interval by one hour (assign new hour to new column!)
int_shift(EssentRaw02$hourInterval, duration(hours = +1))
EssentRaw02$hour00 <- EssentRaw02$hourInterval
EssentRaw02$hour01 <- int_shift(EssentRaw02$hourInterval, duration(hours = +1))
EssentRaw02$hour02 <- int_shift(EssentRaw02$hourInterval, duration(hours = +2))
EssentRaw02$hour03 <- int_shift(EssentRaw02$hourInterval, duration(hours = +3))
EssentRaw02$hour04 <- int_shift(EssentRaw02$hourInterval, duration(hours = +4))
EssentRaw02$hour05 <- int_shift(EssentRaw02$hourInterval, duration(hours = +5))
EssentRaw02$hour06 <- int_shift(EssentRaw02$hourInterval, duration(hours = +6))
EssentRaw02$hour07 <- int_shift(EssentRaw02$hourInterval, duration(hours = +7))
EssentRaw02$hour08 <- int_shift(EssentRaw02$hourInterval, duration(hours = +8))
EssentRaw02$hour09 <- int_shift(EssentRaw02$hourInterval, duration(hours = +9))
EssentRaw02$hour10 <- int_shift(EssentRaw02$hourInterval, duration(hours = +10))
EssentRaw02$hour12 <- int_shift(EssentRaw02$hourInterval, duration(hours = +11))
EssentRaw02$hour13 <- int_shift(EssentRaw02$hourInterval, duration(hours = +12))
EssentRaw02$hour14 <- int_shift(EssentRaw02$hourInterval, duration(hours = +13))
EssentRaw02$hour15 <- int_shift(EssentRaw02$hourInterval, duration(hours = +14))
EssentRaw02$hour16 <- int_shift(EssentRaw02$hourInterval, duration(hours = +15))
EssentRaw02$hour17 <- int_shift(EssentRaw02$hourInterval, duration(hours = +16))
EssentRaw02$hour18 <- int_shift(EssentRaw02$hourInterval, duration(hours = +17))
EssentRaw02$hour19 <- int_shift(EssentRaw02$hourInterval, duration(hours = +18))
EssentRaw02$hour20 <- int_shift(EssentRaw02$hourInterval, duration(hours = +19))
EssentRaw02$hour21 <- int_shift(EssentRaw02$hourInterval, duration(hours = +20))
EssentRaw02$hour22 <- int_shift(EssentRaw02$hourInterval, duration(hours = +21))
EssentRaw02$hour23 <- int_shift(EssentRaw02$hourInterval, duration(hours = +22))
EssentRaw02$hour24 <- int_shift(EssentRaw02$hourInterval, duration(hours = +23))
# Check if they intersect
EssentRaw02$Overlap <- int_overlaps(EssentRaw02$sessionInterval, EssentRaw02$hourInterval)
# Get length of interval in minutes
EssentRaw02$Int.minutes <- (int_length(EssentRaw02$sessionInterval)/60)
# Extract difference in time intervals
dates2 <- EssentRaw02$sessionInterval[1]
hourBeginTime <- "20130129_140000"
hourEndTime <- "20130129_150000"
hourInterval <- interval(ymd_hms(hourBeginTime), ymd_hms(hourEndTime), tz = 'GMT')
hourInterval
dates2 
hourInterval %within% dates2
date3 <- setdiff(as.duration(EssentRaw02$sessionInterval) - as.duration(EssentRaw02$hourInterval))

int_diff(EssentRaw02$sessionInterval, EssentRaw02$hourInterval)

# try --> as.difftime(c("0:3:20", "11:23:15")) --> use only the times (can you do this for time intervals?)
# try %within%
# try %in% 
# http://www.r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
#-------------------------------------------------------------------------------------------  
# pre-processing (Essent test) --> Later on, add to preProcessing script
#-------------------------------------------------------------------------------------------
# In SPSS, magic (maak syntax)

getwd()
GeladenPerUur <- read.csv("GeladenPerUur.csv", header=TRUE, sep = ",")
str(GeladenPerUur)

CS_Bubbles <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  #names(obj.sp)[names(obj.sp)=="ï..Latitude"] <- "Latitude"
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS_first_1), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS_last), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  color.pal <- c("#882E72","#1965B0","#4EB265","#F7EE55","#F1932D","#E8601C","#DC050C")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(file.name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$kWh_per_min_sum,legend.pal=rainbow(7, s=1), legend.file = "kWh_per_uur.png")
  kml_screen(image.file = "kWh_per_uur.png", position = "UL", sname = "kWh per uur")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min_sum", "timeMin_sum", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, TimeSpan.begin=format(obj.sp$Begin_CS_first_1, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS_last, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_per_min_sum, colour_scale=rainbow(7, s=1), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

kml_GeladenPerUur <- CS_Bubbles(GeladenPerUur, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "GeladenPerUur_Test")

