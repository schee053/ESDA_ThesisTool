# Data needs to contain: 
# sum of kWh per minute, 
# for the number of minutes charged, 
# within that hour of that day
# at that charge point.

getwd()# Set directory
mainDir <- "M:/My Documents/ESDA_ThesisTool/"
outputDir <- "Output"
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir))
list.files()

# Download and open required packages
if (!require(plotKML)) install.packages('plotKML')
if (!require(spacetime)) install.packages('spacetime')
if (!require(plyr)) install.packages('plyr')
if (!require(rgdal)) install.packages('rgdal')
if (!require(sp)) install.packages('sp')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(plyr)) install.packages('plyr')
if (!require(RCurl)) install.packages('RCurl')
if (!require(chron)) install.packages('chron')
if (!require(lubridate)) install.packages('lubridate')
#-------------------------------------------------------------------------------------------  
# pre-processing (Essent test) --> later on add to preProcessing script
#-------------------------------------------------------------------------------------------
# Continue from preProcessing script
AdamSubset <- AdamJanuary2013[1:100,]
# Check number of unique charge points
length(unique(AdamSubset$pointID))
str(AdamSubset)

#Testing lubridate package
if (!require(lubridate)) install.packages('lubridate')
str(AdamSubset)
# Set connectionTime string to time
hms(AdamSubset$ConnectionTime)
# Create day of the year variable (always unique since data is from one year)
AdamSubset$YearDay <- yday(AdamSubset$Begin_CS)
# Extract begin time
AdamSubset$beginTime <- paste(hour(AdamSubset$Begin_CS), minute(AdamSubset$Begin_CS), second(AdamSubset$Begin_CS), sep = ":")
# Create session interval 
AdamSubset$sessionInterval <- interval((AdamSubset$Begin_CS), (AdamSubset$End_CS))
# Create hour interval (make a loop!)
hourBeginTime <- ymd_hms("20130101_000000")
hourBeginTime + days(0:30)
hourEndTime <- ymd_hms("20130101_005959")
hourEndTime + days(0:30)
hourBeginTime 
hourEndTime
AdamSubset$hourInterval <- interval(hourBeginTime, hourEndTime, tz = 'GMT')

# Try to create unique day+time combinations (1464 rows)
# df <- data.frame(matrix(ncol=1500, nrow=31))
# beginDate <- ymd("20130101")
# hourBeginTime <- hms("00:00:00")
# dft$beginDate <- beginDate + days(0:30)
# df$hourBeginTime <- hourBeginTime + hours(0:23)
# endDate <- ymd("20130101")
# hourEndTime <- hms("00:59:59")
# df$endDate <- endDate + days(0:30)
# df$hourEndTime <- hourEndTime + hours(0:23)
# df$Days <- interval(beginDate, endDate)
# df$hourDay <- interval(hourBeginTime, hourEndTime, tz = 'GMT')
# for i in (length(Days)) {
#   DayTime <- Days + hourDay
}
  
  
# Shift time interval by one hour (assign new hour to new column!)
AdamSubset$hour00 <- AdamSubset$hourInterval
AdamSubset$hour01 <- int_shift(AdamSubset$hourInterval, duration(hours = +1))
AdamSubset$hour02 <- int_shift(AdamSubset$hourInterval, duration(hours = +2))
AdamSubset$hour03 <- int_shift(AdamSubset$hourInterval, duration(hours = +3))
AdamSubset$hour04 <- int_shift(AdamSubset$hourInterval, duration(hours = +4))
AdamSubset$hour05 <- int_shift(AdamSubset$hourInterval, duration(hours = +5))
AdamSubset$hour06 <- int_shift(AdamSubset$hourInterval, duration(hours = +6))
AdamSubset$hour07 <- int_shift(AdamSubset$hourInterval, duration(hours = +7))
AdamSubset$hour08 <- int_shift(AdamSubset$hourInterval, duration(hours = +8))
AdamSubset$hour09 <- int_shift(AdamSubset$hourInterval, duration(hours = +9))
AdamSubset$hour10 <- int_shift(AdamSubset$hourInterval, duration(hours = +10))
AdamSubset$hour12 <- int_shift(AdamSubset$hourInterval, duration(hours = +11))
AdamSubset$hour13 <- int_shift(AdamSubset$hourInterval, duration(hours = +12))
AdamSubset$hour14 <- int_shift(AdamSubset$hourInterval, duration(hours = +13))
AdamSubset$hour15 <- int_shift(AdamSubset$hourInterval, duration(hours = +14))
AdamSubset$hour16 <- int_shift(AdamSubset$hourInterval, duration(hours = +15))
AdamSubset$hour17 <- int_shift(AdamSubset$hourInterval, duration(hours = +16))
AdamSubset$hour18 <- int_shift(AdamSubset$hourInterval, duration(hours = +17))
AdamSubset$hour19 <- int_shift(AdamSubset$hourInterval, duration(hours = +18))
AdamSubset$hour20 <- int_shift(AdamSubset$hourInterval, duration(hours = +19))
AdamSubset$hour21 <- int_shift(AdamSubset$hourInterval, duration(hours = +20))
AdamSubset$hour22 <- int_shift(AdamSubset$hourInterval, duration(hours = +21))
AdamSubset$hour23 <- int_shift(AdamSubset$hourInterval, duration(hours = +22))
AdamSubset$hour24 <- int_shift(AdamSubset$hourInterval, duration(hours = +23))
# Check if they intersect
AdamSubset$Overlap <- int_overlaps(AdamSubset$sessionInterval, AdamSubset$hourInterval)
# Extract difference in time intervals (minutes of session interval, within hour interval)
dates1 <- AdamSubset[,42]
dates2 <- AdamSubset$sessionInterval[1]
as.duration(dates2)
as.period(dates2)
overlapTimeInterval <- setdiff(dates1, dates2)
# Get length of interval in minutes
int_length(overlapTimeInterval)/60

for (i in (length(AdamSubset))){
  if (int_overlaps(AdamSubset$sessionInterval, AdamSubset[,29]) == TRUE){
    AdamSubset$OverlapInt <- setdiff(AdamSubset[,29], AdamSubset$sessionInterval)
    AdamSubset$OverlapMin <- int_length(AdamSubset$Overlap)/60
    AdamSubset[,10] + 1 
  }
  else{
    AdamSubset[,10] + 1
  }
}



# check_overlaps <- function(intervals) {
#   results <- 
#     matrix(rep(NA, length(intervals)*length(na.omit(intervals))), 
#            nrow=length(intervals))
#   for (i in 1:length(intervals)) {
#     results[i, ] <- int_overlaps(intervals[i], na.omit(intervals))
#   }
#   results
# }
#-------------------------------------------------------------------------------------------  
# Function for the visualisation of hour totals:
#-------------------------------------------------------------------------------------------

CS_hourTotals <- function (CSV_obj, shape, name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  weekpal <- brewer.pal(9, "RdYlGn")
  shape <- shape
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$hourTotal,legend.pal=brewer.pal(9, "RdYlGn"), legend.file = "hourTotal.png")
  kml_screen(image.file = "hourTotal.png", position = "LM", sname = "hourTotal")
  kml_layer.SpatialPoints(obj.sp[c("hourTotal", "MinutesCharged", "kWh_per_min", "Weekday", "beginHour", "endHour", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), colour=hourTotal, colour_scale=brewer.pal(9, "RdYlGn"), shape=shape, 
                          altitudeMode="clampToGround", size=hourTotal, balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_compress(name)
  kml_View(name)
}

# CS_hourTotals(AdamAgg, "M:/My Documents/ESDA_ThesisTool/icons/Station1.png", "hourTotals.kml")


