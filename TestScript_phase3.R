# Data needs to contain: 
# sum of kWh per minute, 
# for the number of minutes charged, 
# within that hour of that day
# at that charge point.

#-------------------------------------------------------------------------------------------  
# pre-processing (Essent test) --> Later on, add to preProcessing script
#-------------------------------------------------------------------------------------------
# Continue from TestScript_phase1
EssentRaw02 <- JanuaryWeekList[1]

length(unique(EssentRaw02$pointID))
length(unique(EssentRaw02$weekID))

JanuaryWeekList[1]
#Testing lubridate package
if (!require(lubridate)) install.packages('lubridate')
str(EssentRaw02)
# Set connectionTime string to time
hms(EssentRaw02$ConnectionTime)
# Create day of the year variable (always unique since data is from one year)
EssentRaw02$YearDay <- yday(EssentRaw02$Begin_CS)
# Create session interval
EssentRaw02$sessionBeginTime <- as.character(EssentRaw02$Begin_CS)
EssentRaw02$sessionEndTime <- as.character(EssentRaw02$End_CS)
EssentRaw02$sessionInterval <- interval(ymd_hms(EssentRaw02$sessionBeginTime), ymd_hms(EssentRaw02$sessionEndTime))
# Create hour interval (make a loop!)
EssentRaw02$hourBeginTime <- "20130101_000000"
EssentRaw02$hourEndTime <- "20130101_001059"
EssentRaw02$hourInterval <- interval(ymd_hms(EssentRaw02$hourBeginTime), ymd_hms(EssentRaw02$hourEndTime))
# Check if they intersect
EssentRaw02$Intersect <- EssentRaw02$sessionInterval %within% EssentRaw02$hourInterval
EssentRaw02$Intersect[1]


# EssentRaw02$checkStart <- int_start(EssentRaw02$sessionsInterval)
# EssentRaw02$checkEnd <- int_end(EssentRaw02$sessionInterval)

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

