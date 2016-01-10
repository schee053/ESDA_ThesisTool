# Purpose        : Create kml file visualization from spacetime object;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 06-01-2015
# Note           : Make sure "preProcessing_rasterESDA.R" script has run, before starting this script.


# Set directory
mainDir <- "M:/My Documents/ESDA_ThesisTool/"
outputDir <- "Output"
dir.create(file.path(mainDir, outputDir), showWarnings = FALSE)
setwd(file.path(mainDir,outputDir))
getwd()
list.files()

# Download and open required packages
if (!require(plotKML)) install.packages('plotKML')
if (!require(spacetime)) install.packages('spacetime')
if (!require(plyr)) install.packages('plyr')
if (!require(rgdal)) install.packages('rgdal')
if (!require(sp)) install.packages('sp')
if (!require(RColorBrewer)) install.packages('RColorBrewer')

#-------------------------------------------------------------------------------------------  
# Questions about plotKML:
#-------------------------------------------------------------------------------------------

# Is there a way to make a bubble map in plotKML?
# How to slow down time?

# What arguments can be called? --> ChargeSession_KML <- function (ST_object, shape, kml.name, labels="", kmz=TRUE, legend=TRUE, balloon = TRUE){
# labels = "" or points_names="" ? 

#-------------------------------------------------------------------------------------------  
# Create KML vector layer (from CSV-file or object), kWh per minute as colors
#-------------------------------------------------------------------------------------------
# For shapes: https://sites.google.com/site/gmapsdevelopment/
# For colors: http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
# Legend position: choose from --> "UL","ML","LL","BC","LR","MR","UR","TC"
# Extrude specifies whether to connect the point to the ground with a line

ChargeSession_KML <- function (CSV_obj, shape, name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  kWhPal <- brewer.pal(9, "RdYlGn")
  shape <- shape
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$kWh_per_min,legend.pal=brewer.pal(9, "RdYlGn"), legend.file = "kWh_per_min.png")
  kml_screen(image.file = "kWh_per_min.png", position = "UL", sname = "kWh_per_min")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_per_min, colour_scale=brewer.pal(9, "RdYlGn"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

ChargeSession_KML(Week.02.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.02.2013")
ChargeSession_KML(Week.24.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.24.2013")
# for (i in 1:length(WeekList)) {
#   ChargeSession_KML(i, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", WeekList[i])
# }

#-------------------------------------------------------------------------------------------  
# Create KML vector layer from CSV-file, with weekday as colors
#-------------------------------------------------------------------------------------------

CS_Weekdays <- function (CSV_obj, shape, name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  weekpal <- c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B")
  shape <- shape
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$Weekday,legend.pal=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), legend.file = "Weekdays.png")
  kml_screen(image.file = "Weekdays.png", position = "ML", sname = "Weekdays")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=Weekday, colour_scale=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

CS_Weekdays(Week.02.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.02.Days")
CS_Weekdays(Week.24.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.02.Days")
#-------------------------------------------------------------------------------------------  
# Create KML vector layer of Charge Point locations in 2015
#-------------------------------------------------------------------------------------------
kml_stations <- function (csv.name, shape, kml.name, legend=TRUE, balloon = TRUE){
  obj <- read.csv(csv.name, header = T, sep=",")
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep=" ")
  obj <- obj[ !duplicated(obj["CSExternalID"]),]
  coordinates(obj) <- ~Longitude+Latitude
  proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
  # Remove unnecessary collomn
  obj_keep <- c("CPExternalID", "Street", "HouseNumber", "PostalCode", "City", "Provider", "VehicleType", "Address")
  obj <- obj[obj_keep] 
  statPal <- c("#FF1493", "#FFFF00")
  shape <- shape
  kml_open(kml.name)
  kml_legend.bar(obj$Provider, legend.pal= c("#FF1493", "#FFFF00"), legend.file = "Providers.png") 
  kml_screen(image.file = "Providers.png", position = "UL", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =.5, colour=Provider, colour_scale=c("#FF1493", "#FFFF00"), points_names="", balloon=TRUE, legend=TRUE)
  kml_close(kml.name)
  kml_View(kml.name)
}

kml_stations("ChargeStations.csv", "M:/My Documents/ESDA_ThesisTool/icons/Station1.png", "Stations2015.kml")
#------------------------------------------------------------------------------------------- 
# Create KML vector layer of Charge Point locations in 2013 (UNFINISHED)
#-------------------------------------------------------------------------------------------
NuonClean01$LonLat <- paste(NuonClean01$Longitude, NuonClean01$Latitude)
EssentClean06$LonLat < paste(EssentClean06$Longitude, EssentClean01$Latitude)
Stations$LonLat < paste(Stations$Longitude, Stations$Latitude) ## ERROR: logical(0)

NuonXYunique <- NuonClean01[ !duplicated(NuonClean01["LonLat"]),]
EssentXYunique <- EssentClean06[ !duplicated(EssentClean06["LonLat"]),]
XYunique <- rbind(NuonXYunique, EssentXYunique)

Stations2013 <- join(XYunique, Stations, by = LonLat , type = "left", match = "first")

#-------------------------------------------------------------------------------------------  
# Create STIDF (space-time irregular data frame) object from CSV-file
#-------------------------------------------------------------------------------------------

ST_DF <- function (obj){
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep="_")
  CP_obj <- SpatialPoints(obj[,c("Longitude","Latitude")])
  proj4string(CP_obj) <- CRS("+proj=longlat +datum=WGS84")
  obj$Begin_CS <- as.POSIXct(paste(obj$Begin_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  obj$End_CS <- as.POSIXct(paste(obj$End_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  CP_obj.st <- STIDF(CP_obj, time=obj$Begin_CS, data=obj[,c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], endTime=obj$End_CS)
  return (CP_obj.st)
} 

# ST_AdamJanuary2013 <- ST_DF(AdamJanuary2013)
# ST_AdamJune2013 <- ST_DF(AdamJune2013)

# You can now use plotKML or KML function to plot STIDF object
# plotKML(ST_AdamJanuary2013)
