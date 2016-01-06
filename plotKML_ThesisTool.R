# Purpose        : Create kml file visualization from spacetime object;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 06-01-2015
# Note           : Make sure "preProcessing_rasterESDA.R" script has run, before starting this script.


# Set directory
mainDir <- "M:/ESDA_ThesisTool/"
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

# How to to make each session have its own color?
# How to slow down time?

# What arguments can be called? --> ChargeSession_KML <- function (ST_object, shape, kml.name, labels="", kmz=TRUE, legend=TRUE, balloon = TRUE){
# labels = "" or points_names="" ? 
# Colors for weekdays: kml_legend.bar(obj$Weekday, width=10, height=15 ,legend.pal=, legend.file = "kWh_per_min.png", factor.labels="Weekday")
# Set in kml_layer: kml_colour  kml_altitude kml_alpha kml_size
# add "size=" in kml_layer?
# add kml_description()? (two-column data.frame) kml_description(data.frame, caption="Exploratory Spatial Data Analysis", asText=TRUE).


#-------------------------------------------------------------------------------------------  
# Create KML vector layer from CSV-file
#-------------------------------------------------------------------------------------------
# For shapes: https://sites.google.com/site/gmapsdevelopment/
# FOr colors: http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/
# Extrude specifies whether to connect the point to the ground with a line

CS_NuonJanuary2013 <- read.csv("NuonJanuary2013_final.csv", header = T, sep=",")
CS_NuonJune2013 <- read.csv("NuonJune2013_final.csv", header = T, sep=",")
#CS_EssentJanuary2013 <- read.csv("EssentJanuary2013_final.csv", header = T, sep=",")
#CS_EssentJune2013 <- read.csv("EssentJune2013_final.csv", header = T, sep=",")

ChargeSession_KML <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$kWh_per_min,legend.pal=brewer.pal(9, "Greens"), legend.file = "kWh_per_min.png")
  kml_screen(image.file = "kWh_per_min.png", position = "UL", sname = "kWh_per_min")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_per_min, colour_scale=brewer.pal(9, "Greens"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

ChargeSession_KML(CS_NuonJanuary2013, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "NuonJanuary2013")
ChargeSession_KML(CS_NuonJune2013, "M:/ESDA_ThesisTool/icon54.png", "NuonJune2013")
# ChargeSession_KML(CS_EssentJanuary2013, "http://maps.google.com/mapfiles/kml/pal4/icon54.png", "EssentJanuary2013")
# ChargeSession_KML(CS_EssentJune2013, "http://maps.google.com/mapfiles/kml/pal4/icon54.png", "EssentJune2013")

#-------------------------------------------------------------------------------------------  
# Create KML vector layer from CSV-file, with weekday as colors
#-------------------------------------------------------------------------------------------

CS_Weekdays <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$Weekday,legend.pal=brewer.pal(7, "Set1"), legend.file = "Weekday.png")
  kml_screen(image.file = "Weekday", position = "UL", sname = "Weekday")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=Weekday, colour_scale=brewer.pal(7, "Set1"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

CS_Weekdays(CS_NuonJanuary2013, "http://maps.google.com/mapfiles/kml/pal2/icon18.png", "NuonJanWeekday2013")

#-------------------------------------------------------------------------------------------  
# Create KML vector layer of Charge Point locations in 2015
#-------------------------------------------------------------------------------------------
kml_stations <- function (csv.name, shape, kml.name, legend=TRUE, balloon = TRUE){
  obj <- read.csv(csv.name, header = T, sep=";")
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep=" ")
  obj <- obj[ !duplicated(obj["CSExternalID"]),]
  coordinates(obj) <- ~Longitude+Latitude
  proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
  # Remove unnecessary collomn
  obj_keep <- c("CPExternalID", "Street", "HouseNumber", "PostalCode", "City", "Provider", "VehicleType", "Address")
  obj <- obj[obj_keep] 
  
  shape <- shape
  kml_open(kml.name)
  kml_legend.bar(obj$Provider, legend.pal=SAGA_pal[[1]][c(1,20)], legend.file = "Providers.png") 
  kml_screen(image.file = "Providers.png", position = "UL", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =.5, colour=Provider, colour_scale=SAGA_pal[[1]], points_names="", balloon=TRUE)
  kml_close(kml.name)
  kml_View(kml.name)
}

kml_stations("ChargeStations.csv", "http://maps.google.com/mapfiles/kml/paddle/wht-blank.png", "Stations2015.kml", legend=TRUE, balloon=TRUE)
#------------------------------------------------------------------------------------------- 
# Create KML vector layer of Charge Point locations in 2013
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

CS_NuonJanuary2013 <- read.csv("NuonJanuary2013_final.csv", header = T, sep=",")
CS_NuonJune2013 <- read.csv("NuonJune2013_final.csv", header = T, sep=",")
#CS_EssentJanuary2013 <- read.csv("EssentJanuary2013_final.csv", header = T, sep=",")
#CS_EssentJune2013 <- read.csv("EssentJune2013_final.csv", header = T, sep=",")

ST_DF <- function (obj){
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep="_")
  CP_obj <- SpatialPoints(obj[,c("Longitude","Latitude")])
  proj4string(CP_obj) <- CRS("+proj=longlat +datum=WGS84")
  obj$Begin_CS <- as.POSIXct(paste(obj$Begin_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  obj$End_CS <- as.POSIXct(paste(obj$End_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  CP_obj.st <- STIDF(CP_obj, time=obj$Begin_CS, data=obj[,c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], endTime=obj$End_CS)
  return (CP_obj.st)
} 

plotKML(ST_NuonJanuary2013)
ST_NuonJanuary2013 <- ST_DF(CS_NuonJanuary2013)
ST_NuonJune2013 <- ST_DF(CS_NuonJune2013)
#CS_STEssentJanuary2013 <- ST_DF(CS_EssentJanuary2013)
#CS_STEssentJune2013 <- ST_DF(CS_EssentJune2013)

# You can now use plotKML or KML function to plot STIDF object
