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

# What arguments can be called? --> ChargeSession_KML <- function (ST_object, shape, kml.name, labels="", kmz=TRUE, legend=TRUE, balloon = TRUE){
# labels = "" or points_names="" ? 

#-------------------------------------------------------------------------------------------  
# Create KML vector layer (from CSV-file or object), kWh per minute as colors + altitude
#-------------------------------------------------------------------------------------------
# For shapes: https://sites.google.com/site/gmapsdevelopment/
# For colors: http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
# Legend position: choose from --> "UL","ML","LL","BC","LR","MR","UR","TC"
# Extrude specifies whether to connect the point to the ground with a line

CS_kWhPerMin <- function (CSV_obj, shape, name){
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
  kml_screen(image.file = "http://s19.postimg.org/9xu93rakj/k_Wh_per_min.png", position = "ML", sname = "kWh_per_min")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_per_min, colour_scale=brewer.pal(9, "RdYlGn"), shape=shape, 
                          labels="", LabelScale=0, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE, points_names=obj.sp$Address)
  kml_close(name)
  kml_compress(name)
  # kml_View(name)
}

CS_kWhPerMin(Week.02.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.02.Minutes")
CS_kWhPerMin(Week.24.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.24.Minutes")

# for (i in 1:length(WeekList)) {
#   ChargeSession_KML(i, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", WeekList[i])
# }

#---------------------------------------------------------------------------------------------------------------  
# Create KML vector layer (from CSV-file or object), with total kWh_total as colors and per_minute as alititude.
#---------------------------------------------------------------------------------------------------------------

CS_kWhTotal <- function (CSV_obj, shape, name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  kWhPal <- brewer.pal(9, "RdYlGn")
  shape <- shape
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$kWh_total,legend.pal=brewer.pal(9, "RdYlGn"), legend.file = "kWh_total.png")
  kml_screen(image.file = "http://s19.postimg.org/3y6i03ps3/k_Wh_total.png", position = "ML", sname = "kWh_total")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_total, colour_scale=brewer.pal(9, "RdYlGn"), shape=shape, 
                          labels="", LabelScale=0, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE, points_names=obj.sp$Address)
  kml_close(name)
  kml_compress(name)
  # kml_View(name)
}

CS_kWhTotal(Week.02.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.02.Totals")
CS_kWhTotal(Week.24.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.24.Totals")

#---------------------------------------------------------------------------------------------------------------------------  
# Create KML vector layer (from CSV-file or object), with weekday as colors, kWh_per_minute in height and kWh_total in size.
#---------------------------------------------------------------------------------------------------------------------------

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
  kml_screen(image.file = "http://s19.postimg.org/6gs70sbib/Weekdays.png", position = "ML", sname = "Weekdays")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=Weekday, colour_scale=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), shape=shape, 
                          labels="", LabelScale=0, altitudeMode="relativeToGround", size=kWh_total, balloon = TRUE, kmz=TRUE, legend=TRUE, points_names=obj.sp$Address)
  kml_close(name)
  kml_compress(name)
  # kml_View(name)
}

CS_Weekdays(Week.02.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.02.Days")
CS_Weekdays(Week.24.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.24.Days")

#---------------------------------------------------------------------------------------------------------------------------  
# Create KML vector layer (from CSV-file or object), with weekday as colors, kWh_total in height and kWh_per_min in size.
#---------------------------------------------------------------------------------------------------------------------------
# Don't use this function. It the high efficiency (big circle), is often low amount of total kWh. 
# High circles in the bottom of the pile cloud the visual.  
# Also, many sessions have same number of kWh charged, so are double on the same hight. Is messy. 

# CS_InvWeekdays <- function (CSV_obj, shape, name){
#   obj.sp <- CSV_obj
#   obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
#   obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
#   coordinates(obj.sp) <- ~ Longitude + Latitude
#   proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
#   weekpal <- c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B")
#   shape <- shape
#   name <- paste(name, "kml", sep = ".")
#   kml_open(name)
#   kml_legend.bar(obj.sp$Weekday,legend.pal=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), legend.file = "Weekdays.png")
#   kml_screen(image.file = "Weekdays.png", position = "ML", sname = "Weekdays")
#   kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
#                           extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
#                           TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_total*100, colour=Weekday, colour_scale=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), shape=shape, 
#                           labels="", LabelScale=0.5, altitudeMode="relativeToGround", size=kWh_per_min, balloon = TRUE, kmz=TRUE, legend=TRUE)
#   kml_close(name)
#   kml_compress(name)
#   kml_View(name)
# }
# 
CS_InvWeekdays(Week.02.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.02.DaysInv")
CS_InvWeekdays(Week.24.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.24.DaysInv")

CS_InvWeekdays <- function (CSV_obj, shape, name){
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
  kml_screen(image.file = "http://s19.postimg.org/6gs70sbib/Weekdays.png", position = "ML", sname = "Weekdays")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=0, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_total*100, colour=Weekday, colour_scale=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), shape=shape, 
                          labels="", LabelScale=0, altitudeMode="relativeToGround", size=kWh_per_min, balloon = TRUE, kmz=TRUE, legend=TRUE, points_names=obj.sp$Address)
  kml_close(name)
  kml_compress(name)
  kml_View(name)
}

CS_InvWeekdays(Week.02.2013, "http://s19.postimg.org/futj6u2bj/EVcar_1.png", "Week.02.Online")
CS_InvWeekdays(Week.24.2013, "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png", "Week.24.Online")
#-------------------------------------------------------------------------------------------  
# Create KML vector layer of charge point locations in 2015
#-------------------------------------------------------------------------------------------
kmlStations2015 <- function (csv.name, shape, name){
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
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj$Provider, legend.pal= c("#FF1493", "#FFFF00"), legend.file = "Providers.png") 
  kml_screen(image.file = "http://s19.postimg.org/5nazufg3j/Providers.png", position = "MR", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =0, colour=Provider, colour_scale=c("#FF1493", "#FFFF00"), points_names=obj$Address, balloon=TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_compress(name)
  # kml_View(kml.name)
}

kmlStations2015("ChargeStations.csv", "http://s19.postimg.org/gydnismyn/Station1.png", "Stations2015")

#------------------------------------------------------------------------------------------- 
# Create KML vector layer of charge point locations in 2013 
#-------------------------------------------------------------------------------------------
# Create one Amsterdam2013 sessions file
Adam2013 <- rbind(AdamJanuary2013, AdamJune2013)

# Aggregate the sessions to unique charge stations
StationAgg <- function(obj){
  AdamClean <- obj
  keep <- c("Latitude", "Longitude", "Address", "Provider")
  AdamClean <- AdamClean[keep]
  # Aggregate sessions to unique charge points
  AdamClean$Prov_nr <- gsub("Nuon", "01", AdamClean$Provider, fixed = TRUE)
  AdamClean$Prov_nr <- gsub("Essent", "02", AdamClean$Prov_nr, fixed = TRUE)
  AdamClean$Prov_nr <- as.numeric(AdamClean$Prov_nr)
  AdamAgg <- aggregate(x = AdamClean, by = list(AdamClean$Latitude, AdamClean$Longitude, AdamClean$Address), FUN = mean)
  keep <- c("Group.1", "Group.2", "Group.3", "Prov_nr")
  AdamAgg <- AdamAgg[keep]
  names(AdamAgg)[names(AdamAgg)=="Group.1"] <- "Latitude"
  names(AdamAgg)[names(AdamAgg)=="Group.2"] <- "Longitude"
  names(AdamAgg)[names(AdamAgg)=="Group.3"] <- "Address"
  names(AdamAgg)[names(AdamAgg)=="Prov_nr"] <- "Provider" 
  AdamAgg$Provider <- as.character(AdamAgg$Provider)
  AdamAgg$Provider <- gsub("1", "Nuon", AdamAgg$Provider, fixed = TRUE)
  AdamAgg$Provider <- gsub("2", "Essent", AdamAgg$Provider, fixed = TRUE)
  return(AdamAgg)
}

Stations2013 <- StationAgg(Adam2013)

# Function to write unique charge points 2013 to kml-file
kmlStations2013 <- function(obj, shape, name){ 
  obj <- AdamAgg
  coordinates(obj) <- ~Longitude+Latitude
  proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
  statPal <- c("#FF1493", "#FFFF00")
  shape <- shape
  name <- paste(name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj$Provider, legend.pal= c("#FF1493", "#FFFF00"), legend.file = "Providers.png") 
  kml_screen(image.file = "http://s19.postimg.org/5nazufg3j/Providers.png", position = "MR", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =0, colour=Provider, colour_scale=c("#FF1493", "#FFFF00"), points_names="", balloon=TRUE, kmz=TRUE, legend=TRUE, points_names=obj.sp$Address)
  kml_close(name)
  kml_compress(name)
  # kml_View(name)
} 

kmlStations2013(Stations2013,"http://s19.postimg.org/gydnismyn/Station1.png", "Stations2013")

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
