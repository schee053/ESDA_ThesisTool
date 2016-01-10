#-------------------------------------------------------------------------------------------  
# plotKML subset_test
#-------------------------------------------------------------------------------------------

#### http://plotkml.r-forge.r-project.org/plotKML.html

# subset per week
# insert subsets into chargeSession function
# ChargeSession function: doesn't make a kmz file (no legend!) + icon doesn't work.
test.plot <- Nuon_June2013[1:20,]

### CHECK THIS: http://stackoverflow.com/questions/14313115/google-kmllayer-not-displaying-custom-marker 
### CHECK THIS LINK (for displaying you file in Google Maps): https://www.google.com/maps/d/
# Zoeken in data --> alleen hoogte kan niet worden weergeven. 
# Misschien in html de kleur aanpassen?

ChargeSession_KML <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(file.name, "kml", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$kWh_per_min,legend.pal=brewer.pal(9, "RdYlGn"), legend.file = "kWh_per_min.png")
  kml_screen(image.file = "kWh_per_min.png", position = "UL", sname = "kWh_per_min")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=kWh_per_min, colour_scale=brewer.pal(9, "RdYlGn"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_compress(name)
  kml_View(name)
}

ChargeSession_KML(test.plot, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "testjun062013")
ChargeSession_KML(Week.00.2013, "http://maps.google.com/mapfiles/kml/pal4/icon54.png", "Week.00.2013")

Sys.getenv("R_ZIPCMD", "zip")

CS_Weekdays <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  color.pal <- c("#882E72","#1965B0","#4EB265","#F7EE55","#F1932D","#E8601C","#DC050C")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(file.name, "kmz", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$Weekday,legend.pal=rainbow(7, s=1), legend.file = "Weekday.png")
  kml_screen(image.file = "Weekday", position = "UL", sname = "Weekday")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=Weekday, colour_scale=rainbow(7, s=1), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)
}

CS_Weekdays(test.plot, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "weekTestJun2013")

library(RColorBrewer)
display.brewer.all()


#-------------------------------------------------------------------------------------------  
# plotKML size test
#-------------------------------------------------------------------------------------------
# https://r-forge.r-project.org/scm/viewvc.php/pkg/R/aesthetics.R?root=plotkml&view=diff&r1=435&r2=436

str(obj.sp)

CSV_obj <- Week.04.2013
shape <- "M:/My Documents/ESDA_ThesisTool/icons/EVcar(1).png"
name <- "Size.kml"
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  weekpal <- c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B")
  shape <- shape
  name <- paste(name, "kmz", sep = ".")
  kml_open(name)
  kml_legend.bar(obj.sp$Weekday,legend.pal=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), legend.file = "Weekdays.png")
  kml_screen(image.file = "Weekdays.png", position = "ML", sname = "Weekdays")
  kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                          extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
                          TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=Weekday, colour_scale=c("#FF7F24", "#FFD700", "#228B22", "#00BFFF", "#6A5ACD", "#EE82EE", "#EE3B3B"), shape=shape, 
                          labels="", LabelScale=0.5, altitudeMode="relativeToGround", size=kWh_total, balloon = TRUE, kmz=TRUE, legend=TRUE)
  kml_close(name)
  kml_View(name)





# kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
#                         extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%S"), 
#                         TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%S"), altitude=kWh_per_min*10, colour=log1p(kWh_per_min), colour_scale=brewer.pal(9, "Greens"), shape=shape, 
#                         labels="Weekday", LabelScale=5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, lengend=TRUE)

csv.name <- "ChargeStations.csv" 
shape <- "M:/My Documents/ESDA_ThesisTool/icons/battery.png"  
kml.name <- "Stations2015.kml"
  obj <- read.csv(csv.name, header = T, sep=",")
  obj$Address <- paste(obj$Street, obj$HouseNumber, sep=" ")
  obj <- obj[ !duplicated(obj["CSExternalID"]),]
  coordinates(obj) <- ~Longitude+Latitude
  proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
  # Remove unnecessary collomn
  obj_keep <- c("CPExternalID", "Street", "HouseNumber", "PostalCode", "City", "Provider", "VehicleType", "Address")
  obj <- obj[obj_keep] 
  palette <- c("#FF1493", "#FFFF00")
  shape <- shape
  kml_open(kml.name)
  kml_legend.bar(obj$Provider, legend.pal= palette, legend.file = "Providers.png") 
  kml_screen(image.file = "Providers.png", position = "UL", sname = "Providers")
  kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =.5, colour=Provider, colour_scale=palette, points_names="", balloon=TRUE, legend=TRUE)
  kml_close(kml.name)
  kml_View(kml.name)


kml_stations("ChargeStations.csv", "M:/My Documents/ESDA_ThesisTool/icons/battery.png", "Stations2015.kml")


#------------------------------------------------------------------------------------------- 
# Create KML vector layer of Charge Point locations in 2013 (UNFINISHED)
#-------------------------------------------------------------------------------------------
# Create one Amsterdam2013 sessions file
Adam2013 <- rbind(AdamJanuary2013, AdamJune2013)
keep <- c("Latitude", "Longitude", "Address", "Provider")
AdamClean <- Adam2013[keep]
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
# Plot unique charge points 2013 in kml-file
obj <- AdamAgg
coordinates(obj) <- ~Longitude+Latitude
proj4string(obj) <- CRS("+proj=longlat +datum=WGS84")
# Remove unnecessary collomn
obj_keep <- c("Provider", "Address")
obj <- obj[obj_keep] 
palette <- c("#FF1493", "#FFFF00")
shape <- "M:/My Documents/ESDA_ThesisTool/icons/Station1.png" 
kml.name <- "Stations2013.kml"
kml_open(kml.name)
kml_legend.bar(obj$Provider, legend.pal= palette, legend.file = "Providers.png") 
kml_screen(image.file = "Providers.png", position = "UL", sname = "Providers")
kml_layer(obj[c("Provider","Address")], shape = shape, LabelScale =.5, colour=Provider, colour_scale=palette, points_names="", balloon=TRUE, legend=TRUE)
kml_close(kml.name)
kml_View(kml.name)
