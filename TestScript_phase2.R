#-------------------------------------------------------------------------------------------  
# plotKML subset_test
#-------------------------------------------------------------------------------------------
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
  kml_View(name)
}

ChargeSession_KML(test.plot, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "testjun062013")
ChargeSession_KML(Week.00.2013, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "Week.00.2013")



CS_Weekdays <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  color.pal <- c("#882E72","#1965B0","#4EB265","#F7EE55","#F1932D","#E8601C","#DC050C")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(file.name, "kml", sep = ".")
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
# plotKML test
#-------------------------------------------------------------------------------------------


str(obj.sp)

obj.sp <- CS_NuonJune2013
obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
obj.sp <- subset(obj.sp, Begin_CS > as.POSIXct("2013-01-01 00:00"))
obj.sp <- subset(obj.sp, Begin_CS < as.POSIXct("2013-01-07 00:00"))
# ERROR! We need to subset (data too big!)
coordinates(obj.sp) <- ~ Longitude + Latitude
proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
shape <- "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
name <- paste("NuonJune2013", "kml", sep = ".")
kml_open(name)
kml_legend.bar(obj.sp$kWh_per_min,legend.pal=brewer.pal(9, "YlOrRd"), legend.file = "kWh_per_min.png")
kml_screen(image.file = "kWh_per_min.png", position = "UL", sname = "kWh_per_min")
kml_layer.SpatialPoints(obj.sp, TimeSpan.begin=format(obj.sp$Tbegin, "%Y-%m-%dT%H:%M:%SZ"), TimeSpan.end=format(obj.sp$Tend, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_total*10, colour=log1p(kWh_total), colour_scale=brewer.pal(9, "YlOrRd"), shape=shape, labels="", altitudeMode="relativeToGround")
# kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
#                         extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%SZ"), 
#                         TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%SZ"), altitude=kWh_per_min*10000, colour=log1p(kWh_per_min), colour_scale=brewer.pal(9, "YlOrRd"), shape=shape, 
#                         labels="", LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
kml_close(name)
kml_View(name)





# kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
#                         extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%S"), 
#                         TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%S"), altitude=kWh_per_min*10, colour=log1p(kWh_per_min), colour_scale=brewer.pal(9, "Greens"), shape=shape, 
#                         labels="Weekday", LabelScale=5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, lengend=TRUE)