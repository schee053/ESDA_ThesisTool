

str(CS_NuonJanuary2013)

obj.sp <- CS_NuonJanuary2013
obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%d-%m-%Y %H:%M")
obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%d-%m-%Y %H:%M")
coordinates(obj.sp) <- ~ Longitude + Latitude
proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
name <- paste("NuonJanuary2013", "kml", sep = ".")
kml_open(name)
kml_legend.bar(obj.sp$kWh_per_min,legend.pal=brewer.pal(9, "Greens"), legend.file = "kWh_per_min.png")
kml_screen(image.file = "kWh_per_min.png", position = "UL", sname = "kWh_per_min")
kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
                        extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%S"), 
                        TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%S"), altitude=kWh_per_min*10, colour=log1p(kWh_per_min), colour_scale=brewer.pal(9, "Greens"), shape=shape, 
                        labels=Weekday, LabelScale=0.5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, legend=TRUE)
kml_close(name)
kml_View(name)





# kml_layer.SpatialPoints(obj.sp[c("kWh_per_min", "ConnectionTime", "kWh_total", "Weekday", "Begin_CS", "End_CS", "Address", "Provider")], subfolder.name="Output", 
#                         extrude=TRUE, z.scale=10, TimeSpan.begin=format(obj.sp$Begin_CS, "%Y-%m-%dT%H:%M:%S"), 
#                         TimeSpan.end=format(obj.sp$End_CS, "%Y-%m-%dT%H:%M:%S"), altitude=kWh_per_min*10, colour=log1p(kWh_per_min), colour_scale=brewer.pal(9, "Greens"), shape=shape, 
#                         labels="Weekday", LabelScale=5, altitudeMode="relativeToGround", balloon = TRUE, kmz=TRUE, lengend=TRUE)