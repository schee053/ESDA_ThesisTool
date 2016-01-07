#-------------------------------------------------------------------------------------------  
# pre-process Essent (test)
#-------------------------------------------------------------------------------------------
  # Read CSV file
  EssentRaw01 <- read.csv("exp_201301-62014.csv",  header = T, sep=",")
  EssentRaw <- EssentRaw01
  # Extract the sum of sessions
  EssentRaw$IS_SUM_RECORD <- as.character(EssentRaw$IS_SUM_RECORD)
  EssentRaw <- subset(EssentRaw, IS_SUM_RECORD == "X")
  
  # Set date and time 
  EssentRaw$Begin_DA <- as.character(EssentRaw$BEGIN_LOAD_DATE)
  EssentRaw$Begin_TI <- as.character(EssentRaw$BEGIN_LOAD_TIME)
  EssentRaw$End_DA <- as.character(EssentRaw$END_LOAD_DATE)
  EssentRaw$End_TI <- as.character(EssentRaw$END_LOAD_TIME)
  EssentRaw$Begin_CS <- as.POSIXct(paste(EssentRaw$Begin_DA, EssentRaw$Begin_TI), format="%d.%m.%Y %H:%M:%S", tz = "GMT")
  EssentRaw$End_CS <- as.POSIXct(paste(EssentRaw$End_DA, EssentRaw$End_TI), format="%d.%m.%Y %H:%M:%S",  tz = "GMT")
  
  # Remove sessions from December 2012
  EssentRaw <- subset(EssentRaw, Begin_CS >= as.POSIXct("2013-01-01 00:00"))
  
  # Add weekdays column
  EssentRaw$Weekday <- weekdays(as.Date(EssentRaw$Begin_CS, "%Y-%m-%d %H:%M:%S", tz = "GMT"))
  
  # Convert energy from factor to numeric
  EssentRaw$ENERGIE <- as.character(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- gsub(",", "", EssentRaw$ENERGIE, fixed = TRUE)
  EssentRaw$ENERGIE <- as.numeric(EssentRaw$ENERGIE)
  EssentRaw$ENERGIE <- (EssentRaw$ENERGIE / 10000)
  
  # Rename columns: 
  names(EssentRaw)[names(EssentRaw)=="STREET"] <- "Street"
  names(EssentRaw)[names(EssentRaw)=="HOUSE_NUM1"] <- "HouseNumber"
  names(EssentRaw)[names(EssentRaw)=="POST_CODE1"] <- "PostalCode"
  names(EssentRaw)[names(EssentRaw)=="CHARGE_DURATION"] <- "ConnectionTime"
  names(EssentRaw)[names(EssentRaw)=="ENERGIE"] <- "kWh_total"
  names(EssentRaw)[names(EssentRaw)=="UNIQUE_ID"] <- "Session_ID"
  
  # Remove white space from PostalCode
  EssentRaw$PostalCode <- as.character(EssentRaw$PostalCode)
  EssentRaw$PostalCode <- gsub(" ", "", EssentRaw$PostalCode, fixed = T)
  
  # Add ConnectionTime in seconds
  EssentRaw$ConnectionTime <- as.character.Date(EssentRaw$ConnectionTime, format= "%H:%M:%S", tz="GMT")
  
  toSeconds <- function(x){
    if (!is.character(x)) stop("x must be a character string of the form H:M:S")
    if (length(x)<=0)return(x)
    
    unlist(
      lapply(x,
             function(i){
               i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
               if (length(i) == 3) 
                 i[1]*3600 + i[2]*60 + i[3]
               else if (length(i) == 2) 
                 i[1]*60 + i[2]
               else if (length(i) == 1) 
                 i[1]
             }  
      )  
    )  
  } 
  
  EssentRaw$timeSec <- toSeconds(EssentRaw$ConnectionTime)
  
  # Remove sessions of 0 seconds (failed sessions)
  EssentRaw <- subset(EssentRaw, timeSec != 0)
  
  # Calculate kWh per minute
  EssentRaw$kWh_per_min <- ((EssentRaw$kWh_total/EssentRaw$timeSec)*60) 
  EssentRaw$kWh_per_min <- round(EssentRaw$kWh_per_min,digits=3)
  
  # Join Charge data with xy-coordinates
  EssentRaw$Address <- paste(EssentRaw$Street, EssentRaw$HouseNumber, EssentRaw$PostalCode, sep=" ")
  EssentRaw.Stations <- join(EssentRaw, Stations, by="Address", type = "left", match = "all")
  
  # Remove duplicates in joined file 
  EssentRaw.Stations$REMOVE_ID <- paste(EssentRaw.Stations$Session_ID, EssentRaw.Stations$METER_READ_BEGIN, EssentRaw.Stations$Address)
  EssentRaw.Sessions <- EssentRaw.Stations[ !duplicated(EssentRaw.Stations["REMOVE_ID"]),]
  # Not the right combination of joins! --> find out where the duplicates come from! 
  
  # Remove NA values in Latitude column 
  EssentRaw.Sessions <- EssentRaw.Sessions[!is.na(EssentRaw.Sessions$Latitude),] 
  str(EssentRaw.Sessions)
  # Remove unnecessary columns
  keep <- c("Session_ID", "Begin_CS", "End_CS", "Weekday", "kWh_per_min", "ConnectionTime", "kWh_total", "Street", "HouseNumber", "PostalCode", "Address", "Latitude", "Longitude", "Provider")
  EssentClean <- EssentRaw.Sessions[keep]
  
  # Write to csv and return object
  write.csv(EssentClean, file = paste(obj.name, "csv", sep =".")) 
  return (EssentClean)
  


#-------------------------------------------------------------------------------------------  
# kWh per minute in R (Nuon. Essent also has seconds ":%S"!)
#-------------------------------------------------------------------------------------------
str(obj_kwh)

obj_kwh <- Nuon_June2013
obj_kwh$ConnectionTime <- paste(obj_kwh$ConnectionTime, "00", sep = ":")
obj_kwh$chr <- as.character.Date(obj_kwh$ConnectionTime, format= "%H:%M:%S", tz="GMT")

toSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
} 


obj_kwh$timeTest <- toSeconds(obj_kwh$chr)
str(obj_kwh)
#-------------------------------------------------------------------------------------------  
# Remove 0 values
#-------------------------------------------------------------------------------------------

obj3 <- subset(obj2, timeTest != 0)

#-------------------------------------------------------------------------------------------  
# calculate kwh per minute
#-------------------------------------------------------------------------------------------

str(obj3)
obj3$kWh_per_min <- ((obj3$kWh/obj3$timeTest)*60) 
obj3$kWh_per_min <- round(obj3$kWh_per_min,digits=3)

#-------------------------------------------------------------------------------------------  
# plotKML subset_test
#-------------------------------------------------------------------------------------------
# subset per week
# insert subsets into chargeSession function
# ChargeSession function: doesn't make a kmz file (no legend!) + icon doesn't work.
test.plot <- Nuon_June2013[1:20,]

ChargeSession_KML <- function (CSV_obj, shape, file.name){
  obj.sp <- CSV_obj
  obj.sp$Begin_CS <- as.POSIXct(paste(obj.sp$Begin_CS), format="%Y-%m-%d %H:%M:%S")
  obj.sp$End_CS <- as.POSIXct(paste(obj.sp$End_CS), format="%Y-%m-%d %H:%M:%S")
  coordinates(obj.sp) <- ~ Longitude + Latitude
  proj4string(obj.sp) <- CRS("+proj=longlat +datum=WGS84")
  shape <- "http://maps.google.com/mapfiles/kml/pal4/icon54.png"
  name <- paste(file.name, "kml", sep = ".")
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

ChargeSession_KML(test.plot, "http://maps.google.com/mapfiles/kml/pal4/icon18.png", "testjun062013")




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