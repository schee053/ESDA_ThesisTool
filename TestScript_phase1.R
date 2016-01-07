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
# pre-process Nuon (test)
#-------------------------------------------------------------------------------------------
  # Read csv files and create R-objects
  NuonRaw <- read.csv("NuonSplitJan2013.csv", header = T, sep=",")
  str(NuonRaw)
  NuonRaw$Begin_CS <- as.POSIXct(paste(NuonRaw$Begin_CS), format="%Y-%m-%d %H:%M:%S", tz = "GMT")
  NuonRaw$End_CS <- as.POSIXct(paste(NuonRaw$End_CS), format="%Y-%m-%d %H:%M:%S",  tz = "GMT")
  
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
# Merge providers per month
#-------------------------------------------------------------------------------------------
str(Nuon_January2013)
str(Essent_January2013)

Adam_january <- rbind(Nuon_January2013, Essent_January2013)

#-------------------------------------------------------------------------------------------  
# Create function that subsets data per week
#-------------------------------------------------------------------------------------------

test.week <- AdamJune2013[1:20,]
# Create week identifier based on week of the year (you want subset per week)
AdamJune2013$WeekNr <- strftime(AdamJune2013$Begin_CS, format = "%W")
AdamJune2013$Year <- strftime(AdamJune2013$Begin_CS, format = "%Y")
AdamJune2013$weekID <- paste(AdamJune2013$WeekNr, AdamJune2013$Year, sep = ".")
uniq <- unique(unlist(AdamJune2013$weekID))
for (i in 1:length(uniq)) {
  assign(paste("Week",uniq[i],sep="."), subset(AdamJune2013, weekID == uniq[i]))
}

SubWeek.Adam <- function(x){}

