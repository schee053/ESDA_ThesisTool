# Data needs to contain: 
# sum of kWh per minute, 
# for the number of minutes charged, 
# within that hour of that day
# at that charge point.

#-------------------------------------------------------------------------------------------  
# pre-processing (Essent test) --> Later on, add to preProcessing script
#-------------------------------------------------------------------------------------------
# Continue from TestScript_phase1
EssentRaw02 <- EssentClean

length(unique(EssentRaw02$pointID))
length(unique(EssentRaw02$weekID))

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

