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
length(unique(EssentClean$pointID))
length(unique(EssentClean$weekID))