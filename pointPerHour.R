# Purpose        : Create actual charging per point, per (unique) day, per hour;
# Maintainer     : Daniel Scheerooren (daniel.scheerooren@wur.nl);
# Status         : In progress
# Last update    : 08-01-2015
# Note           : Make sure "preProcessing_rasterESDA.R" script has run, before starting this script.

# Data needs to contain: 
# sum of kWh per minute, 
# for the number of minutes charged, 
# within that hour of that day
# at that charge point.
