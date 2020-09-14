######################################################
# Purpose: fill in dominant.veg when we know it from other records in the same plot
# Developped by: Valentine Herrmann - HerrmannV@si.edu in April 2018
# R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libaries ####



# Load data ####

MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# identify recods with NAC in dominant.veg
idx <- MEASUREMENTS$dominant.veg %in% "NAC"

# replace unknown dominant.veg by the one we know based on other records

for(s_p in unique(paste(MEASUREMENTS$sites.sitename[idx], MEASUREMENTS$plot.name[idx]))) {
  x <- MEASUREMENTS[paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name) %in% s_p, ]
  
  if(any(x$dominant.veg != "NAC") & # if there is a known dominant vegetation
     length(unique(x$dominant.veg)) == 2 & # if there is only 2 options (NAC and the knwon dominent vegetation)
     length(unique(x$stand.age)) == 1 # if there is only 1 age
     ) { 
  
    x$dominant.veg <- x$dominant.veg[which(x$dominant.veg != "NAC")[1]]
    MEASUREMENTS[paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name) %in% s_p, ] <- x
  }
}


sum(idx) - sum(MEASUREMENTS$dominant.veg %in% "NAC") # saves only this number of records
table(MEASUREMENTS$required.citations[idx & !(MEASUREMENTS$dominant.veg %in% "NAC")]) # this is how they are distributed in the different sources.


# save  ####
write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names = F)
