######################################################
# Purpose:Add the list of all Citation.IDs of studies reporting measurements at each site in the new field created in SITES (measurement.ref).
# Developped by: Valentine Herrmann - HerrmannV@si.edu in April 2018
# R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
MEASUREMENTS  <- read.csv("data/ForC_measurements.csv", stringsAsFactors =F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Find all measurements of each site and append "loaded_from' to SITE table if it is unique in the measurement table  ####

MEASUREMENTS_loaded_from_of_each_sites <- list() 

for(site in SITES$sites.sitename) {
  print(site)
  
  meas_Citation.IDs <- MEASUREMENTS$citation.ID[MEASUREMENTS$sites.sitename %in% site]
  
  SITES$measurement.refs[SITES$sites.sitename %in% site] <- paste(unique(meas_Citation.IDs), collapse = "; ")
  
  } 


write.csv(SITES, file = "data/ForC_sites.csv", row.names = F)

