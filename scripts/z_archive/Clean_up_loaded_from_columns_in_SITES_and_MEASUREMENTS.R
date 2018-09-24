######################################################
# Purpose: Fill in "loaded_from" column in SITES table, based on MEASUREMENTS table
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
  
  meas_loaded_from <- MEASUREMENTS$loaded.from[MEASUREMENTS$sites.sitename %in% site]
  site_loaded_from <- SITES$loaded.from[SITES$sites.sitename %in% site]
  
  if(length(unique(meas_loaded_from)) == 1) { # only one loaded_from in measurement table
    if(my_is.na(site_loaded_from)) {
      site_loaded_from <- unique(meas_loaded_from)
      MEASUREMENTS_loaded_from_of_each_sites[[site]] <- unique(meas_loaded_from)
      SITES$loaded.from[SITES$sites.sitename %in% site] <- unique(meas_loaded_from)
    }
    if(!my_is.na(site_loaded_from) & !my_is.na(unique(meas_loaded_from))) {
      if(!site_loaded_from %in%  unique(meas_loaded_from)) {
        print(site)
        readline(print(c(site_loaded_from, unique(meas_loaded_from))))
      }
    }
  } 
  
  # if(length(unique(meas_loaded_from)) > 1) { # more than loaded_from in measurement table
  #   warning("more than one loaded_from in measurement table")
  #   print(site_loaded_from)
  #   readline(print(unique(meas_loaded_from)))
  #     }
} # fine if only a hanffull (4) stops

write.csv(SITES, file = "data/ForC_sites.csv", row.names = F)

