######################################################
# Purpose: create a ForC version for high school education as described here: https://github.com/forc-db/ForC/issues/99
# Developped by: Valentine Herrmann - HerrmannV@si.edu in July 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####
## Keep only GPP ###
ForC_HighSchool <- ForC_simplified[ForC_simplified$variable.name %in% "GPP", ]


## Keep only forest >= 100 years
age <- as.numeric(ForC_HighSchool$stand.age)
ForC_HighSchool <- ForC_HighSchool[!is.na(age) & age >= 100, ]

## keep only unmanaged, undisturbed ####
ForC_HighSchool <- ForC_HighSchool[ForC_HighSchool$managed %in% 0 & ForC_HighSchool$disturbed %in% 0, ]

# Please include the following fields:####
    # measurement.ID
    # mat (rename "mean annual temperature")
    # mean (remame "GPP") 

ForC_HighSchool <- ForC_HighSchool[, c("measurement.ID", "mat", "mean")]
names(ForC_HighSchool) <- c("measurement.ID", "mean_annual_temperature_F", "GPP")


# Convert to Fahrenheit ####
ForC_HighSchool$mean_annual_temperature_F <- (9/5) * ForC_HighSchool$mean_annual_temperature_F + 32

# Save ForC-simplified ####

write.csv(ForC_HighSchool, file = "educational resources/ForC_GPP_and_temperature.csv", row.names = F)

