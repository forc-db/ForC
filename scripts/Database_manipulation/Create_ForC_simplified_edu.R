######################################################
# Purpose: create aa version of ForC_Simplified in the ForC edu resources that only includes mature, undisturbed, and unmanaged forests (https://github.com/forc-db/ForC/issues/99)
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

## Keep only mature forest (>= 100 years)
age <- as.numeric(ForC_simplified$stand.age)
ForC_HighSchool <- ForC_simplified[!is.na(age) & age >= 100, ]

## keep only unmanaged, undisturbed ####
ForC_HighSchool <- ForC_HighSchool[ForC_HighSchool$managed %in% 0 & ForC_HighSchool$disturbed %in% 0, ]


# remove columsn managed	disturbed	history.no.info
ForC_HighSchool <- ForC_HighSchool[, !names(ForC_HighSchool) %in% c("managed",	"disturbed",	"history.no.info")]

# Save ForC-simplified ####

write.csv(ForC_HighSchool, file = "educational resources/ForC_simplified_edu.csv", row.names = F)

