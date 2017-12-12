######################################################
# Purpose: Filling out the min and max of VARIABLES Table + add a column with number of records per variable
# Inputs: - ForC VARIABLES table
#         - ForC MEASUREMENTS table
# outputs: updated ForC_variables.csvg
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12-08)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load tables ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
VARIABLES <- read.csv("data/ForC_variables.csv", stringsAsFactors = F)



NA.forms <- c("NI", "NRA", "NaN", "NAC", "999")


# Calculate min and max for each variable + add count of records for each

VARIABLES$min
VARIABLES$max
VARIABLES$n_records
VARIABLES$units

VARIABLES$units <- ifelse(VARIABLES$units %in% "", NA, VARIABLES$units) # Replace "" of units by NA

for(i in 1:nrow(VARIABLES)){
  
  
  v <- VARIABLES$variables.name[i]
  print(v)
  
  if(!VARIABLES$variables.type[i] %in% "covariates"){
    
    x <- MEASUREMENTS[MEASUREMENTS$variables.name %in% v, ]$mean
    x <- na.omit(ifelse(x %in% NA.forms, NA, x))
 
    print(head(x))
    
    VARIABLES[i, "min"] <- min(x)
    VARIABLES[i, "max"] <- max(x)
    
    
    VARIABLES[i, "n_records"] <- sum(!is.na(x))
  }
  
  if(VARIABLES$variables.type[i] %in% "covariates"){
    
    x <- c(MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v, ]$coV1_value, MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v, ]$coV2_value)
    x <- na.omit(ifelse(x %in% NA.forms, NA, x))
    
    print(head(x))
    
    VARIABLES[i, "min"] <- min(x)
    VARIABLES[i, "max"] <- max(x)
    
    
    VARIABLES[i, "n_records"] <- sum(!is.na(x))
  }
 
}


VARIABLES[, c("variables.type", "variables.name", "units", "min", "max", "n_records")]

# REplace Inf by NA


VARIABLES$min <- ifelse(VARIABLES$min == "Inf", NA, VARIABLES$min)
VARIABLES$max <- ifelse(VARIABLES$max == "-Inf", NA, VARIABLES$max)

VARIABLES[, c("variables.type", "variables.name", "units", "min", "max", "n_records")]



# SAVE ####
write.csv(VARIABLES, "data/ForC_variables.csv", row.names = F)
  