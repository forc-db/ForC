######################################################
# Purpose: Filling out the Min and Max of VARIABLES Table + add a column with number of records per variable
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



na_codes <- c("NI", "NRA", "NaN", "NAC", "999")


# Calculate Min and Max for each variable + add count of records for each

VARIABLES$Min
VARIABLES$Max
VARIABLES$n_records
VARIABLES$units

VARIABLES$units <- ifelse(VARIABLES$units %in% "", NA, VARIABLES$units) # Replace "" of units by NA

for(i in 1:nrow(VARIABLES)){
  
  
  v <- VARIABLES$variables.name[i]
  print(v)
  
  if(!VARIABLES$variables.type[i] %in% "covariates"){
    
    x <- MEASUREMENTS[MEASUREMENTS$variables.name %in% v, ]$mean
    x <- na.omit(ifelse(x %in% na_codes, NA, x))
 
    print(head(x))
    
    VARIABLES[i, "Min"] <- min(x)
    VARIABLES[i, "Max"] <- max(x)
    
    
    VARIABLES[i, "n_records"] <- sum(!is.na(x))
  }
  
  if(VARIABLES$variables.type[i] %in% "covariates"){
    
    x <- c(MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v, ]$coV_1.value, MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v, ]$coV_2.value)
    x <- na.omit(as.numeric(ifelse(x %in% na_codes, NA, x)))
    
   
    
    VARIABLES[i, "Min"] <- min(x)
    VARIABLES[i, "Max"] <- max(x)
    
    
    VARIABLES[i, "n_records"] <- sum(!is.na(x))
  }
 
}


VARIABLES[, c("variables.type", "variables.name", "units", "Min", "Max", "n_records")]

# REplace Inf by NA


VARIABLES$Min <- ifelse(VARIABLES$Min == "Inf", "-", VARIABLES$Min)
VARIABLES$Max <- ifelse(VARIABLES$Max == "-Inf", "-", VARIABLES$Max)

VARIABLES[, c("variables.type", "variables.name", "units", "Min", "Max", "n_records")]



# SAVE ####
write.csv(VARIABLES, "data/ForC_variables.csv", row.names = F)

  
