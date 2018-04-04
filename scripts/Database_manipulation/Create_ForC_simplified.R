######################################################
# Purpose: Creates ForC-simplified as described here: https://github.com/forc-db/ForC/tree/master/ForC_simplified
# Inputs:
# - SITES table
# - PLOTS table
# - MEASUREMENTS table
# - ForC_simplified_metadata.csv
# - R code that resolves duplicatd records "scripts/Database_manipulation/Reconcile_duplicated_records.R"
# - 
# outputs: - ForC-simplified table
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-29)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# set warnings as erros ####
options("warn" = 2)

# Load data ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
ForC_simplified_meta <- read.csv("ForC_simplified/ForC_simplified_metadata.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####

## SITES ####

sites.columns.to.keep <- c("sites.sitename", ForC_simplified_meta[ForC_simplified_meta$Source.Table == "SITES", "Field"])
names(SITES)

SITES <- SITES[, sites.columns.to.keep]
str(SITES)

### masl ####
SITES$masl <- ifelse(my_is.na(SITES$masl), NA, SITES$masl)
SITES$masl <- as.numeric(SITES$masl) ## If you get an error here, that means that there is range values that need to be averaged

### mat ####
SITES$mat <- ifelse(my_is.na(SITES$mat), NA, SITES$mat)
SITES$mat <- as.numeric(SITES$mat) ## If you get an error here, that means that there is range values that need to be averaged

### map ####
SITES$map <- ifelse(my_is.na(SITES$map), NA, SITES$map)
SITES$map <- as.numeric(SITES$map) ## If you get an error here, that means that there is range values that need to be averaged



## PLOTS ####

plots.columns.to.keep <- c("sites.sitename", "plot.name", ForC_simplified_meta[ForC_simplified_meta$Source.Table == "PLOTS", "Field"])
names(PLOTS)

PLOTS <- PLOTS[, plots.columns.to.keep]
str(PLOTS)


## MEASUREMENTS ####

### RESOLVE DUPLICATES #####

####~~~~~~~~~~ STILL NEEDS TO BE WRITTEN ~~~~~~~~~ ####

### formate ####
measurements.columns.to.keep <- ForC_simplified_meta[ForC_simplified_meta$Source.Table == "MEASUREMENTS", "Field"]
names(MEASUREMENTS)

MEASUREMENTS <- MEASUREMENTS[, measurements.columns.to.keep]
str(MEASUREMENTS)

### Convert all measurements to units of C (use IPCC default C=0.47*biomass) + rename variables ####
units <- sapply(strsplit(MEASUREMENTS$variable.name, "_"), tail, 1)

MEASUREMENTS$mean <- ifelse(units %in% "OM", 0.47*MEASUREMENTS$mean, MEASUREMENTS$mean)
MEASUREMENTS$variable.name <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS$variable, perl = T)


# MERGE ALL TABLES ####

SITES_PLOTS <- merge(SITES, PLOTS, by = "sites.sitename", all.y = T)
SITES_PLOTS_MEASUREMENTS <- merge(SITES_PLOTS, MEASUREMENTS, by = c("sites.sitename", "plot.name"))

# Order columns like in the metadata ####
SITES_PLOTS_MEASUREMENTS <- SITES_PLOTS_MEASUREMENTS[, ForC_simplified_meta$Field]


# Save ForC-simplified ####

write.csv(SITES_PLOTS_MEASUREMENTS, file = "ForC_simplified/ForC_simplified.csv", row.names = F)


# put back options to default
options(warn = 0)