######################################################
# Purpose: Creates ForC_simplified as described here: https://github.com/forc-db/ForC/tree/master/ForC_simplified
# Inputs:
# - MEASUREMENTS table
# - SITES table
# - PLOTS table
# - R code that resolves duplicate records "scripts/Database_manipulation/Reconcile_duplicated_records.R"
# - VARIABLES table (to know what variables are secondary and remove those)
# Outputs:
# - ForC_simplified table
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
VARIABLES <- read.csv("data/ForC_variables.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####

## MEASUREMENTS ####

### RESOLVE DUPLICATES #####

source("scripts/Database_manipulation/Reconcile_duplicated_records.R")


### Ignore secondary variables ####
secondary.variables <- VARIABLES[VARIABLES$variable.type %in% "secondary",]$variable.name
MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[!MEASUREMENTS_no_duplicates$variable.name %in% secondary.variables, ]

### Ignore NEE_cum_C, GPP_cum_C, and R_eco_cum_C ignored.####
MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[!MEASUREMENTS_no_duplicates$variable.name %in% c("NEE_cum_C", "GPP_cum_C", "R_eco_cum_C"), ]


### Re-organize table ####
measurements.columns.to.keep <- c("measurement.ID", "sites.sitename", "plot.name", "stand.age", 
                                  "dominant.life.form", "dominant.veg", "variable.name", "date", 
                                  "start.date", "end.date", "mean", "min.dbh", "citation.ID")
names(MEASUREMENTS_no_duplicates)

MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[, measurements.columns.to.keep]
str(MEASUREMENTS_no_duplicates)

### Convert all measurements to units of C (use IPCC default C=0.47*biomass) + rename variables ####
units <- sapply(strsplit(MEASUREMENTS_no_duplicates$variable.name, "_"), tail, 1)

MEASUREMENTS_no_duplicates$mean <- ifelse(units %in% "OM", 0.47*MEASUREMENTS_no_duplicates$mean, MEASUREMENTS_no_duplicates$mean)
MEASUREMENTS_no_duplicates$variable.name <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS_no_duplicates$variable, perl = T)



## SITES ####

sites.columns.to.keep <- c("sites.sitename", "country", "lat", "lon", "masl", "mat", "map", 
                           "geographic.area", "biogeog", "Koeppen", "FAO.ecozone")
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

plots.columns.to.keep <- c("sites.sitename", "plot.name", "plot.area", "year.establishment.oldest.trees", 
                           "regrowth.hist.type", "regrowth.year", "dist.mrs.hist.type", 
                           "dist.mrs.yr")
names(PLOTS)

PLOTS <- PLOTS[, plots.columns.to.keep]
str(PLOTS)



# MERGE ALL TABLES ####
MEASUREMENTS_SITES <- merge(MEASUREMENTS_no_duplicates, SITES, by = "sites.sitename", all.x = T)
MEASUREMENTS_SITES_PLOTS <- merge(MEASUREMENTS_SITES, PLOTS, by = c("sites.sitename", "plot.name"), all.x = T)

# Order columns ####
ordered.field <- c("measurement.ID", "sites.sitename", "plot.name", 
                  "stand.age", "dominant.life.form", "dominant.veg", "variable.name", 
                  "date", "start.date", "end.date", "mean", "min.dbh", "citation.ID",
                  "country", "lat", "lon", "masl", "mat", "map", "geographic.area", 
                  "biogeog", "Koeppen", "FAO.ecozone",
                  "plot.area", "year.establishment.oldest.trees", 
                  "regrowth.hist.type", "regrowth.year", "dist.mrs.hist.type", 
                  "dist.mrs.yr")

MEASUREMENTS_SITES_PLOTS <- MEASUREMENTS_SITES_PLOTS[, ordered.field]


# Save ForC-simplified ####

write.csv(MEASUREMENTS_SITES_PLOTS, file = "ForC_simplified/ForC_simplified.csv", row.names = F)

