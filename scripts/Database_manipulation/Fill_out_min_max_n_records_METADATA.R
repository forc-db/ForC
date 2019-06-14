######################################################
# Purpose: In the metadata tables, write code that computes the following for each column: (1) number of records that are not missing data (NA/NAC/NRA/NI), (2) min, if numeric, (3) max, if numeric, (4) mean, if numeric
# Inputs:  - ForC MEASUREMENTS table and metadata
#          - ForC PLOTS table and metadata
#          - ForC SITES table and metadata
#          - ForC SITES table and metadata
#          - ForC HISTORY table and metadata
#          - ForC PFT table and metadata
#          - ForC HISTTYPE table and metadata
#          - ForC VARIABLES table and metadata
#          - ForC METHODOLOGY table and metadata
#          - ForC ALLOMETRY table and metadata
# outputs: All metadata updated
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12-08)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load tables ####


na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999")  # various ways "NA" is encoded in ForC


MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
PLOTS        <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
SITES        <- read.csv("data/ForC_sites.csv", na = na_codes, stringsAsFactors = F)
HISTORY      <- read.csv("data/ForC_history.csv", stringsAsFactors = F)
PFT          <- read.csv("data/ForC_pft.csv", na = na_codes, stringsAsFactors = F)
HISTTYPE     <- read.csv("data/ForC_histtype.csv", stringsAsFactors = F)
VARIABLES    <- read.csv("data/ForC_variables.csv", na = na_codes, stringsAsFactors = F)
METHODOLOGY  <- read.csv("data/ForC_methodology.csv", na = na_codes, stringsAsFactors = F)
ALLOMETRY    <- read.csv("data/ForC_allometry.csv", na = na_codes, stringsAsFactors = F)


MEASUREMENTS_meta  <- read.csv("metadata/measurements_metadata.csv", stringsAsFactors = F)
PLOTS_meta         <- read.csv("metadata/plots_metadata.csv", stringsAsFactors = F)
SITES_meta         <- read.csv("metadata/sites_metadata.csv", stringsAsFactors = F)
HISTORY_meta       <- read.csv("metadata/history_metadata.csv", stringsAsFactors = F)
PFT_meta           <- read.csv("metadata/pft_metadata.csv", stringsAsFactors = F)
HISTTYPE_meta      <- read.csv("metadata/histtype_metadata.csv", stringsAsFactors = F)
VARIABLES_meta     <- read.csv("metadata/variables_metadata.csv", stringsAsFactors = F)
METHODOLOGY_meta   <- read.csv("metadata/methodology_metadata.csv", stringsAsFactors = F)
ALLOMETRY_meta     <- read.csv("metadata/allometry_metadata.csv", stringsAsFactors = F)


# CALCULATE n, max and min ####

for(Table in c("MEASUREMENTS", "PLOTS", "SITES", "HISTORY", "PFT", "HISTTYPE", "VARIABLES", "METHODOLOGY","ALLOMETRY")){
  
  print(Table)
  
  DF <- get(Table)
  DF_meta <- get(paste0(Table, "_meta"))
  
  
  head(DF_meta)
  
  DF_meta$n <- NA
  DF_meta$Min <- NA
  DF_meta$Max <- NA
  
  
  for(i in 1:nrow(DF_meta)){
    
    
    
    f <- DF_meta$Field[i]
    print(f)
    
    x <- DF[, f]
    x <- na.omit(ifelse(x %in% na_codes, NA, x))
    
    DF_meta$n[i] <- length(x)
    
    x <- na.omit(as.numeric(x))
    min.x <- round(min(x))
    max.x <- round(max(x))
    
    DF_meta$Min[i] <- ifelse(min.x %in% "Inf", "-", min.x)
    DF_meta$Max[i] <- ifelse(max.x %in% "-Inf", "-", max.x)
  }
  
  print(head(DF_meta))
  
  # readline("Press [enter] to work on next table")
  
  assign(paste0(Table, "_meta"), DF_meta)
}


# Fix a few issues

HISTORY_meta[HISTORY_meta$Field == "level", c("Min", "Max")] <- "-"
MEASUREMENTS_meta[MEASUREMENTS_meta$Field == "dup.code", c("Min", "Max")] <- "-"

MEASUREMENTS_meta$n[MEASUREMENTS_meta$Field %in% "measurement.ID"] <- length(MEASUREMENTS$measurement.ID) # because 999 is not counted
PLOTS_meta$n[PLOTS_meta$Field %in% "prior.history.ID"] <- length(PLOTS$prior.history.ID) # because 999 is not counted
SITES_meta$n[SITES_meta$Field %in% "site.ID"] <- length(SITES$site.ID) # because 999 is not counted


# Remove range column


for(Table in c("MEASUREMENTS", "PLOTS", "SITES", "HISTORY", "PFT", "HISTTYPE", "VARIABLES", "METHODOLOGY","ALLOMETRY")){
  
  print(Table)
  
  DF_meta <- get(paste0(Table, "_meta"))
  
  names(DF_meta)
  
  if (any(grepl("range", names(DF_meta), ignore.case = T))) {
    DF_meta <- DF_meta[, - grep("range", names(DF_meta), ignore.case = T)]
  }

  
  assign(paste0(Table, "_meta"), DF_meta)
}

# SAVE ####
# write.csv(MEASUREMENTS_meta, "metadata/measurements_metadata.csv", row.names = F)
# write.csv(PLOTS_meta, "metadata/plots_metadata.csv", row.names = F)
# write.csv(SITES_meta, "metadata/sites_metadata.csv", row.names = F)
# write.csv(HISTORY_meta, "metadata/history_metadata.csv", row.names = F)
# write.csv(PFT_meta, "metadata/pft_metadata.csv", row.names = F)
# write.csv(HISTTYPE_meta, "metadata/histtype_metadata.csv", row.names = F)
# write.csv(VARIABLES_meta, "metadata/variables_metadata.csv", row.names = F)
# write.csv(METHODOLOGY_meta, "metadata/methodology_metadata.csv", row.names = F)
# write.csv(ALLOMETRY_meta, "metadata/allometry_metadata.csv", row.names = F)
