######################################################
# Purpose: Creates ForC_simplified_metadata
# Inputs:
# - ForC_simplified table
# - MEASUREMENTS metadata
# - SITES metadata
# - PLOTS metadata
# Outputs:
# - ForC_simplified_metadata table and metadata tables
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)

MEASUREMENTS_meta <- read.csv("metadata/measurements_metadata.csv", stringsAsFactors = F)
SITES_meta <- read.csv("metadata/sites_metadata.csv", stringsAsFactors = F)
PLOTS_meta <- read.csv("metadata/plots_metadata.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare metadata ####
## MEASUREMENTS ####
### get the rows we need
MEASUREMENTS_ForC_simplified_meta <- MEASUREMENTS_meta[MEASUREMENTS_meta$Field %in% names(ForC_simplified),]

### change n, min and max values

DF <- ForC_simplified
DF_meta <- MEASUREMENTS_ForC_simplified_meta

DF_meta$n <- NA
DF_meta$Min <- NA
DF_meta$Max <- NA


for(i in 1:nrow(DF_meta)){
  
  
  
  f <- DF_meta$Field[i]
  print(f)
  
  x <- DF[, f]
  x <- my_na.omit(x)
  
  DF_meta$n[i] <- length(x)
  
  x <- my_na.omit(as.numeric(x))
  min.x <- round(min(x))
  max.x <- round(max(x))
  
  DF_meta$Min[i] <- ifelse(min.x %in% "Inf", "-", min.x)
  DF_meta$Max[i] <- ifelse(max.x %in% "-Inf", "-", max.x)
}


DF_meta[, c("Field", "n", "Min", "Max")]
MEASUREMENTS_ForC_simplified_meta[, c("Field", "n", "Min", "Max")]


### Add source.Table field
DF_meta$Source.Table <- "MEASUREMENTS"


MEASUREMENTS_ForC_simplified_meta <- DF_meta


## SITES ####

### get the rows we need
SITES_ForC_simplified_meta <- SITES_meta[SITES_meta$Field %in% names(ForC_simplified),]

### change n, min and max values

DF <- ForC_simplified
DF_meta <- SITES_ForC_simplified_meta

head(DF_meta)

DF_meta$n <- NA
DF_meta$Min <- NA
DF_meta$Max <- NA


for(i in 1:nrow(DF_meta)){
  
  
  
  f <- DF_meta$Field[i]
  print(f)
  
  x <- DF[, f]
  x <- my_na.omit(x)
  
  DF_meta$n[i] <- length(x)
  
  x <- my_na.omit(as.numeric(x))
  min.x <- round(min(x))
  max.x <- round(max(x))
  
  DF_meta$Min[i] <- ifelse(min.x %in% "Inf", "-", min.x)
  DF_meta$Max[i] <- ifelse(max.x %in% "-Inf", "-", max.x)
}


DF_meta[, c("Field", "n", "Min", "Max")]
SITES_ForC_simplified_meta[, c("Field", "n", "Min", "Max")]

### Add source.Table field
DF_meta$Source.Table <- "SITES"


SITES_ForC_simplified_meta <- DF_meta

## PLOTS ####
### get the rows we need
PLOTS_ForC_simplified_meta <- PLOTS_meta[PLOTS_meta$Field %in% names(ForC_simplified),]

### change n, min and max values

DF <- ForC_simplified
DF_meta <- PLOTS_ForC_simplified_meta

DF_meta$n <- NA
DF_meta$Min <- NA
DF_meta$Max <- NA


for(i in 1:nrow(DF_meta)){
  
  
  
  f <- DF_meta$Field[i]
  print(f)
  
  x <- DF[, f]
  x <- my_na.omit(x)
  
  DF_meta$n[i] <- length(x)
  
  x <- my_na.omit(as.numeric(x))
  min.x <- round(min(x))
  max.x <- round(max(x))
  
  DF_meta$Min[i] <- ifelse(min.x %in% "Inf", "-", min.x)
  DF_meta$Max[i] <- ifelse(max.x %in% "-Inf", "-", max.x)
}


DF_meta[, c("Field", "n", "Min", "Max")]
PLOTS_ForC_simplified_meta[, c("Field", "n", "Min", "Max")]


### Add source.Table field
DF_meta$Source.Table <- "PLOTS"

PLOTS_ForC_simplified_meta <- DF_meta

# MERGE ALL META TABLES + organize ####

## Merge
ForC_simplified_meta <- rbind(MEASUREMENTS_ForC_simplified_meta, SITES_ForC_simplified_meta, PLOTS_ForC_simplified_meta)

## Replace the units cell for mean with Mg C ha-1 (stocks) or Mg C ha-1 yr-1 (fluxes)."Mg C ha-1 (stocks) or Mg C ha-1 yr-1 (fluxes). Variables are identified as stocks of fluxes in the VARIABLES table. Records originally expressed in units of organic matter (OM) were converted to C assuming C =0.48*OM."

ForC_simplified_meta[ForC_simplified_meta$Field %in%"mean", ]$Units <- "Mg C ha-1 (stocks) or Mg C ha-1 yr-1 (fluxes). Variables are identified as stocks of fluxes in the VARIABLES table. Records originally expressed in units of organic matter (OM) were converted to C assuming C =0.48*OM."

## Remove duplicated fields 
duplicated.fields <- ForC_simplified_meta[duplicated(ForC_simplified_meta$Field),]$Field

ForC_simplified_meta <- ForC_simplified_meta[!duplicated(ForC_simplified_meta$Field),]

ForC_simplified_meta[ForC_simplified_meta$Field %in% duplicated.fields,]

## Edit sites.sitename Description
ForC_simplified_meta[ForC_simplified_meta$Field %in% "sites.sitename", ]$Description <- "Site identifier, sufficient to uniquely identify the site within the paper."

## Add managed and disturbed metadata

managed.disturbed_meta <- data.frame(Column = nrow(ForC_simplified_meta)+c(1:2),
                      Field = c("managed", "disturbed"),
                      Description = c('Indicates whether plot has been managed. Plots are counted as managed if they have any records of management actions manipulating CO2, temperature, hydrology, nutrients, or biota (records in HISTORY, summarized in PLOTS) or if their site or plot name contained the word “plantation”, "planted", "managed", "irrigated" or "fertilized"', 'Indicates whether plot has undergone significant disturbance since the establishment of the oldest cohort. We removed stands that had undergone anthropogenic thinning or partial harvest ("Cut" or "Harvest" codes) unless this was very minor (mortality <<100%). Retains sites that were grazed or had undergone low severity natural disturbances (<10% mortality) including droughts, major storms, fires, and floods.'),
                      Storage.Type = "numeric (integer)",
                      Variable.Codes = "1-true; 0- false",
                      Units = "-",
                      n = nrow(ForC_simplified),
                      Min = 0,
                      Max = 1,
                      Source.Table = "None")

ForC_simplified_meta <- rbind(ForC_simplified_meta, managed.disturbed_meta)


## Add history.no.info metadata

history.no.info_meta <- data.frame(Column = nrow(ForC_simplified_meta)+1,
                                     Field = c("history.no.info"),
                                     Description = c("Indicates whether plot has 1- no info about history ('No.info' in hist.cat field of HISTORY table) or 0- some info about history."),
                                     Storage.Type = "numeric (integer)",
                                     Variable.Codes = "1-true; 0- false",
                                     Units = "-",
                                     n = nrow(ForC_simplified),
                                     Min = 0,
                                     Max = 1,
                                     Source.Table = "None")

ForC_simplified_meta <- rbind(ForC_simplified_meta, history.no.info_meta)

## Edit Column ID
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
ForC_simplified_meta$Column <- paste(1:nrow(ForC_simplified_meta), LETTERS702[1:nrow(ForC_simplified_meta)], sep = "/")

## Reorder columns
colomns.ForC_simp <- c("Column", "Source.Table", "Field", "Description", "Storage.Type", 
  "Variable.Codes", "Units", "n", "Min", "Max")

ForC_simplified_meta <- ForC_simplified_meta[, colomns.ForC_simp]

# Save ForC_simplified_metadata ####

write.csv(ForC_simplified_meta, file = "ForC_simplified/ForC_simplified_metadata.csv", row.names = F)






