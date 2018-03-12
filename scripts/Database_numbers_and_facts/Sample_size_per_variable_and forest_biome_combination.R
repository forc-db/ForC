######################################################
# Purpose: extract number of records for each variable (combining _OM and _C) for the following forest types:
# - tropical (Koppen = A_) broadleaf ≥100 yrs
# - tropical (Koppen = A_) broadleaf <100 yrs
# - temperate (Koppen= C__, D_a or D_b) broadleaf ≥100 yrs
# - temperate (Koppen= C__, D_a or D_b) broadleaf <100 yrs
# - temperate (Koppen= C__, D_a or D_b) conifer ≥100 yrs
# - temperate (Koppen= C__, D_a or D_b) conifer <100 yrs
# - boreal (Koppen=D_c or D_d) conifer ≥100 yrs
# - boreal (Koppen=D_c or D_d) conifer <100 yrs
# Koppen comes from SITES table, forest type from dominant.veg field of MEAUREMENTS table, age from stand.age field of MEAUREMENTS table.
# Inputs: - ForC SITES table
#         - ForC MEASUREMENTS table
# outputs: table with number of records for each variable
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-12)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####
library(reshape)

# Load tables ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# Create Forest-biome combinations ####

## merge Koeppen of SITES into MEASUREMENTS
MEASUREMENTS <- merge(MEASUREMENTS, SITES[, c("sites.sitename", "Koeppen")], all.x = T)

## Rename Koeppen with english words
unique(MEASUREMENTS$Koeppen)

MEASUREMENTS$Koeppen_name <- MEASUREMENTS$Koeppen

MEASUREMENTS$Koeppen_name <- gsub("A{1,}(\\w*)", "tropical",  MEASUREMENTS$Koeppen_name, perl=TRUE) # replace "A_" by "tropical"
MEASUREMENTS$Koeppen_name <- gsub("C{1,}(\\w*)", "temperate",  MEASUREMENTS$Koeppen_name, perl=TRUE) # replace "C_" by "temperate"
MEASUREMENTS$Koeppen_name <- gsub("D{1,}(\\w*)[a|b]{1,}", "temperate",  MEASUREMENTS$Koeppen_name, perl=TRUE) # replace "D_a or D_b" by "temperate"
MEASUREMENTS$Koeppen_name <- gsub("D{1,}(\\w*)[c|d]{1,}", "boreal",  MEASUREMENTS$Koeppen_name, perl=TRUE) # replace "D_a or D_b" by "temperate"

MEASUREMENTS$Koeppen_name <- ifelse(MEASUREMENTS$Koeppen_name %in% c("tropical", "temperate", "boreal"), MEASUREMENTS$Koeppen_name, "other biome")

unique(MEASUREMENTS$Koeppen_name)

## rename dominant.veg with english words
unique(MEASUREMENTS$dominant.veg)

broadleaf_codes <- c("2TEB", "2TDB", "2TB")
conifer_codes <- c("2TEN", "2TDN", "2TN")
mix_codes <-  c("2TE",  "2TD", "2TM", "2TREE")

MEASUREMENTS$Forest_type <- MEASUREMENTS$dominant.veg

MEASUREMENTS$Forest_type <- ifelse(MEASUREMENTS$Forest_type %in% broadleaf_codes, "broadleaf", MEASUREMENTS$Forest_type)
MEASUREMENTS$Forest_type <- ifelse(MEASUREMENTS$Forest_type %in% conifer_codes, "conifer", MEASUREMENTS$Forest_type)
MEASUREMENTS$Forest_type <- ifelse(MEASUREMENTS$Forest_type %in% mix_codes, "mix", MEASUREMENTS$Forest_type)

MEASUREMENTS$Forest_type <- ifelse(MEASUREMENTS$Forest_type %in% c("broadleaf", "conifer", "mix"), MEASUREMENTS$Forest_type, "other veg type")

unique(MEASUREMENTS$Forest_type)

## recatgeorize ages
unique(MEASUREMENTS$stand.age)

MEASUREMENTS$age100 <- as.numeric(MEASUREMENTS$stand.age)
MEASUREMENTS$age100 <- ifelse(!is.na(MEASUREMENTS$age100) & MEASUREMENTS$age100 < 100, "< 100 years",
                              ifelse(!is.na(MEASUREMENTS$age100) & MEASUREMENTS$age100 >= 100, paste("\u2265", "100 years"), "age unknown"))

unique(MEASUREMENTS$age100)


## Forest-biome combinations
# MEASUREMENTS$Forest_biome_combinations <- paste(MEASUREMENTS$Koeppen_name, MEASUREMENTS$Forest_type, MEASUREMENTS$age100)
# unique(MEASUREMENTS$Forest_biome_combinations)


## Combine _OM and _OC variables ####
unique(MEASUREMENTS$variable.name)

MEASUREMENTS$variable.name.combined <- MEASUREMENTS$variable.name
MEASUREMENTS$variable.name.combined <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS$variable.name.combined, perl = T)



# Count number of records per Forest-biome and variables

A.melt <- melt(MEASUREMENTS, id.vars = c("Koeppen_name", "Forest_type", "age100", "variable.name.combined"), measure.vars = "mean")
A.cast <- cast(A.melt,  Koeppen_name + Forest_type + age100 ~ variable.name.combined )

A <- datatable(A.cast, options = list(pageLength = 50))

setwd("numbers_and_facts")
saveWidget(A, "Sample_size_per_variable_and_forest_biome_combination.html")

setwd(dirname(getwd()))
