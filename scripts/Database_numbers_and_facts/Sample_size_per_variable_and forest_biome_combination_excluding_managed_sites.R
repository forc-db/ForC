######################################################
# Purpose: extract number of records for each variable (combining _OM and _C) for the following forest types:
# - tropical (Koppen = A_) broadleaf >200 yrs
# - tropical (Koppen = A_) broadleaf 100 - 200 yrs
# - tropical (Koppen = A_) broadleaf <100 yrs
# - temperate (Koppen= C__, D_a or D_b) broadleaf >200 yrs
# - temperate (Koppen= C__, D_a or D_b) broadleaf 100 - 200 yrs
# - temperate (Koppen= C__, D_a or D_b) broadleaf <100 yrs
# - temperate (Koppen= C__, D_a or D_b) conifer >200 yrs
# - temperate (Koppen= C__, D_a or D_b) conifer 100 - 200 yrs
# - temperate (Koppen= C__, D_a or D_b) conifer <100 yrs
# - boreal (Koppen=D_c or D_d) conifer >200 yrs
# - boreal (Koppen=D_c or D_d) conifer 100 - 200 yrs
# - boreal (Koppen=D_c or D_d) conifer <100 yrs
# Koppen comes from SITES table, management comes from PLOTS table, forest type from dominant.veg field of MEAUREMENTS table, age from stand.age field of MEAUREMENTS table.
# Inputs: - ForC SITES table
#         - ForC MEASUREMENTS table
#         - ForC PLOTS table
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
library(DT)

# Load tables ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)

## Managed ####

# 1. Find plots with management record (recorded in HISTORY but as summarized in PLOTS) -- i.e., anything with management.[XXX].ID not equal to zero

plots.management.columns <- names(PLOTS)[grepl("management", names(PLOTS)) & !names(PLOTS) %in% "management.other.ID"]
managed <- PLOTS[, c("sites.sitename", "plot.name"),][apply(PLOTS[, plots.management.columns], 1, function(x) any(x!=0)),]
unmanaged.for.now <- PLOTS[, c("sites.sitename", "plot.name"),][apply(PLOTS[, plots.management.columns], 1, function(x) all(x==0)),] # calling "for now because ste 2below is done on those to further filter out)

if((nrow(managed) + nrow(unmanaged.for.now)) != nrow(PLOTS)) {stop("Problem when ID-ing the managed sites. unmanaged + managed does not equal total of sites")}

managed.1 <- paste(managed[,1], managed[,2])
unmanaged.for.now <- paste(unmanaged.for.now[,1], unmanaged.for.now[,2]) # calling "for now" because still need to be filtered by next step

# 2. Find any site.name or plot.name containing "plantation", "planted", "managed", "irrigated" or "fertilied"

managed.2 <- unmanaged.for.now[grepl("(plantation)|(planted)|(\\bmanaged)|(irrigated)|(fertilized)", unmanaged.for.now, perl = T, ignore.case = T)]

# 3. Give a 1 to all managed plots found in 1. and 2. 
all.managed.sites.plot.name <- c(managed.1, managed.2)
PLOTS$managed <- ifelse(paste(PLOTS$sites.sitename, PLOTS$plot.name) %in% all.managed.sites.plot.name, 1, 0)

# 4. double check we got all
if(any(!c( paste(PLOTS$sites.sitename, PLOTS$plot.name)[grepl("(plantation)|(planted)|(\\bmanaged)|(irrigated)|(fertilized)", paste(PLOTS$sites.sitename, PLOTS$plot.name), perl = T, ignore.case = T)]) %in% all.managed.sites.plot.name)) { stop("Didn't get all managed sites")}# double check we got all either in managed.1 or managed.2... 



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
                              ifelse(!is.na(MEASUREMENTS$age100) & MEASUREMENTS$age100 >= 100 & MEASUREMENTS$age100 < 200, "100 - 200 years",
                                     ifelse(!is.na(MEASUREMENTS$age100) & MEASUREMENTS$age100 >= 200, "> 200 years","age unknown")))

unique(MEASUREMENTS$age100)


## Forest-biome combinations
# MEASUREMENTS$Forest_biome_combinations <- paste(MEASUREMENTS$Koeppen_name, MEASUREMENTS$Forest_type, MEASUREMENTS$age100)
# unique(MEASUREMENTS$Forest_biome_combinations)


#merge managed of plots data into MEASUREMENTS##
m <- match(paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name), paste(PLOTS$sites.sitename, PLOTS$plot.name))
MEASUREMENTS <- cbind(MEASUREMENTS, PLOTS[m, ])
MEASUREMENTS <- MEASUREMENTS[!MEASUREMENTS$managed %in% 1, ]

unique(MEASUREMENTS$managed)

## Combine _OM and _OC variables ####
unique(MEASUREMENTS$variable.name)

MEASUREMENTS$variable.name.combined <- MEASUREMENTS$variable.name
MEASUREMENTS$variable.name.combined <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS$variable.name.combined, perl = T)




# Count number of records per Forest-biome and variables

A.melt <- melt(MEASUREMENTS, id.vars = c("Koeppen_name", "Forest_type", "age100", "variable.name.combined"), measure.vars = "mean")
A.cast <- cast(A.melt,  Koeppen_name + Forest_type + age100 ~ variable.name.combined )

A <- datatable(A.cast, options = list(pageLength = 50))

setwd("numbers_and_facts")
saveWidget(A, "Sample_size_per_variable_and_forest_biome_combination_with_age_and_management.html")
write.csv(A.cast, "Sample_size_per_variable_and_forest_biome_combination_with_age_and_management.csv")

setwd(dirname(getwd()))
