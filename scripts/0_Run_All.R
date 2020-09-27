######################################################
# Purpose: Run all necessary scripts after an update in one of the table of the database
# Developped by: Valentine Herrmann - HerrmannV@si.edu
#  R version 3.5.1 (2018-07-02)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Run all necessary scripts

## check for special characters
source("scripts/QA_QC/checks_for_special_characters.R") # if no errors show up, you are good to move on

## Generate plots from History ####
source("scripts/Database_manipulation/Generate PLOTS from HISTORY/ForC_plots_from_history.R")


## Data quality and Checks
source("scripts/QA_QC/checks.R")


## before getting coordinare related info, seperate sites without coordinates
source("scripts/Database_manipulation/Separate_out_SITES_with_missing_coordinates.R")


## Get biogeog, Koeppen and FAO zones
source("scripts/Database_manipulation/Extract_site_info_based_on_lat_long/Extract_Koppen_and_FAO_zones_frome_shapefiles.R")

## Group sites into areas
source("scripts/Database_manipulation/Extract_site_info_based_on_lat_long/Group_sites_into_geographic_areas_25km.R")

## ID SITES duplicates
source("scripts/Database_manipulation/Identify_and_resolve_duplicates/ID_sets_of_SITES_duplicates.R")

## ID MEASUREMENTS duplicates
source("scripts/Database_manipulation/Identify_and_resolve_duplicates/ID_sets_of_duplicate_records.R")

## Fill in dominant.veg when we can now (commented out when no new data because is a little long)
# source("scripts/Database_manipulation/Fill_in_dominant_veg_when_possible_to_know.R")

## Temperarily flag SRDB and GROA potential duplicates
# source("scripts/Database_manipulation/Temporarily_flag_some_potential_duplicates_SRDB_and_GROA.R)


## Generate ForC_simplified and ForC_simplified_metadata ####
### this internally runs (source("scripts/Database_manipulation/Reconcile_duplicated_records.R"))
source("scripts/Database_manipulation/Create_ForC_simplified.R")
source("scripts/Database_manipulation/Create_ForC_simplified_metadata.R")


## Database numbers and facts ####
source("scripts/Database_numbers_and_facts/Calculate_biome_average_and_age_trend_for_any_ForC_variable.R") # previously Calculate_biome_average_for_any_ForC_variable.R"
source("scripts/Database_numbers_and_facts/Sample_size_per_variable_and forest_biome_combination.R")
source("scripts/Database_numbers_and_facts/Test_for_C_cycle_closure_(consistency).R")

## Figures ####
# source("scripts/Figures/Age_trend_in_C_cycle_variables.R") #this is now in Calculate_biome_average_and_age_trend_for_any_ForC_variable
source("scripts/Figures/C_Cylce_diagrams.R")
source("scripts/Figures/Figure_of_Climate_of_forC_sites.R")
source("scripts/Figures/Histogram_of_dominant_vegetation.R")
source("scripts/Figures/Histogram_of_number_of_records_by_measurement_date.R")
source("scripts/Figures/Histogram_of_sites_elevetion.R")
source("scripts/Figures/Histogram_of_Stand_Age.R")
source("scripts/Figures/World_Map_with_Biogeographic_Regions_and_ForC_Sites.R")
source("scripts/Figures/World_Map_with_Biomes_and_ForC_simplified_Sites_for_ERL_review.R")





## put back sites without coordinates back in
source("scripts/Database_manipulation/Put_back_SITES_with_missing_coordinates_into_SITES.R")


## update metadata of ForC ? need to fix data way outside of range first...


