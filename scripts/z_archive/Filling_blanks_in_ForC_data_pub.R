######################################################
# Purpose: Filling out the blanks in ForC data publication paper
# Inputs:  - ForC MEASUREMENTS table
#          - ForC PLOTS table
#          - ForC VARIABLES table
#          - ForC SITES table
# outputs: 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12-08)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load tables ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
VARIABLES <- read.csv("data/ForC_variables.csv", stringsAsFactors = F)


NA.forms <- c("NI", "NRA", "NaN", "NAC", "999")

# Get values for filling blanks ####


## now including ## records (previously 3568) ###

nrow(MEASUREMENTS) # 17,367

## representing ### plots (previously 845) ####
nrow(PLOTS) # 2,731

## in ## geographically distinct areas (previously 178) ####
length(unique(SITES$geographic.area)) #827

## includes ## C cycle variables collected between 1943 and 2015 ####
VARIABLES <- VARIABLES[order(VARIABLES$variable.name),]
VARIABLES[, c(1,3)]

sort(unique(gsub("C", "",  VARIABLES$variable.name, ignore.case = F))) # 93
unique(gsub("C", "",  VARIABLES[VARIABLES$variables.type == "primary",]$variables.name, ignore.case = F))
length(unique(gsub("C", "",  VARIABLES[VARIABLES$variables.type == "primary",]$variables.name, ignore.case = F))) # 65
length(unique(gsub("_C", "",  VARIABLES[VARIABLES$variables.type != "covariates",]$variables.name))) # 80


## FIGURE 1 . If this is unknown (## records), 
### Run this is Histogram_of_number_of_records_by_measurement_date.R sum(apply(cbind(dates.collection, dates.citation), 1, sum, na.rm = T) == 0) # 37 



## The database contains records from 2,731 plots (1,886 new/ 845 previously published in Anderson-Teixeira et al., 2016a,b) at 2,070 sites (1,567 new/ 503 previously published; Fig. 2)  ####

nrow(PLOTS) #2,731 plots
# 1886 new
# 84548 previously

nrow(SITES)
nrow(SITES[is.na(SITES$site.ID.v1), ]) # 1,345 new
nrow(SITES[!is.na(SITES$site.ID.v1), ]) # 725 previsouly

## representing 827 distinct geographic.areas (## new/ 178 previously  published). 

length(unique(SITES$geographic.area)) #827

length(unique(SITES[is.na(SITES$site.ID.v1), ]$geographic.area)) # 508 new
length(unique(SITES[!is.na(SITES$site.ID.v1), ]$geographic.area)) # 248 previously


## for the vast majority of records in ForC-db (##%), the dominant.life.form (dominant.life.form; MEASUREMENTS table) is woody vegetation ####

sum(MEASUREMENTS$dominant.life.form %in% "woody") *100 / nrow(MEASUREMENTS) # 99.1%

## but there are some records for savannas (mix of woody vegetation and grasses; n=## records from ## sites)  ####
sum(MEASUREMENTS$dominant.life.form %in% "woody+grass") # 150 records
unique(MEASUREMENTS[MEASUREMENTS$dominant.life.form %in% "woody+grass", ]$sites.sitename)
length(unique(MEASUREMENTS[MEASUREMENTS$dominant.life.form %in% "woody+grass", ]$sites.sitename)) # 18 sites


## or early seral grassland (n=## records from # sites)  ####
sum(MEASUREMENTS$dominant.life.form %in% "grass") # 2 records
unique(MEASUREMENTS[MEASUREMENTS$dominant.life.form %in% "grass", ]$sites.sitename)
length(unique(MEASUREMENTS[MEASUREMENTS$dominant.life.form %in% "grass", ]$sites.sitename)) # 1 site


## The largest number of records (##) comes from sites dominated by evergreen needleleaf trees, followed by broadleaf deciduous (##) and broadleaf evergreen (##) trees, with <## records each for mixes of broadleaf – and needleleaf trees, needleleaf deciduous trees, or other/ unclassified trees  ####
unique(MEASUREMENTS$dominant.veg)
sum(MEASUREMENTS$dominant.veg %in% "2TEN") # 7236 evergreen needleleaf
sum(MEASUREMENTS$dominant.veg %in% "2TDB") # 4582 broadleaf deciduous
sum(MEASUREMENTS$dominant.veg %in% "2TEB") # 4299 broadleaf evergreen

sum(! MEASUREMENTS$dominant.veg %in% c("2TEN", "2TDB", "2TEB")) # 1421 others records
max(table(MEASUREMENTS$dominant.veg[!MEASUREMENTS$dominant.veg %in% c("2TEN", "2TDB", "2TEB")]))# < 398 records each other



## Soil records exist for ##% of sites (##% of sites new to this version). ####
sum(apply(SITES[, c("soil.classification", "soil.texture", "soil.notes")], 1, function(x) any(!x %in% NA.forms))) *100 / nrow(SITES) # 97% records

sum(apply(SITES[!is.na(SITES$site.ID.v1), c("soil.classification", "soil.texture", "soil.notes")], 1, function(x) any(!x %in% NA.forms))) *100 / nrow(SITES[!is.na(SITES$site.ID.v1),]) # 100% new version


## Elevation (masl; SITES table) is recorded for ##% of sites (## % of new sites). Site elevation ranged from ## - ##, with ##% at elevations ≤500 m.a.s.l. (Fig. 4).  ####
sum(!SITES$masl %in% NA.forms & !is.na(SITES$masl)) *100 / nrow(SITES) # 59 %

sum(!SITES[!is.na(SITES$site.ID.v1), "masl"] %in% NA.forms & !is.na(SITES[!is.na(SITES$site.ID.v1), "masl"]))  *100 / nrow(SITES[!is.na(SITES$site.ID.v1),]) # 69.0%

x <- SITES$masl
x <- ifelse(x%in% NA.forms, NA, x)
x <- gsub(" ", "", x)
x <- unlist(strsplit(x, "-"))

range(x, na.rm = T) # 1 to 992

sum(na.omit(x) <= 500) * 100 / length(na.omit(x)) # 72%



## hydrology notes were acquired for only #% of sites in the database. ####

sum(!SITES$hydrology.notes %in% NA.forms & !is.na(SITES$hydrology.notes)) *100 / nrow(SITES) # 5.0%


## Stand age estimates are available for ##% of the measurement records, with an additional ##% known to be old-growth / undisturbed (Fig. 5).  ####
sum(!MEASUREMENTS$stand.age %in% NA.forms & !is.na(MEASUREMENTS$stand.age)) * 100 / nrow(MEASUREMENTS) # 83.5%
sum(MEASUREMENTS$stand.age %in% 999) * 100 / nrow(MEASUREMENTS) # 11.6%

## Of the 2,737 plot records, 1,207 (#%) contain records of dates of establishment of the oldest cohort of trees,####

sum(!PLOTS$year.establishment.oldest.trees %in% NA.forms & !is.na(PLOTS$year.establishment.oldest.trees)) # 1,206 records
sum(!PLOTS$year.establishment.oldest.trees %in% NA.forms & !is.na(PLOTS$year.establishment.oldest.trees)) *100 / nrow(PLOTS) # 44%

##  1,371 (#%) contain records of initiation of a post-disturbance cohort, ## (#%) contain records for one or more major disturbances ####

quoi

##  ## (#%) contain records for one or more non-stand clearing disturbances, and ## (#%) contain records for one or more management events.    ####

quoi

## The database includes fields for mean annual temperature (mat; recorded for ##% of sites), mean temperature of the coldest month (min.temp; recorded for ##% of sites), mean temperature of the warmest month (max.temp; recorded for ##% of sites), and mean annual precipitation (map; recorded for ##% of sites),  ####


sum(!SITES$mat %in% NA.forms & !is.na(SITES$mat)) *100 / nrow(SITES) # 78%
sum(!SITES$min.temp %in% NA.forms & !is.na(SITES$min.temp)) *100 / nrow(SITES) # 7%
sum(!SITES$max.temp %in% NA.forms & !is.na(SITES$max.temp)) *100 / nrow(SITES) # 7%
sum(!SITES$map %in% NA.forms & !is.na(SITES$map)) *100 / nrow(SITES) # 87%

## The database represents all of Earth’s major forested climates (Fig. 6), with mean annual temperature ranging from ## to ## °C and mean annual precipitation ranging from ## to ## mm yr-1.  ####

x <- as.numeric(SITES[!SITES$mat %in% NA.forms & !is.na(SITES$mat), "mat"])
range(x) # -16.8 to 30.1

x <- as.numeric(SITES[!SITES$map %in% NA.forms & !is.na(SITES$map), "map"])
range(x) # 55.5 to 7,7670


## Under the Köppen-Geiger climate classification system (Peel et al. 2007), coverage is approximately equal among the major forested climate zone groups (A- tropical, C-temperate, and D-cold), with ##-## sites for each.  The arid (B) and polar (E) climate zones contain little forest and are each represented by <## ForC sites.####

x <- SITES$Koeppen
x <- substr(x, 1, 1)

range(c(sum(x == "A"), sum(x == "C"), sum(x == "D"))) # 632-722

min(c (sum(x == "B"), sum(x == "E"))) # <23 ForC sites

## Figure 6 ####

###  Within Figure_of_Climate_of_forC.R script and then, run sum(apply(cbind(WorldClim.missing.map.mat$map, WorldClim.missing.map.mat$mat), 1, function(x) all(!is.na(x)))) # 407

## Plot area (plot.area) is recorded in the PLOTS table for ##% of records.  ##

sum(!PLOTS$plot.area %in% NA.forms & !is.na(PLOTS$plot.area)) * 100 / nrow(PLOTS) # 28 %


## ForC-db contains ## variables (VARIABLES table). ####
length(unique(gsub("_C", "",  VARIABLES[VARIABLES$variable.type == "flux",]$variable.name))) # 65
length(unique(VARIABLES[VARIABLES$variable.type == "primary",]$variables.name)) # 65


