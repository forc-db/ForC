######################################################
# Purpose: Pull out Koppen zone and FAO ecozone for the records for which these are missing 
# Inputs:  - ForC SITE table
#          - FAO shapefile file in S drive
#          - KOEPPEN shapefile in S drive
#          - KOEPPEN categories in S drive
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12-08)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####
library(rgdal)

# Load tables ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

# Load shapefiles and legends ####

# FAO <- readOGR("S:/Global Maps Data/Shapefiles/FAO global eco_zone/gez_2010_wgs84.shp", layer = "gez_2010_wgs84", stringsAsFactors = F)
# 
# KOEPPEN <- readOGR("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/koeppen_dissolved.shp", layer = "koeppen_dissolved", stringsAsFactors = F)
# categoriesKOEPPEN <- read.table("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/Legend.txt", stringsAsFactors = F)
# categoriesKOEPPEN <- categoriesKOEPPEN[, c(1,3)]
# colnames(categoriesKOEPPEN) <- c("GRIDCODE", "Koeppen")

load("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")


continents <- readOGR("supplementary_resources/World Map data/Continents/World_Continents.shp", stringsAsFactors = F)

biogeog <-  readOGR("supplementary_resources/World Map data/Biogegraphic_Regions/wwf_terrestrial_ecoregions.shp", stringsAsFactors = F)


# set SITES as coordinates ####

SITES.xy <- SITES
coordinates(SITES.xy) <- SITES.xy[, c("lon", "lat")]
proj4string(SITES.xy) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                             
# Extract FAO ####


# plot(FAO)
# points(SITES.xy, col = "red")
# CRS(SITES.xy)

new.FAO <- over(SITES.xy, FAO)$gez_name

new.FAO[SITES$FAO.ecozone %in% "NAC"]


View(cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1]%in% x[2]),]) # double checking that only "NAC" are given a "different" FAO.ecozone

# Ignore water and keep what was there before
new.FAO[new.FAO %in% "Water"] <- SITES$FAO.ecozone[new.FAO %in% "Water"]


# Ignore NA and keep what was there before
new.FAO[is.na(new.FAO)] <- SITES$FAO.ecozone[is.na(new.FAO)]


# Ignore Tropical rainforest and keep Tropical mountain system for site "La Fortuna Forest"

new.FAO[new.FAO %in% "Tropical rainforest" & grepl("La Fortuna Forest", SITES$sites.sitename)] <- SITES$FAO.ecozone[new.FAO %in% "Tropical rainforest" & grepl("La Fortuna Forest", SITES$sites.sitename)]



# Double check other differences
View(cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1] %in% x[2]),])



# Replace all FAO.ecozone by the ones extracted

SITES$FAO.ecozone <- new.FAO

SITES[is.na(SITES$FAO.ecozone) | SITES$FAO.ecozone %in% "NAC", c("site.ID", "sites.sitename", "lat", "lon", "country", "FAO.ecozone", "continent", "Koeppen")] # should be empty. if not, either fix the coordinates by hand or use code like the few lines below


# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "Japan" & SITES$Koeppen %in% "Cfa"] <- "Subtropical humid forest"
# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "United States of America" & SITES$Koeppen %in% c("Cfa", "Dfb")] <- "Temperate continental forest"
# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% c("Brazil", "French Guiana")  & SITES$Koeppen %in% "Af"] <- "Tropical rainforest"


SITES[SITES$site.ID %in% 3810, "FAO.ecozone"] <- "Subtropical humid forest"
SITES[SITES$site.ID %in% 3843,  "FAO.ecozone"] <- "Tropical rainforest"
SITES[SITES$site.ID %in% 3820,  "FAO.ecozone"] <- "Temperate oceanic forest"
SITES[SITES$site.ID %in% 3995,  "FAO.ecozone"] <- "Subtropical humid forest"

if(any(is.na(SITES$FAO.ecozone) | SITES$FAO.ecozone %in% "NAC")) stop("There are missing FAO.ecozone that you need to fill by hand in this script.")


# Extract KOEPPEN ####

plot(KOEPPEN, col = factor(KOEPPEN$GRIDCODE))

plot(KOEPPEN, col = ifelse(KOEPPEN$GRIDCODE == 11, "red", "grey"))
points(SITES.xy[SITES.xy$Koeppen %in% "NAC",], pch = 16, col = "red")

new.KOEPPEN <- over(SITES.xy, KOEPPEN)
new.KOEPPEN$order <- 1:nrow(new.KOEPPEN)
new.KOEPPEN <- merge(new.KOEPPEN, categoriesKOEPPEN, by = "GRIDCODE", all.x = T, sort = F)
new.KOEPPEN <- new.KOEPPEN[order(new.KOEPPEN$order),]

new.KOEPPEN[SITES$Koeppen %in% "NAC",] # if all Csb is ok, in our case

new.KOEPPEN <- new.KOEPPEN$Koeppen

identical(SITES.xy$site.ID, SITES$site.ID)

cbind(SITES$sites.sitename, SITES$Koeppen, new.KOEPPEN)[!apply(cbind(SITES$Koeppen, new.KOEPPEN), 1, function(x) x[1] %in% x[2]),] # should be empty

SITES[is.na(new.KOEPPEN), c("sites.sitename", "lat", "lon", "country", "Koeppen", "geography.notes", "climate.notes", "site.notes", "ref.notes", "site.ref")]

plot(KOEPPEN, col = factor(KOEPPEN$GRIDCODE))
points(SITES.xy[is.na(new.KOEPPEN), ])

# Replace all KOEPPEN by the ones extracted

SITES$Koeppen <- new.KOEPPEN

SITES$Koeppen[is.na(SITES$Koeppen) & SITES$FAO.ecozone %in% "Tropical rainforest"] <- "Af"
SITES$Koeppen[is.na(SITES$Koeppen) & SITES$FAO.ecozone %in% "Subtropical humid forest"] <- "Cfa"

SITES[SITES$site.ID %in% 3819,  c("Koeppen")] <- c("Cfb")

if(any(is.na(SITES$Koeppen) | SITES$Koeppen %in% "NAC")) stop("There are missing Koeppen that you need to fill by hand in this script.")
plot(KOEPPEN)
points(SITES.xy[is.na(SITES$Koeppen) | SITES$Koeppen %in% "NAC", ], col = "red", pch= 16)
SITES.xy[is.na(SITES$Koeppen) | SITES$Koeppen %in% "NAC", ]

# Extract biogeog ####
plot(biogeog, col = factor(biogeog$REALM_1))


new.biogeog <- over(SITES.xy, biogeog)

new.biogeog[is.na(new.biogeog) & SITES$biogeog %in% "NAC",]
View(SITES[is.na(new.biogeog)  & SITES$biogeog %in% "NAC", ])

points(SITES.xy[is.na(c(new.biogeog[[1]])),], col = "red", pch= 16)

new.biogeog[is.na(new.biogeog)] <- SITES.xy$biogeog[is.na(new.biogeog)]

SITES$biogeog <- new.biogeog


# Extract continent ####
plot(continents)

new.continent <- over(SITES.xy, continents)

new.continent[SITES$continent %in% "NAC",]

new.continent <- new.continent$CONTINENT

plot(SITES.xy, pch = 16, col = factor(SITES$continent)) # should look the same as previous plot

identical(SITES.xy$site.ID, SITES$site.ID) # has to be TRUE

cbind(SITES$sites.sitename, SITES$continent, new.continent)[!apply(cbind(SITES$continent, new.continent), 1, function(x) x[1] %in% x[2]),] # should be empty


SITES[is.na(new.continent), c("sites.sitename", "lat", "lon", "country")]
plot(continents)
points(SITES.xy[is.na(new.continent),], pch = 2, col = "purple")

# Replace all continent by the ones extracted + manually edit the ones that fall in the water
SITES$continent <- new.continent

SITES$continent[is.na(SITES$continent) & SITES$country %in% c("United States of America", "United States Virgin Islands", "USA", "Costa Rica")] <- "North America"
SITES$continent[is.na(SITES$continent) & SITES$country %in% "Australia"] <- "Australia"
SITES$continent[is.na(SITES$continent) & SITES$country %in% "New Zealand"] <- "Oceania"

SITES$continent[is.na(SITES$continent) & SITES$country %in% c("Malaysia", "Japan", "China", "Cambodia", "Turkey")] <- "Asia"
SITES$continent[is.na(SITES$continent) & SITES$country %in% c("French Guiana", "Brazil", "Panama", "Belize", "Colombia")] <- "South America"


if(any(is.na(SITES$continent) | SITES$continent %in% "NAC")) stop("There are missing continent that you need to fill by hand in this script.")

SITES[is.na(SITES$continent) | SITES$continent %in% "NAC", c("sites.sitename", "lat", "lon", "country")]
points(SITES.xy[is.na(SITES$continent)| SITES$continent%in% "NAC",], pch = 16, col = "red")


# SAVE ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)

