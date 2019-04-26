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

FAO <- readOGR("S:/Global Maps Data/Shapefiles/FAO global eco_zone/gez_2010_wgs84.shp", layer = "gez_2010_wgs84", stringsAsFactors = F)

KOEPPEN <- readOGR("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/koeppen_dissolved.shp", layer = "koeppen_dissolved", stringsAsFactors = F)
categoriesKOEPPEN <- read.table("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/Legend.txt", stringsAsFactors = F)
categoriesKOEPPEN <- categoriesKOEPPEN[, c(1,3)]
colnames(categoriesKOEPPEN) <- c("GRIDCODE", "Koeppen")


continents <- readOGR("supplementary_resources/World Map data/Continents/World_Continents.shp", stringsAsFactors = F)


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


cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1]%in% x[2]),] # double checking that only "NAC" are given a "different" FAO.ecozone

# Ignore water and keep what was there before
new.FAO[new.FAO %in% "Water"] <- SITES$FAO.ecozone[new.FAO %in% "Water"]

# Double check othere differences
cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1] %in% x[2]),]



# Replace all FAO.ecozone by the ones extracted

SITES$FAO.ecozone <- new.FAO

SITES[is.na(SITES$FAO.ecozone), c("sites.sitename", "lat", "lon", "country", "FAO.ecozone", "biogeog", "Koeppen")]


SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "Japan" & SITES$Koeppen %in% "Cfa"] <- "Subtropical humid forest"
SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "United States of America" & SITES$Koeppen %in% c("Cfa", "Dfb")] <- "Temperate continental forest"
SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% c("Brazil", "French Guiana")  & SITES$Koeppen %in% "Af"] <- "Tropical rainforest"

if(any(is.na(SITES$Koeppen))) stop("There are missing Koeppen that you need to fill by hand in this script.")


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

if(any(is.na(SITES$Koeppen))) stop("There are missing Koeppen that you need to fill by hand in this script.")

# Extract biogeog ####
plot(continents)
points(SITES.xy, pch = 16, col = factor(new.biogeog))

new.biogeog <- over(SITES.xy, continents)

new.biogeog[SITES$biogeog %in% "NAC",]

new.biogeog <- new.biogeog$CONTINENT

plot(SITES.xy, pch = 16, col = factor(SITES$biogeog)) # should look the same as previous plot

identical(SITES.xy$site.ID, SITES$site.ID) # has to be TRUE

cbind(SITES$sites.sitename, SITES$biogeog, new.biogeog)[!apply(cbind(SITES$biogeog, new.biogeog), 1, function(x) x[1] %in% x[2]),] # should be empty


SITES[is.na(new.biogeog), c("sites.sitename", "lat", "lon", "country")]
plot(continents)
points(SITES.xy[is.na(new.biogeog),], pch = 2, col = "purple")

# Replace all biogeog by the ones extracted + manually edit the ones that fall in the water
SITES$biogeog <- new.biogeog

SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("United States of America", "United States Virgin Islands")] <- "North America"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% "Australia"] <- "Australia"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("Malaysia", "Japan", "China", "Cambodia")] <- "Asia"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("French Guiana", "Brazil")] <- "South America"


if(any(is.na(SITES$biogeog))) stop("There are missing biogeog that you need to fill by hand in this script.")

# SAVE ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)
