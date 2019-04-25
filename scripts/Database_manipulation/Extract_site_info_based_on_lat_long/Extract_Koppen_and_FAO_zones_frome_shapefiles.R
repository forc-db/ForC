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
idx.non.missing.coordinates <- apply(SITES[, c("lon", "lat")], 1, function(x) !any(is.na(x)))

SITES.xy <- SITES[idx.non.missing.coordinates, ]
coordinates(SITES.xy) <- SITES.xy[, c("lon", "lat")]
proj4string(SITES.xy) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                             
# Extract FAO ####


# plot(FAO)
# points(SITES.xy, col = "red")
# CRS(SITES.xy)

new.FAO <- over(SITES.xy, FAO)$gez_name

new.FAO[SITES[idx.non.missing.coordinates, ]$FAO.ecozone %in% "NAC"]


View(cbind(SITES[idx.non.missing.coordinates, ]$sites.sitename, SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO)[!apply(cbind(SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO), 1, function(x) x[1]%in% x[2]),]) # double checking that only "NAC" are given a "different" FAO.ecozone

# Ignore water and keep what was there before
new.FAO[new.FAO %in% "Water"] <- SITES[idx.non.missing.coordinates, ]$FAO.ecozone[new.FAO %in% "Water"]

# Ignore NA and keep what was there before
new.FAO[is.na(new.FAO)] <- SITES[idx.non.missing.coordinates, ]$FAO.ecozone[is.na(new.FAO)]


# Double check other differences
cbind(SITES[idx.non.missing.coordinates, ]$sites.sitename, SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO)[!apply(cbind(SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO), 1, function(x) x[1] %in% x[2]),]

table( SITES[idx.non.missing.coordinates, ]$FAO.ecozone[!apply(cbind(SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO), 1, function(x) x[1] %in% x[2])], new.FAO[!apply(cbind(SITES[idx.non.missing.coordinates, ]$FAO.ecozone, new.FAO), 1, function(x) x[1] %in% x[2])]) # if only one row for "NAC, you are good to go

# Replace all FAO.ecozone by the ones extracted

SITES[idx.non.missing.coordinates, ]$FAO.ecozone <- new.FAO

SITES[is.na(SITES$FAO.ecozone), c("sites.sitename", "lat", "lon", "country", "FAO.ecozone", "biogeog", "Koeppen")] # should be empty

SITES[SITES$FAO.ecozone %in% "NAC", c("sites.sitename", "lat", "lon", "country", "FAO.ecozone", "biogeog", "Koeppen")] # if there are still "NAC", and lat lon are not NA, that means the coordinates are falling in a place where no FAO are define... 

SITES[SITES$FAO.ecozone %in% "NAC" & !is.na(SITES$lat) , c("sites.sitename", "lat", "lon", "country", "FAO.ecozone", "biogeog", "Koeppen", "loaded.by")] # if there are still "NAC", and lat lon are not NA, that means the coordinates are falling in a place where no FAO are define... 

# plot(FAO)
# points(SITES.xy[new.FAO %in% "NAC", ], pch = 2, col = "red")

# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "Japan" & SITES$Koeppen %in% "Cfa"] <- "Subtropical humid forest"
# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% "United States of America" & SITES$Koeppen %in% c("Cfa", "Dfb")] <- "Temperate continental forest"
# SITES$FAO.ecozone[is.na(SITES$FAO.ecozone) & SITES$country %in% c("Brazil", "French Guiana")  & SITES$Koeppen %in% "Af"] <- "Tropical rainforest"

if(any(is.na(SITES$FAO.ecozone))) stop("There are missing Koeppen that you need to fill by hand in this script.")


# Extract KOEPPEN ####

plot(KOEPPEN, col = factor(KOEPPEN$GRIDCODE))

plot(KOEPPEN, col = ifelse(KOEPPEN$GRIDCODE == 11, "red", "grey"))
points(SITES.xy[SITES.xy$Koeppen %in% "NAC",], pch = 16, col = "red")

new.KOEPPEN <- over(SITES.xy, KOEPPEN)
new.KOEPPEN$order <- 1:nrow(new.KOEPPEN)
new.KOEPPEN <- merge(new.KOEPPEN, categoriesKOEPPEN, by = "GRIDCODE", all.x = T, sort = F)
new.KOEPPEN <- new.KOEPPEN[order(new.KOEPPEN$order),]

new.KOEPPEN[SITES[idx.non.missing.coordinates, ]$Koeppen %in% "NAC",] # if all Csb is ok, in our case

new.KOEPPEN <- new.KOEPPEN$Koeppen

identical(SITES.xy$site.ID, SITES[idx.non.missing.coordinates, ]$site.ID)

cbind(SITES[idx.non.missing.coordinates, ]$sites.sitename, SITES[idx.non.missing.coordinates, ]$Koeppen, new.KOEPPEN)[!apply(cbind(SITES[idx.non.missing.coordinates, ]$Koeppen, new.KOEPPEN), 1, function(x) x[1] %in% x[2]),] # should be empty

SITES[idx.non.missing.coordinates, ][is.na(new.KOEPPEN), c("sites.sitename", "lat", "lon", "country", "Koeppen", "geography.notes", "climate.notes", "site.notes", "ref.notes", "site.ref", "loaded.by")]

plot(KOEPPEN, col = factor(KOEPPEN$GRIDCODE))
points(SITES.xy[is.na(new.KOEPPEN), ])


# Ignore NA and keep what was there before
new.KOEPPEN[is.na(new.KOEPPEN)] <- SITES[idx.non.missing.coordinates, ]$Koeppen[is.na(new.KOEPPEN)]

# Replace all KOEPPEN by the ones extracted
SITES[idx.non.missing.coordinates, ]$Koeppen <- new.KOEPPEN

SITES$Koeppen[is.na(SITES$Koeppen) & SITES$FAO.ecozone %in% "Tropical rainforest"] <- "Af"
SITES$Koeppen[(SITES$Koeppen == "NAC") & SITES$FAO.ecozone %in% "Subtropical humid forest"] <- "Cfa"

if(any(is.na(SITES$Koeppen))) stop("There are missing Koeppen that you need to fill by hand in this script.")

# Extract biogeog ####
plot(continents)

new.biogeog <- over(SITES.xy, continents)

points(SITES.xy, pch = 16, col = factor(new.biogeog))

new.biogeog[SITES[idx.non.missing.coordinates, ]$biogeog %in% "NAC",]

new.biogeog <- new.biogeog$CONTINENT

plot(SITES.xy, pch = 16, col = factor(SITES$biogeog)) # should look the same as previous plot

identical(SITES.xy$site.ID, SITES[idx.non.missing.coordinates, ]$site.ID) # has to be TRUE

cbind(SITES[idx.non.missing.coordinates, ]$sites.sitename, SITES[idx.non.missing.coordinates, ]$biogeog, new.biogeog)[!apply(cbind(SITES[idx.non.missing.coordinates, ]$biogeog, new.biogeog), 1, function(x) x[1] %in% x[2]),] # should be empty


SITES[idx.non.missing.coordinates, ][is.na(new.biogeog), c("sites.sitename", "lat", "lon", "country")]
plot(continents)
points(SITES.xy[is.na(new.biogeog),], pch = 2, col = "purple")

# Replace all biogeog by the ones extracted + manually edit the ones that fall in the water
SITES[idx.non.missing.coordinates, ]$biogeog <- new.biogeog

SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("United States of America", "United States Virgin Islands", "USA")] <- "North America"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("Australia", "New Zealand")] <- "Australia"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("Malaysia", "Japan", "China", "Cambodia", "Turkey")] <- "Asia"
SITES$biogeog[is.na(SITES$biogeog) & SITES$country %in% c("French Guiana", "Brazil", "Panama", "Belize")] <- "South America"

SITES[is.na(SITES$biogeog),]

if(any(is.na(SITES$biogeog))) stop("There are missing biogeog that you need to fill by hand in this script.")

# SAVE ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)
