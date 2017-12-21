######################################################
# Purpose: Pull out Koppen zone and FAO ecozone for the records for which these are missing 
# Inputs:  - ForC SITE table
#          - FAO shapefile file in R drive
#          - KOEPPEN shapefile in R drive
#          - KOEPPEN categories in R drive
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

FAO <- readOGR("R:/Global Maps Data/Shapefiles/FAO global eco_zone/gez_2010_wgs84.shp", layer = "gez_2010_wgs84", stringsAsFactors = F)

KOEPPEN <- readOGR("R:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/koeppen_dissolved.shp", layer = "koeppen_dissolved", stringsAsFactors = F)
categoriesKOEPPEN <- read.table("R:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/Legend.txt", stringsAsFactors = F)
categoriesKOEPPEN <- categoriesKOEPPEN[, c(1,3)]
colnames(categoriesKOEPPEN) <- c("GRIDCODE", "Koeppen")


# set SITES as coordinates ####

SITES.xy <- SITES
coordinates(SITES.xy) <- SITES.xy[, c("lon", "lat")]
proj4string(SITES.xy) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
                             
# Extract FAO ####


# plot(FAO)
# points(SITES.xy, col = "red")
# CRS(SITES.xy)

new.FAO <- over(SITES.xy, FAO)$gez_name

new.FAO[SITES$FAO.ecozone == "NAC"]


cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1]== x[2]),]


A <- cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1]== x[2]),]
A
A <- A[-c(6:62),]
A
A <- A[-c(1, 5, 7, 9, 11),]
A

# Ignor water and keep what was there before
new.FAO[new.FAO %in% "Water"] <- SITES$FAO.ecozone[new.FAO %in% "Water"]

# Double check othere differences
A <- cbind(SITES$sites.sitename, SITES$FAO.ecozone, new.FAO)[!apply(cbind(SITES$FAO.ecozone, new.FAO), 1, function(x) x[1]== x[2]),]
A
A <-  A[-c(3:59),]
A
A <- A[-c(1, 2, 4, 6, 8),]
A

SITES$FAO.ecozone <- new.FAO



# Extract KOEPPEN ####
plot(KOEPPEN, col = ifelse(KOEPPEN$GRIDCODE == 11, "red", "grey"))
points(SITES.xy[SITES.xy$Koeppen == "NAC",], pch = 16, col = "red")

new.KOEPPEN <- over(SITES.xy, KOEPPEN)
new.KOEPPEN$order <- 1:nrow(new.KOEPPEN)
new.KOEPPEN <- merge(new.KOEPPEN, categoriesKOEPPEN, by = "GRIDCODE", all.x = T, sort = F)
new.KOEPPEN <- new.KOEPPEN[order(new.KOEPPEN$order),]

new.KOEPPEN[SITES$Koeppen == "NAC",] # if all Csb is ok, in our case

new.KOEPPEN <- new.KOEPPEN$Koeppen

identical(SITES.xy$site.ID, SITES$site.ID)

A <- cbind(SITES$sites.sitename, SITES$Koeppen, new.KOEPPEN)[!apply(cbind(SITES$Koeppen, new.KOEPPEN), 1, function(x) x[1]== x[2]),]

A

A <- A[-c(10:66),]
A
A <- A[-c(1:2, 17),]
A

# Replace all KOEPPEN by the ones extracted

SITES$Koeppen <- new.KOEPPEN


# SAVE ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)
