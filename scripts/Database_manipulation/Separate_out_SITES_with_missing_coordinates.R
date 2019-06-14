######################################################
# Purpose: Separate SITES into 2 files, one (normal) with and one (new) without sites coordinates, so that other scripts run smoothly
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12-08)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####

# Load tables ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
# SITES_MISSING_COOR <- read.csv("data/ForC_sites_missing_coordinates.csv", stringsAsFactors = F)

# separate out SITES with missing coordinates ####
SITES_with_missing_coordinates <- SITES[apply(SITES[, c("lon", "lat")], 1, function(x) any(is.na(x))), ]
SITES_without_missing_coordinates <- SITES[apply(SITES[, c("lon", "lat")], 1, function(x) !any(is.na(x))), ]

if(!(nrow(SITES_without_missing_coordinates) + nrow(SITES_with_missing_coordinates)) == nrow(SITES)) stop("There is a problem, the number of rows don't add up right")

# SITES_with_missing_coordinates <- rbind(SITES_MISSING_COOR, SITES_with_missing_coordinates)
# SAVE ####

write.csv(SITES_without_missing_coordinates, "data/ForC_sites.csv", row.names = F)
write.csv(SITES_with_missing_coordinates, "data/ForC_sites_missing_coordinates.csv", row.names = F)
