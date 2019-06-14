######################################################
# Purpose: put back sites without coordinates into SITES
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
SITES_MISSING_COOR <- read.csv("data/ForC_sites_missing_coordinates.csv", stringsAsFactors = F)

# SAVE ####

write.csv(rbind(SITES, SITES_MISSING_COOR), "data/ForC_sites.csv", row.names = F)
