######################################################
# Purpose: Plot histogram of elevation using ForC_db
# Inputs: ForC_db SITES table
# outputs: 1. png file, saved in figures folder as Histogram_of_sites_elevation.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.3.2 (2016-10-31)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load sites table ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

# Plot histogram figure ####

png(file="figures/Histogram_of_sites_elevation.png", width=169, height = 100, units = "mm", res = 300, pointsize = 12)
hist(as.numeric(SITES$masl), ylab = "Number of sites", xlab = "Elevation (m)", las = 1, main = "")
dev.off()

