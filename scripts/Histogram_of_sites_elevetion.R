######################################################
# Purpose: Plot histogram of elevation using ForC_db
# Inputs: ForC_db sites table
# outputs: tiff file with histogram
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.3.2 (2016-10-31)
######################################################


# Clean environment ####
rm(list = ls())

# Load labraries ####

library(dplyr)
library(tidyr)

# Setup working directory ####
setwd(".")

# Load sites table ####
SITES <- read.csv("ForC_sites.csv", stringsAsFactors = F)

# Plot histogram figure ####

tiff(file="Histogram_of_sites_elevation.tiff", width=169, height = 100, units = "mm", res = 300, pointsize = 12)
hist(as.numeric(SITES$masl), ylab = "Number of sites", xlab = "Elevation (m)", las = 1, main = "")
dev.off()

