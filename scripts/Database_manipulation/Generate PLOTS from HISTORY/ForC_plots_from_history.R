######################################################
# Purpose: Generate ForC_plots from ForC_history
# Developped by: Valentine Herrmann - HerrmannV@si.edu (original script was developped by Kristina Anderson-Teixeira in Matlab)
#  R version 3.5.1 (2018-07-02)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(progress)

# Load data ####
HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)
PLOTS <-  read.csv("data/ForC_plots.csv", stringsAsFactors = F) # this is just to steel the column names from that table
