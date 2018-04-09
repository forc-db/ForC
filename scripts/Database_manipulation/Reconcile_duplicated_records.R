######################################################
# Purpose: Select records of ForC among duplicates.
#           - Replicate measurements (‘R’ code) should be averaged.
#           - For duplicate records we take only those that have D.precendence = 1
#           - For subdubed, take only the one that have a small s (not capital S)
# Inputs:
# - MEASUREMENTS table (NOTE: we are not actually loading this table in this script because this script is meant to be sourced from the Create_ForC_simplified.R script which is where the table is loaded )
# outputs:
# - MEASUREMENTS table with only one record selected amoung sets of duplicates and one averaged record for sets of Duplicates.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-13)
######################################################


# Clean environment ####
# rm(list = ls()) # Here wee don't clean the environment because this scrip is meant to be sourced fonr another script

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)

# Load data ####
# MEASUREMENTS <- read.csv("data/ForC_measurements.csv") # we are not actually loading this table in this script because this script is meant to be sourced from the Create_ForC_simplified.R script which is where the table is loaded )

# Select the records we want to keep or average ####

Independents <- MEASUREMENTS[MEASUREMENTS$conflicts %in% "I",] # take only independent records
Replicates  <- MEASUREMENTS[grepl("R", MEASUREMENTS$conflicts) & (is.na(MEASUREMENTS$D.precedence) | MEASUREMENTS$D.precedence %in% 1),] # take replicates. Take only if precedence is 1 or NA, that will elimate if they are a duplicate of a D.group record we don't want (which has 0 for D.precedence)
Duplicates <- MEASUREMENTS[(grepl("D", MEASUREMENTS$conflicts) ) & !(grepl("R", MEASUREMENTS$conflicts) ) &  (MEASUREMENTS$D.precedence %in% 1),] # take duplicates that are not replicates (we already have them in Replicates) and that have a one in D.precedence.
subdubed_only <- MEASUREMENTS[grepl("s", MEASUREMENTS$conflicts, ignore.case = F) & !(grepl("R", MEASUREMENTS$conflicts) ) & (is.na(MEASUREMENTS$D.precedence)),] # take subdubed records that are not Replicates nore duplicates (since we already have them in previous objects)

## double check we got what we wanted
table(apply(Independents[c("conflicts", "D.precedence")], 1, paste, collapse = " "))
table(apply(Replicates[c("conflicts", "D.precedence")], 1, paste, collapse = " "))
table(apply(Duplicates[c("conflicts", "D.precedence")], 1, paste, collapse = " "))
table(apply(subdubed_only[c("conflicts", "D.precedence")], 1, paste, collapse = " "))

# double check all ID-s are in there and if not they have a zeroe in D.precedence or a cap S in conflicts
sum(duplicated(rbind(Independents, Replicates, Duplicates, subdubed_only)$measurement.ID))

A <- MEASUREMENTS$measurement.ID
B <- rbind(Independents, Replicates, Duplicates, subdubed_only)$measurement.ID

all(B %in% A) # TRUE, makes sense
all(A %in% B) # should be FASLE

C <- A[!A %in% B] # missing IDs

unique(MEASUREMENTS[MEASUREMENTS$measurement.ID %in% C, ]$D.precedence) # should only be 0, if NA, conflict should have a cap S
unique(MEASUREMENTS[MEASUREMENTS$measurement.ID %in% C & is.na(MEASUREMENTS$D.precedence), ]$conflicts)  # shold only be S
unique(MEASUREMENTS[MEASUREMENTS$measurement.ID %in% C, ]$conflicts) # should not be any I

### --> All  is good !



# average the records that need to be averaged ####
Replicates.split <- split(Replicates, Replicates$R.group, drop = TRUE)

Replicates.final <- NULL


for(i in 1:length(Replicates.split)){
  
  X <- Replicates.split[[i]]
  
  
  if(!length(unique(X$variable.name)) %in% 1) problem.grouping <- c(problem.grouping, i)
  
  x <- X[1,] # keep first row
  x$mean <- mean(X$mean) # take average of all rows
  
  Replicates.final <- rbind(Replicates.final, x)
  
}

nrow(Replicates.final) == length(Replicates.split) # should be TRUE


# Combine all what we keep ####


MEASUREMENTS_no_duplicates <- rbind(Independents, Replicates.final, Duplicates, subdubed_only)


## double check we don't have any duplicated left ####

# ## Ignore _OM and _Oc in variables names ####
# MEASUREMENTS_no_duplicates$variable.name.combined <- MEASUREMENTS_no_duplicates$variable.name
# MEASUREMENTS_no_duplicates$variable.name.combined <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS_no_duplicates$variable.name.combined, perl = T)
# 
# ## Create a better date column ####
# MEASUREMENTS_no_duplicates$my.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS_no_duplicates$date)))
# MEASUREMENTS_no_duplicates$my.start.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS_no_duplicates$start.date)))
# MEASUREMENTS_no_duplicates$my.end.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS_no_duplicates$end.date)))
# 
# MEASUREMENTS_no_duplicates$my.date <- ifelse(!is.na(MEASUREMENTS_no_duplicates$my.start.date) & !is.na(MEASUREMENTS_no_duplicates$my.end.date), NA,  MEASUREMENTS_no_duplicates$my.date)
# class(MEASUREMENTS_no_duplicates$my.date) <- "Date"
# 
# ### take floor of all dates
# MEASUREMENTS_no_duplicates$my.date <- year(MEASUREMENTS_no_duplicates$my.date)
# MEASUREMENTS_no_duplicates$my.date <- ifelse(is.na(MEASUREMENTS_no_duplicates$my.date), NA, paste0(MEASUREMENTS_no_duplicates$my.date, "-01-01"))
# MEASUREMENTS_no_duplicates$my.date <- as.Date(MEASUREMENTS_no_duplicates$my.date)
# 
# MEASUREMENTS_no_duplicates$my.start.date <- year(MEASUREMENTS_no_duplicates$my.start.date)
# MEASUREMENTS_no_duplicates$my.start.date <- ifelse(is.na(MEASUREMENTS_no_duplicates$my.start.date), NA, paste0(MEASUREMENTS_no_duplicates$my.start.date, "-01-01"))
# MEASUREMENTS_no_duplicates$my.start.date <- as.Date(MEASUREMENTS_no_duplicates$my.start.date)
# 
# MEASUREMENTS_no_duplicates$my.end.date <- year(MEASUREMENTS_no_duplicates$my.end.date)
# MEASUREMENTS_no_duplicates$my.end.date <- ifelse(is.na(MEASUREMENTS_no_duplicates$my.end.date), NA, paste0(MEASUREMENTS_no_duplicates$my.end.date, "-01-01"))
# MEASUREMENTS_no_duplicates$my.end.date <- as.Date(MEASUREMENTS_no_duplicates$my.end.date)
# 
# 
# ## consider NA as a factor level for plot.names (otherwise doesn't work when splitting the data) ####
# MEASUREMENTS_no_duplicates$plot.name <- addNA(MEASUREMENTS_no_duplicates$plot.name)
# MEASUREMENTS_no_duplicates$my.date <- addNA(MEASUREMENTS_no_duplicates$my.date)
# MEASUREMENTS_no_duplicates$my.start.date <- addNA(MEASUREMENTS_no_duplicates$my.start.date)
# MEASUREMENTS_no_duplicates$stand.age <- addNA(MEASUREMENTS_no_duplicates$stand.age)
# 
# 
# 
# 
# # SPlit Measurement by site plot and variables name ####
# 
# MEASUREMENTS_no_duplicates.split <- split(MEASUREMENTS_no_duplicates, list(MEASUREMENTS_no_duplicates$sites.sitename, MEASUREMENTS_no_duplicates$plot.name, MEASUREMENTS_no_duplicates$variable.name.combined, MEASUREMENTS_no_duplicates$my.date, MEASUREMENTS_no_duplicates$my.start.date, MEASUREMENTS_no_duplicates$stand.age), drop = TRUE)
# 
# 
# length(MEASUREMENTS_no_duplicates.split)
# nrow(MEASUREMENTS_no_duplicates)
# 
# which(sapply(MEASUREMENTS_no_duplicates.split, nrow) >1)
# MEASUREMENTS_no_duplicates.split$'Daxing Forest Farm.poplar plantation.GPP_cum.NA.2007-01-01.9'
# # --> those 2 records are Ok, their date (not floored) actually don't overlap  so they ARE independent

