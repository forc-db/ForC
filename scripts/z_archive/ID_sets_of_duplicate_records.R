######################################################
# Purpose: Create the dup.num field of MEASUREMENTS table to make it easier to Id the duplicates sets that we should look at to reconcile duplicates records. Krista went through to give higher score to duplicates that are using different IDS, and equal score for actual replicates that should be averaged. This code simply adds an ID in front of the scrore so that later on, it is easy to pick or average records within each sets of replicates.
# Inputs: - MEASUREMENTS table
# outputs: - MEASUREMENTS table with updated dup.num column.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-29)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(readr)


# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)


# Prepare data ####

## Fill in date with start date if needed
MEASUREMENTS$date[is.na(MEASUREMENTS$date)] <- MEASUREMENTS$start.date[is.na(MEASUREMENTS$date)]

## Take integer part of dat only

MEASUREMENTS$date <- floor(as.numeric(MEASUREMENTS$date))

## Make NA an actual factor level for date and stand age
MEASUREMENTS$date <- addNA(MEASUREMENTS$date)
MEASUREMENTS$stand.age <- addNA(MEASUREMENTS$stand.age)

## Ignore _OM and _Oc in variables names
MEASUREMENTS$variable.name.combined <- MEASUREMENTS$variable.name
MEASUREMENTS$variable.name.combined <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS$variable.name.combined, perl = T)


# subset to keep only records with a dup.code other than 0 and that don't have the dup.num already set by krista (a dup.num should be madee of an interger and a digit other than 0. so if dup num is of format 1.0 we know it has not been set yet).


dup.rec <-droplevels(MEASUREMENTS[(!MEASUREMENTS$dup.code %in% "0") & (MEASUREMENTS$dup.num %% 1)*10 == 0,])

unique(dup.rec$dup.num) # should be only integers

# Split the duplicates records in the sets of duplicates ####

## 1- Keep records with a dup.code only
### 2- Working one sites.sitename/plot.name at a time, average by stand age



dup.rec.split <- split(dup.rec, list(dup.rec$sites.sitename, dup.rec$plot.name, dup.rec$variable.name.combined, dup.rec$date, dup.rec$stand.age), drop = TRUE)
table(do.call(rbind, lapply(dup.rec.split, dim)))

dup.rec.unsplit <- do.call(rbind, dup.rec.split)

if(!nrow(do.call(rbind, dup.rec.split)) == nrow(dup.rec)) stop("Error: problem in the way the data is split. need to remove one factor but then we don't have the right set of replicates... need to find a solution ") # if false, problem.... need to remove date but then we don't have the right set of replicates...

dup.rec[!dup.rec$measurement.ID %in% dup.rec.unsplit$measurement.ID,] # should be empty

for(i in sort(names(dup.rec.split)[lapply(dup.rec.split, nrow) == 1])){
  cat(i)
  cat("\n")
}

new.dup.rec.records <- NULL

for(i in 1:length(dup.rec.split)){
  
  x <- dup.rec.split[[i]]
  
  if(nrow(x) <= 1){
    warning("Error: only one row for this duplicate record (so not duplicated ?)", immediate. = T)
    print(x)
    readline("Press [eneter] to continue")
  }
  
    
}

  
  if(nrow(new.x) != 1) stop("Error: nrow(new.x)  is not equal to 1")
  
  new.dup.rec.records <- rbind(new.dup.rec.records, new.x)
  print(paste(i, nrow(new.dup.rec.records)))
}

}