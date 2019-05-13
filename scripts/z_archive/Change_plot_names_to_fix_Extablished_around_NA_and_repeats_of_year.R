######################################################
# Purpose: Replace " Established around NA" and repeated "Stand established around [YEAR]" by "" in plot.names.
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)


# Find and change plot.name ####
old.plot.name <- PLOTS$plot.name
new.plot.name <- old.plot.name

## find and change ####
## " Established around NA" ####
found.plot.name <- new.plot.name[grepl(" Established around NA", new.plot.name, perl = T)]



## "Stand established around [YEAR]. Established around [same YEAR] ####
found.plot.name <- new.plot.name[grepl("Stand established around [0-9]{4}. Established around [0-9]{4}", new.plot.name)]

all(sapply(regmatches(found.plot.name, gregexpr("[0-9]{4}", found.plot.name)), function(x) x[1]==x[2])) # double check years are both same - should be TRUE!

new.plot.name <- ifelse(new.plot.name %in% found.plot.name, gsub(" Established around [0-9]{4}", "", new.plot.name, perl = T), new.plot.name)





##  Keep a table of change ####
compare.plot.name <- data.frame(old.plot.name, new.plot.name, stringsAsFactors = F)
plot.name.change <- compare.plot.name[apply(compare.plot.name, 1, function(x) !x[1] %in% x[2]),]



# change plot.name in all tables ####

## plot.name ####
### PLOTS ####
m <- match(PLOTS$plot.name, plot.name.change$old.plot.name)
old.plot.name <- PLOTS$plot.name
PLOTS$plot.name <- ifelse(is.na(m), PLOTS$plot.name, plot.name.change$new.plot.name[m])
### verify
compare.plot.name <- data.frame(old.plot.name, PLOTS$plot.name)
compare.plot.name[apply(compare.plot.name, 1, function(x) !x[1] %in% x[2]),]

### HISTORY ####
m <- match(HISTORY$plot.name, plot.name.change$old.plot.name)
old.plot.name <- HISTORY$plot.name
HISTORY$plot.name <- ifelse(is.na(m), HISTORY$plot.name, plot.name.change$new.plot.name[m])
### verify
compare.plot.name <- data.frame(old.plot.name, HISTORY$plot.name)
compare.plot.name[apply(compare.plot.name, 1, function(x) !x[1] %in% x[2]),]

### MEASUREMENTS ####
m <- match(MEASUREMENTS$plot.name, plot.name.change$old.plot.name)
old.plot.name <- MEASUREMENTS$plot.name
MEASUREMENTS$plot.name <- ifelse(is.na(m), MEASUREMENTS$plot.name, plot.name.change$new.plot.name[m])
### verify
compare.plot.name <- data.frame(old.plot.name, MEASUREMENTS$plot.name)
compare.plot.name[apply(compare.plot.name, 1, function(x) !x[1] %in% x[2]),]


# SAVE ####
write.csv(SITES, "data/ForC_sites.csv", row.names = F)
write.csv(PLOTS, "data/ForC_plots.csv", row.names = F)
write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names = F)
write.csv(HISTORY, "data/ForC_history.csv", row.names = F)

