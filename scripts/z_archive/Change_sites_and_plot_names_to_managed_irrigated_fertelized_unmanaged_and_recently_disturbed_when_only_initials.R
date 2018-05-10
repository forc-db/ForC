######################################################
# Purpose: Replace "M", "I", "F" and "IF" by corresponding management words "managed", "irrigated", "fertilized' and "irrigated/fertilized"
# Inputs:
# - MEASUREMENTS table
# - SITES table
# - PLOTS table
# - HISTORY table
# Outputs:
# - updated tables
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

# Find and change sites.sitename ####
old.sites.sitename <- SITES$sites.sitename
new.sites.sitename <- old.sites.sitename

## find and change ####

## M for managed ####
managed.sites.sitename <- new.sites.sitename[grepl("(\\bM\\b)(?=$)|(\\bM\\b)(?= )", new.sites.sitename, perl = T)]

new.sites.sitename <- ifelse(new.sites.sitename %in% managed.sites.sitename, gsub("(\\bM\\b)(?=$)|(\\bM\\b)(?= )", "managed", new.sites.sitename, perl = T), new.sites.sitename)

## I for irrigated (carefull, do not convert when it is class I, chronosequence I, Bosque I, FACTS, Observational) ####
irrigated.sites.sitename <- new.sites.sitename[grepl("(\\bI\\b)(?=$)|(\\bI\\b)(?= )|(\\bI\\b)(?=\\+)", new.sites.sitename, perl = T)]
irrigated.sites.sitename <- irrigated.sites.sitename[!grepl("Class|Bosque|chronosequence|FACTS|Observational", irrigated.sites.sitename)]

new.sites.sitename <- ifelse(new.sites.sitename %in% irrigated.sites.sitename, gsub("(\\bI\\b)(?=$)|(\\bI\\b)(?= )|(\\bI\\b)(?=\\+)", "irrigated", new.sites.sitename, perl = T), new.sites.sitename)

## F for fertilized ####
fertilized.sites.sitename <- new.sites.sitename[grepl("(\\bF\\b)(?=$)|(\\bF\\b)(?= )|(\\bF\\b)(?=\\+)", new.sites.sitename, perl = T)]

new.sites.sitename <- ifelse(new.sites.sitename %in% fertilized.sites.sitename, gsub("(\\bF\\b)(?=$)|(\\bF\\b)(?= )|(\\bF\\b)(?=\\+)", "fertilized", new.sites.sitename, perl = T), new.sites.sitename)




## Keep a table of change ####
compare.sites.name <- data.frame(old.sites.sitename, new.sites.sitename, stringsAsFactors = F)
sites.name.change <- compare.sites.name[apply(compare.sites.name, 1, function(x) !x[1] %in% x[2]),]


# Find and change plot.name ####
old.plot.name <- PLOTS$plot.name
new.plot.name <- old.plot.name

## find and change ####
## M for managed ####
managed.plot.name <- new.plot.name[grepl("(\\bM\\b)(?=$)|(\\bM\\b)(?= )", new.plot.name, perl = T)]

new.plot.name <- ifelse(new.plot.name %in% managed.plot.name, gsub("(\\bM\\b)(?=$)|(\\bM\\b)(?= )", "managed", new.plot.name, perl = T), new.plot.name)

## I for irrigated (carefull, do not convert when it is class I, chronosequence I, Bosque I, FACTS, Observational) ####
irrigated.plot.name <- new.plot.name[grepl("(\\bI\\b)(?=$)|(\\bI\\b)(?= )|(\\bI\\b)(?=\\+)", new.plot.name, perl = T)]
irrigated.plot.name <- irrigated.plot.name[!grepl("Class|Bosque|chronosequence|FACTS|Observational", irrigated.plot.name)]

new.plot.name <- ifelse(new.plot.name %in% irrigated.plot.name, gsub("(\\bI\\b)(?=$)|(\\bI\\b)(?= )|(\\bI\\b)(?=\\+)", "irrigated", new.plot.name, perl = T), new.plot.name)

## F for fertilized ####
fertilized.plot.name <- new.plot.name[grepl("(\\bF\\b)(?=$)|(\\bF\\b)(?= )|(\\bF\\b)(?=\\+)", new.plot.name, perl = T)]

new.plot.name <- ifelse(new.plot.name %in% fertilized.plot.name, gsub("(\\bF\\b)(?=$)|(\\bF\\b)(?= )|(\\bF\\b)(?=\\+)", "fertilized", new.plot.name, perl = T), new.plot.name)


## UM for unmanaged ####
unmanaged.plot.name <- new.plot.name[grepl("(\\bUM\\b)", new.plot.name, perl = T)]

new.plot.name <- ifelse(new.plot.name %in% unmanaged.plot.name, gsub("(\\bUM\\b)", "unmanaged", new.plot.name, perl = T), new.plot.name)

## UM for recently disturbed ####
recently.disturbed.plot.name <- new.plot.name[grepl("(\\bRD\\b)", new.plot.name, perl = T)]

new.plot.name <- ifelse(new.plot.name %in% recently.disturbed.plot.name, gsub("(\\bRD\\b)", "recently disturbed", new.plot.name, perl = T), new.plot.name)



##  Keep a table of change ####
compare.plot.name <- data.frame(old.plot.name, new.plot.name, stringsAsFactors = F)
plot.name.change <- compare.plot.name[apply(compare.plot.name, 1, function(x) !x[1] %in% x[2]),]



# change sites.sitename and plot.name in all tables ####

## sites.sitename ####
### SITES ####
m <- match(SITES$sites.sitename, sites.name.change$old.sites.sitename)
old.sites.sitename <- SITES$sites.sitename
SITES$sites.sitename <- ifelse(is.na(m), SITES$sites.sitename,sites.name.change$new.sites.sitename[m])
### verify
compare.sites.name <- data.frame(old.sites.sitename, SITES$sites.sitename)
compare.sites.name[apply(compare.sites.name, 1, function(x) !x[1] %in% x[2]),]

### PLOTS ####
m <- match(PLOTS$sites.sitename, sites.name.change$old.sites.sitename)
old.sites.sitename <- PLOTS$sites.sitename
PLOTS$sites.sitename <- ifelse(is.na(m), PLOTS$sites.sitename,sites.name.change$new.sites.sitename[m])
### verify
compare.sites.name <- data.frame(old.sites.sitename, PLOTS$sites.sitename)
compare.sites.name[apply(compare.sites.name, 1, function(x) !x[1] %in% x[2]),]

### HISTORY ####
m <- match(HISTORY$sites.sitename, sites.name.change$old.sites.sitename)
old.sites.sitename <- HISTORY$sites.sitename
HISTORY$sites.sitename <- ifelse(is.na(m), HISTORY$sites.sitename,sites.name.change$new.sites.sitename[m])
### verify
compare.sites.name <- data.frame(old.sites.sitename, HISTORY$sites.sitename)
compare.sites.name[apply(compare.sites.name, 1, function(x) !x[1] %in% x[2]),]

### MEASUREMENTS ####
m <- match(MEASUREMENTS$sites.sitename, sites.name.change$old.sites.sitename)
old.sites.sitename <- MEASUREMENTS$sites.sitename
MEASUREMENTS$sites.sitename <- ifelse(is.na(m), MEASUREMENTS$sites.sitename,sites.name.change$new.sites.sitename[m])
### verify
compare.sites.name <- data.frame(old.sites.sitename, MEASUREMENTS$sites.sitename)
compare.sites.name[apply(compare.sites.name, 1, function(x) !x[1] %in% x[2]),]

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

