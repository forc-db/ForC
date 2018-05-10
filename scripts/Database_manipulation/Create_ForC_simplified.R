######################################################
# Purpose: Creates ForC_simplified as described here: https://github.com/forc-db/ForC/tree/master/ForC_simplified
# Inputs:
# - MEASUREMENTS table
# - SITES table
# - PLOTS table
# - HISTORY table
# - R code that resolves duplicate records "scripts/Database_manipulation/Reconcile_duplicated_records.R"
# - VARIABLES table (to know what variables are secondary and remove those)
# Outputs:
# - ForC_simplified table
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
PLOTS <- read.csv("data/ForC_plots.csv", stringsAsFactors = F)
HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)
VARIABLES <- read.csv("data/ForC_variables.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####

## MEASUREMENTS ####


### remove measurements that we don't trust

MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$flag.suspicious %in% 0, ] # keep only non-suspicious records
### Resolve duplicates #####

source("scripts/Database_manipulation/Reconcile_duplicated_records.R")


### Ignore secondary variables ####
secondary.variables <- VARIABLES[VARIABLES$variable.type %in% "secondary",]$variable.name
MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[!MEASUREMENTS_no_duplicates$variable.name %in% secondary.variables, ]

### Ignore NEE_cum_C, GPP_cum_C, and R_eco_cum_C ignored.####
MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[!MEASUREMENTS_no_duplicates$variable.name %in% c("NEE_cum_C", "GPP_cum_C", "R_eco_cum_C"), ]


### Re-organize table ####
measurements.columns.to.keep <- c("measurement.ID", "sites.sitename", "plot.name", "stand.age", 
                                  "dominant.life.form", "dominant.veg", "variable.name", "date", 
                                  "start.date", "end.date", "mean", "min.dbh", "citation.ID")
names(MEASUREMENTS_no_duplicates)

MEASUREMENTS_no_duplicates <- MEASUREMENTS_no_duplicates[, measurements.columns.to.keep]
str(MEASUREMENTS_no_duplicates)

### Convert all measurements to units of C (use IPCC default C=0.47*biomass) + rename variables ####
units <- sapply(strsplit(MEASUREMENTS_no_duplicates$variable.name, "_"), tail, 1)

MEASUREMENTS_no_duplicates$mean <- ifelse(units %in% "OM", 0.47*MEASUREMENTS_no_duplicates$mean, MEASUREMENTS_no_duplicates$mean)
MEASUREMENTS_no_duplicates$variable.name <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS_no_duplicates$variable, perl = T)




## SITES ####

sites.columns.to.keep <- c("sites.sitename", "country", "lat", "lon", "masl", "mat", "map", 
                           "geographic.area", "biogeog", "Koeppen", "FAO.ecozone")
names(SITES)

SITES_simplified <- SITES[, sites.columns.to.keep]
str(SITES_simplified)

### masl ####
SITES_simplified$masl <- ifelse(my_is.na(SITES_simplified$masl), NA, SITES_simplified$masl)
SITES_simplified$masl <- as.numeric(SITES_simplified$masl) ## If you get an error here, that means that there is range values that need to be averaged

### mat ####
SITES_simplified$mat <- ifelse(my_is.na(SITES_simplified$mat), NA, SITES_simplified$mat)
SITES_simplified$mat <- as.numeric(SITES_simplified$mat) ## If you get an error here, that means that there is range values that need to be averaged

### map ####
SITES_simplified$map <- ifelse(my_is.na(SITES_simplified$map), NA, SITES_simplified$map)
SITES_simplified$map <- as.numeric(SITES_simplified$map) ## If you get an error here, that means that there is range values that need to be averaged



## PLOTS ####

plots.columns.to.keep <- c("sites.sitename", "plot.name", "plot.area", "year.establishment.oldest.trees", 
                           "regrowth.hist.type", "regrowth.year", "dist.mrs.hist.type", 
                           "dist.mrs.yr")
names(PLOTS)

PLOTS_simplified <- PLOTS[, plots.columns.to.keep]
str(PLOTS_simplified)



# MERGE ALL TABLES ####
MEASUREMENTS_SITES <- merge(MEASUREMENTS_no_duplicates, SITES_simplified, by = "sites.sitename", all.x = T)
MEASUREMENTS_SITES_PLOTS <- merge(MEASUREMENTS_SITES, PLOTS_simplified, by = c("sites.sitename", "plot.name"), all.x = T)

ForC_simplified <- MEASUREMENTS_SITES_PLOTS

# Add managed and disturbed columns ####

## Managed ####

# 1. Find plots with management record (recorded in HISTORY but as summarized in PLOTS) -- i.e., anything with management.[XXX].ID not equal to zero

plots.management.columns <- names(PLOTS)[grepl("management", names(PLOTS)) & !names(PLOTS) %in% "management.other.ID"]
managed <- PLOTS[, c("sites.sitename", "plot.name"),][apply(PLOTS[, plots.management.columns], 1, function(x) any(x!=0)),]
unmanaged.for.now <- PLOTS[, c("sites.sitename", "plot.name"),][apply(PLOTS[, plots.management.columns], 1, function(x) all(x==0)),] # calling "for now because ste 2below is done on those to further filter out)

if((nrow(managed) + nrow(unmanaged.for.now)) != nrow(PLOTS)) {stop("Problem when ID-ing the managed sites. unmanaged + managed does not equal total of sites")}

managed.1 <- paste(managed[,1], managed[,2])
unmanaged.for.now <- paste(unmanaged.for.now[,1], unmanaged.for.now[,2]) # calling "for now" because still need to be filtered by next step

# 2. Find any site.name or plot.name containing "plantation", "planted", "managed", "irrigated" or "fertilied"

managed.2 <- unmanaged.for.now[grepl("(plantation)|(planted)|(\\bmanaged)|(irrigated)|(fertilized)", unmanaged.for.now, perl = T, ignore.case = T)]

# 3. Give a 1 to all managed plots found in 1. and 2. 
all.managed.sites.plot.name <- c(managed.1, managed.2)
ForC_simplified$managed <- ifelse(paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name) %in% all.managed.sites.plot.name, 1, 0)

# 4. double check we got all
if(any(!c( paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name)[grepl("(plantation)|(planted)|(\\bmanaged)|(irrigated)|(fertilized)", paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name), perl = T, ignore.case = T)]) %in% all.managed.sites.plot.name)) { stop("Didn't get all managed sites")}# double check we got all either in managed.1 or managed.2... 


## disturbed ####

# 1. Find plots referencing dist_1.hist.type, dist_1.mort, dist_2.hist.type, dist_2.mort, and additional.dist.ID links to HISTORY table (just a handful of these)

## Find plots referencing dist_1.hist.type, dist_1.mort, dist_2.hist.type, dist_2.mort
plots.disturbance.columns <- c("dist_1.hist.type", "dist_1.mort", "dist_2.hist.type", "dist_2.mort")
disturbed <- PLOTS[apply(PLOTS[, plots.disturbance.columns], 1, function(x) any(!is.na(x))),]

## add plots for which additional.dist.ID links to HISTORY table
disturbed <- rbind(disturbed, PLOTS[!PLOTS$additional.dist.ID %in% 0,]) 

# 2. From the plots found in 1., remove the following:
# hist.type = Cut or Harvest - mort level "<<100%"
disturbed <- disturbed[!((grepl("Cut|Harvest", disturbed$dist_1.hist.type, perl = T) & grepl("<<100%", disturbed$dist_1.mort)) |
                           (grepl("Cut|Harvest", disturbed$dist_2.hist.type, perl = T) & grepl("<<100%", disturbed$dist_2.mort))), ]

# hist.type = Burned- mort level <= 10%
dist_1.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_1.mort))
dist_2.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_2.mort))

disturbed <- disturbed[!((grepl("Burned", disturbed$dist_1.hist.type, perl = T) & dist_1.mort.num <=  10 & !is.na(dist_1.mort.num)) |
                           (grepl("Burned", disturbed$dist_2.hist.type, perl = T) & dist_2.mort.num <=  10 & !is.na(dist_2.mort.num))), ]

# hist.type = Drought- exclude only if mort level has a specific value > 10%.
dist_1.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_1.mort))
dist_2.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_2.mort))

disturbed <- disturbed[!((grepl("Drought", disturbed$dist_1.hist.type, perl = T) & (dist_1.mort.num <=  10 | is.na(dist_1.mort.num))) |
                           (grepl("Drought", disturbed$dist_2.hist.type, perl = T) & (dist_2.mort.num <=  10 | is.na(dist_2.mort.num)))), ]


# hist.type = Flood- mort level <= 10%
dist_1.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_1.mort))
dist_2.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_2.mort))

disturbed <- disturbed[!((grepl("Flood", disturbed$dist_1.hist.type, perl = T) & dist_1.mort.num <=  10 & !is.na(dist_1.mort.num)) |
                           (grepl("Flood", disturbed$dist_2.hist.type, perl = T) & dist_2.mort.num <=  10 & !is.na(dist_2.mort.num))), ]

# hist.type = Major storm- mort level <= 10%
dist_1.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_1.mort))
dist_2.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_2.mort))

disturbed <- disturbed[!((grepl("Major storm", disturbed$dist_1.hist.type, perl = T) & dist_1.mort.num <=  10 & !is.na(dist_1.mort.num)) |
                           (grepl("Major storm", disturbed$dist_2.hist.type, perl = T) & dist_2.mort.num <=  10 & !is.na(dist_2.mort.num))), ]


# hist.type = Other- any mort level > 0%
dist_1.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_1.mort))
dist_2.mort.num <- as.numeric(gsub("[[:punct:]]", "", disturbed$dist_2.mort))

disturbed <- disturbed[!((grepl("Other", disturbed$dist_1.hist.type, perl = T) & dist_1.mort.num <=  0 & !is.na(dist_1.mort.num)) |
                           (grepl("Other", disturbed$dist_2.hist.type, perl = T) & dist_2.mort.num <=  0 & !is.na(dist_2.mort.num))), ]

# hist.type = Grazed only  - all records (Grazing wouldn't kill trees)
disturbed <- disturbed[!(grepl("Grazed", disturbed$dist_1.hist.type, perl = T) & (grepl("Grazed", disturbed$dist_2.hist.type, perl = T) | is.na(disturbed$dist_2.hist.type))), ]

# 3. verify
table(disturbed$dist_1.hist.type, disturbed$dist_1.mort)
table(disturbed$dist_2.hist.type, disturbed$dist_2.mort)

# 4. Give a 1 to all disturbed plots
ForC_simplified$disturbed <- ifelse(paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name) %in% paste(disturbed$sites.sitename, disturbed$plot.name), 1, 0)


# Add history_info column ####

# Finds plots that have "No.info" in hist.cat
head(HISTORY)
no.hist.info <- HISTORY[HISTORY$hist.cat %in% "No.info",]
ForC_simplified$history.no.info <- ifelse(paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name) %in% paste(no.hist.info$sites.sitename, no.hist.info$plot.name), 1, 0)




# Order columns ####
ordered.field <- c("measurement.ID", "sites.sitename", "plot.name", 
                  "stand.age", "dominant.life.form", "dominant.veg", "variable.name", 
                  "date", "start.date", "end.date", "mean", "min.dbh", "citation.ID",
                  "country", "lat", "lon", "masl", "mat", "map", "geographic.area", 
                  "biogeog", "Koeppen", "FAO.ecozone",
                  "plot.area", "year.establishment.oldest.trees", 
                  "regrowth.hist.type", "regrowth.year", "dist.mrs.hist.type", 
                  "dist.mrs.yr", "managed", "disturbed", "history.no.info")

ForC_simplified <- ForC_simplified[, ordered.field]

# order records ####

ForC_simplified <- ForC_simplified[order(ForC_simplified$measurement.ID),]

# Save ForC-simplified ####

write.csv(ForC_simplified, file = "ForC_simplified/ForC_simplified.csv", row.names = F)

# A <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
# B <- ForC_simplified
# 
# for(i in 1:nrow(A)){
#  print(i)
#   if(!sum(A[i,] %in% B[i,]) >= ncol(A) -1) stop("problem" )
#   if(!A[i,]$mean - B[i,]$mean < 0.0001) stop("problem" )
# }
# 
# A[1,]$mean - B[1,]$mean
# identical(A[1,], B[1,],  num.eq = F, single.NA = F, attrib.as.set = F,
#           ignore.bytecode = F, ignore.environment = F,
#           ignore.srcref = F)
