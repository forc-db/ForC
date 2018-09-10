######################################################
# Purpose: identify potential site duplicates in ForC
# Developped by: Valentine Herrmann - HerrmannV@si.edu in April 2018
# R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)
library(fields)


# Load data ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Find groups of potential duplicates ####

## list of columns to compare (beside lat and lon) - fields related to physical environment ####
# col.to.compare <- c("masl", "geography.notes", "mat", "min.temp", "max.temp", "map", "climate.notes", "soil.texture", "soil.classification", "soil.notes", "hydrology.notes", "site.notes")
col.to.compare <- c("site.ref", "loaded.from")

## Find groups of SITES that are within a 5 km buffer ####

### Set up distance threshold
threshold.in.km = 5  # 5 km

### distance matrix
dist.in.km.matrix <- rdist.earth(SITES[, c("lon", "lat")], miles = F, R = 6378.137)

### get the clusterID of sites
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)

table(clusters)
sum(table(clusters) > 1) # 307 clusters of more than one site

clusters_with_potential_duplicates <- names(table(clusters))[table(clusters) > 1]

## Within each cluster of potential duplicates, compare the fields related to physical emnvironement. When missing, consider as potential duplicates. 

SITES$potential_duplicate_group <- 0
SITES$potential_duplicate_group_parsed <- 0

list.all.duplicate.groupings <- list()


for(cluster in clusters_with_potential_duplicates) {
  
  s <- SITES[clusters %in% cluster, ]
 
  group.c <- data.frame( cluster= rep(cluster, nrow(s)))
  
  for(c in col.to.compare) {
    
    # consider as potential duplicates if...
    ## all are NA, 
    ## none are NA and at least 2 different sources
    ## some are NA, consider as potential duplicates 
    
    if (all(my_is.na(s[, c]))) group.c[, c] <- rep(1, length(s[, c]))

    if((!any(my_is.na(s[, c])) & length(unique(s[, c])) > 1) | (!all(my_is.na(s[, c])) & any(my_is.na(s[, c])))) {
      
      group.c[, c] <- rep(0, length(s[, c]))
      
      duplicates <- s[, c][duplicated(s[, c])]
      duplicates <- duplicates[!my_is.na(duplicates)]
      
      if ( length(unique(duplicates)) == 1 ) {
        group.c[, c][s[, c] %in% duplicates] <- seq(s[, c][s[, c] %in% duplicates])
        group.c[, c][!s[, c] %in% duplicates] <- paste(seq(s[, c][s[, c] %in% duplicates]), collapse = ",")
      } else {
        group.c[, c] <- rep(1, length(s[, c]))
      }
    }
    
    # if all are not NA and are all the same, then they are not duplicates but we can still put them in a group (mostly to double ceck code)
    
    if((!any(my_is.na(s[, c])) & length(unique(s[, c])) == 1)) {
      group.c[, c] <- seq(length(s[, c]))
    }
     
  }

  list.all.duplicate.groupings[[which(clusters_with_potential_duplicates %in% cluster)]] <- group.c
  
  SITES$potential_duplicate_group[clusters %in% cluster] <- which(clusters_with_potential_duplicates %in% cluster) # create a "cluser ID"
  SITES$potential_duplicate_group_parsed[clusters %in% cluster] <- group.c[,1+ which.max(apply(group.c[,-1], 2, max))] # get the grouping that is the most "informed"
  
  if(length(SITES$potential_duplicate_group_parsed[clusters %in% cluster]) == length(unique(SITES$potential_duplicate_group_parsed[clusters %in% cluster]))) {
    SITES$potential_duplicate_group[clusters %in% cluster] <- 0
    SITES$potential_duplicate_group_parsed[clusters %in% cluster] <- 0
  }
}

write.csv(SITES, file = "data/ForC_sites.csv", row.names = F)
