# temporarily_deal_with_SRDB_issue (and GROA)
## see issue https://github.com/forc-db/ERL-review/issues/34

# clear environment ###
rm(list = ls())

# load libraries ####
library(fields)
library(sp)

# load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

## group sites into 0.1 degree lat-lon (around 11 kilometers) areas ####
### Set up distance threshold 
threshold.in.km = 11  # 11 km


### distance matrix by sites
dist.in.km.matrix <- rdist.earth(SITES[, c("lon", "lat")], miles = F, R = 6378.137)

### clustering
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)

### add into SITES
SITES$cluster <- clusters

### add into MEASUREMENTS
MEASUREMENTS$cluster <- SITES$cluster[match(MEASUREMENTS$sites.sitename, SITES$sites.sitename)]

## find out what variables are represented in both ForC and SRDB in the same cluster ####
SRDB_measID_to_flag <- NULL
SRDB_ForC_potential_conflicts <- NULL
for(c_v in unique(paste(MEASUREMENTS$cluster, MEASUREMENTS$variable.name))) {
  x <- MEASUREMENTS[paste(MEASUREMENTS$cluster, MEASUREMENTS$variable.name) %in% c_v, ]
  
  if(length(unique(x$ForC.investigator))>1 & any(x$ForC.investigator %in% "Ben Bond-Lamberty")) {
  
    SRDB_IDs <- x$measurement.ID[x$ForC.investigator %in% "Ben Bond-Lamberty"]
    ForC_IDs <- x$measurement.ID[!x$ForC.investigator %in% "Ben Bond-Lamberty"]
      
    SRDB_measID_to_flag <- c(SRDB_measID_to_flag, SRDB_IDs)
    SRDB_ForC_potential_conflicts <- rbind(SRDB_ForC_potential_conflicts, expand.grid(ForC_measurement.ID = ForC_IDs, SRDB_ForC_measurement.ID =  SRDB_IDs))
    
  }
  
}

length(SRDB_measID_to_flag)
table(MEASUREMENTS$ForC.investigator[MEASUREMENTS$measurement.ID %in% SRDB_measID_to_flag]) # should all be Ben's data


## find out what variables are represented in both ForC and GROA in the same cluster ####
GROA_measID_to_flag <- NULL
GROA_ForC_potential_conflicts <- NULL
for(c_v in unique(paste(MEASUREMENTS$cluster, MEASUREMENTS$variable.name))) {
  x <- MEASUREMENTS[paste(MEASUREMENTS$cluster, MEASUREMENTS$variable.name) %in% c_v, ]
  
  if(length(unique(x$ForC.investigator))>1 & any(x$ForC.investigator %in% "Dr. Susan Cook-Patton")) {
    
    GROA_IDs <- x$measurement.ID[x$ForC.investigator %in% "Dr. Susan Cook-Patton"]
    ForC_IDs <- x$measurement.ID[!x$ForC.investigator %in% "Dr. Susan Cook-Patton"]
    
    GROA_measID_to_flag <- c(GROA_measID_to_flag, GROA_IDs)
    GROA_ForC_potential_conflicts <- rbind(GROA_ForC_potential_conflicts, expand.grid(ForC_measurement.ID = ForC_IDs, GROA_ForC_measurement.ID =  GROA_IDs))
    
  }
  
}

length(GROA_measID_to_flag)
table(MEASUREMENTS$ForC.investigator[MEASUREMENTS$measurement.ID %in% GROA_measID_to_flag]) # should all be Suzanes's data

# save 2 tables ####
write.csv(SRDB_ForC_potential_conflicts, file = "database_management_records/SRDB_ForC_potential_conflicts.csv", row.names = F)
write.csv(GROA_ForC_potential_conflicts, file = "database_management_records/GROA_ForC_potential_conflicts.csv", row.names = F)
