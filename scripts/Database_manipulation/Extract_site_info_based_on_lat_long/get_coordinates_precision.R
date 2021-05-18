######################################################
# Purpose: Find out coordintate precision of SITES 
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####
library(rgdal)

# Load tables ####
SITES <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_sites.csv", stringsAsFactors = F)


# fill info about coordinate precisiion ####
# in coordinates.precision , enter the precision reported in the original pub (which should now match what's in ForC). Please enter exactly one of the following:
# degree- rounded to nearest degree or rough fraction of a degree (e.g., .167, .25, .33, .5, .67, .75, .83, or to just one decimal point);



## look at the SITES where precision is NAC
A <- SITES[, c("lat", "lon")]
B <- strsplit(as.character(abs(A$lat)), "\\.")

A$dec_lat <-  sapply(strsplit(as.character(abs(A$lat)), "\\."), function(x) ifelse(length(x)>1, x[2], ""))
A$dec_lon <-  sapply(strsplit(as.character(abs(A$lon)), "\\."), function(x) ifelse(length(x)>1, x[2], ""))

A$ndec_lat <- nchar(A$dec_lat)
A$ndec_lon <-  nchar(A$dec_lon)

C <- A[!duplicated(A[,c("ndec_lat", "ndec_lon")]),]
C$lat <- as.character(C$lat)
C$lon <- as.character(C$lon)
C[order(C$ndec_lat, decreasing = T),]

# if min(ndec_lat, ndec_lon) <= 1

# DEGREE
pattern = "^25$|^5$|^75$"
idx_degree <- (A$ndec_lat == 0 | grepl(pattern, A$dec_lat)) &  (A$ndec_lon  == 0  | grepl(pattern, A$dec_lon)) #  apply(A, 1 , function(x) all(c(x["ndec_lat"], x["ndec_lon"]) == 0)) | (grepl(pattern, A$dec_lat) & grepl(pattern, A$dec_lon))

# View(SITES[SITES$coordinates.precision %in% "NAC" & !is.na(SITES$lat) & !is.na(SITES$lon) & idx_degree,])

SITES$coordinates.precision[SITES$coordinates.precision %in% "NAC" & !is.na(SITES$lat) & !is.na(SITES$lon) & idx_degree] <- "(degree)"



# minutes rounded
pattern = "^25$|^75$|^167+$|^33+$|^67+$|^83+$"
idx_minutes <-  (A$ndec_lat <=1 | grepl(pattern, A$dec_lat)) &  (A$ndec_lon <= 1 | grepl(pattern, A$dec_lon)) # apply(A, 1 , function(x) all(c(x["ndec_lat"], x["ndec_lon"]) <=1))  | (grepl(pattern, A$dec_lat) &  grepl(pattern, A$dec_lon))

# View(SITES[SITES$coordinates.precision %in% "NAC" & !is.na(SITES$lat) & !is.na(SITES$lon) & (!idx_degree & idx_minutes),])

SITES$coordinates.precision[SITES$coordinates.precision %in% "NAC" & !is.na(SITES$lat) & !is.na(SITES$lon) & (!idx_degree & idx_minutes)] <- "(minutes rounded)"

# save ####
SITES$climate.data.suspect[is.na(SITES$climate.data.suspect)]<-""
SITES$X[is.na(SITES$X)]<-""

write.csv(SITES, "data/ForC_sites.csv", row.names = F)

