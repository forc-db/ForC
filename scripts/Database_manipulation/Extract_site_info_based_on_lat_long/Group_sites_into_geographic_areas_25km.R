######################################################
# Purpose: Group sites geographically based on their coordinates following the rule that sites with <25km  between them are in the same group
# Inputs: - ForC SITES table
# outputs: - ForC SITES table with updated area column
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2017-12/11)
######################################################


# Clean environment ####
rm(list = ls())

# Setup working directory ####
setwd(".")

# Load libraries ####
library(fields)
library(sp)

# Load forC MEASUREMENTS table ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)


# Set up distance threshold ####
threshold.in.km = 25  # 25 km


# Get lat long of Sites ####
idx.non.missing.coordinates <- apply(SITES[, c("lon", "lat")], 1, function(x) !any(is.na(x)))

xy <- SpatialPointsDataFrame(SITES[idx.non.missing.coordinates, c("lon", "lat")], data.frame(sites.sitename = SITES[idx.non.missing.coordinates, ]$sites.sitename),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

plot(xy)

#distance matrix
dist.in.km.matrix <- rdist.earth(SITES[idx.non.missing.coordinates, c("lon", "lat")], miles = F, R = 6378.137)

#clustering
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)


table(clusters)


# Plot results
# for(i in unique(clusters)){
#   plot(xy, main = paste(i, " n.plot =", sum(clusters == i)))
#   points(xy[clusters == i,], col = "red" , pch=19)
# }

plot(xy, col = rainbow(length(unique(clusters)))[clusters])


# See what changed (sometimes old geographic.area get changed... I think we just go with it)
cbind(SITES[idx.non.missing.coordinates, ]$sites.sitename, SITES[idx.non.missing.coordinates, ]$geographic.area, clusters)[apply(cbind(SITES[idx.non.missing.coordinates, ]$geographic.area, clusters), 1, function(x) x[1] != x[2]), ] # show the ones that changed... but we want to try to keep the same numbers...

site.ID_by_old_cluster <- split(SITES[idx.non.missing.coordinates, ]$site.ID, SITES[idx.non.missing.coordinates, ]$geographic.area)
site.ID_by_new_cluster <- split(SITES[idx.non.missing.coordinates, ]$site.ID, clusters)

names(site.ID_by_old_cluster)[!(names(site.ID_by_old_cluster) %in% names(site.ID_by_new_cluster))] # should only be NAC

# Update geographic.area column
SITES[idx.non.missing.coordinates, ]$geographic.area <- clusters

# double check some things


## check countries per cluser
A <- by(SITES[idx.non.missing.coordinates, ]$country, SITES[idx.non.missing.coordinates, ]$geographic.area, function(x) length(unique(x)))  
names(A[A != 1])
  
for(i in names(A[A != 1])){
  x <- SITES[SITES$geographic.area == i,]
  print(unique(x$country))
  plot(xy, main = paste("geographic areas in", unique(x$country)))
  points(xy[SITES[idx.non.missing.coordinates, ]$geographic.area == i,], col = rainbow(length(unique(clusters)))[clusters[SITES[idx.non.missing.coordinates, ]$geographic.area == i]], pch = 16)
}



## check biogeog per cluser
A <- by(SITES$biogeog, SITES$geographic.area, function(x) length(unique(x)))  
names(A[A != 1])

for(i in names(A[A != 1])){
  x <- SITES[SITES$geographic.area == i,]
  print(unique(x$biogeog))
}

i = 852
plot(xy, main = paste(i, " n.plot =", sum(clusters == i)))
points(xy[clusters == i,], col = "red" , pch=19)



# SAVE OUTPUT ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)
