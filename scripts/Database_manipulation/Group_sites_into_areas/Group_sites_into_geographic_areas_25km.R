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
 
# Load forC MEASUREMENTS table ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)


# Set up distance threshold ####
threshold.in.km = 25  # 25 km


# Get lat long of Sites ####

xy <- SpatialPointsDataFrame(SITES[, c("lon", "lat")], data.frame(sites.sitename = SITES$sites.sitename),
                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

plot(xy)

#distance matrix
dist.in.km.matrix <- rdist.earth(SITES[, c("lon", "lat")], miles = F, R = 6378.137)

#clustering
fit <- hclust(as.dist(dist.in.km.matrix), method = "single")
clusters <- cutree(fit,h = threshold.in.km)


table(clusters)


# Plot results
# for(i in unique(clusters)){
#   plot(xy, main = paste(i, " n.plot =", sum(clusters == i)))
#   points(xy[clusters == i,], col = "red" , pch=19)
# }


# Update area column
cbind(SITES$geographic.area, clusters)


SITES$geographic.area <- clusters

# double check some things


## check countries per cluser
A <- by(SITES$country, SITES$geographic.area, function(x) length(unique(x)))  
names(A[A != 1])
  
for(i in names(A[A != 1])){
  x <- SITES[SITES$geographic.area == i,]
  print(unique(x$country))
}



## check biogeog per cluser
A <- by(SITES$biogeog, SITES$geographic.area, function(x) length(unique(x)))  
names(A[A != 1])

for(i in names(A[A != 1])){
  x <- SITES[SITES$geographic.area == i,]
  print(unique(x$biogeog))
}

i = 352
plot(xy, main = paste(i, " n.plot =", sum(clusters == i)))
points(xy[clusters == i,], col = "red" , pch=19)



# SAVE OUTPUT ####

write.csv(SITES, "data/ForC_sites.csv", row.names = F)
