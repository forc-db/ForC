#adapted from http://spatialanalysis.co.uk/wp-content/uploads/2012/07/exporting-to-kml.txt
library(sp)
library(maptools)
library(rgdal)
setwd()
ForC_sites <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/ForC/data/ForC_sites.csv")
coordinates(ForC_sites)<- c("lon", "lat")
BNG<- CRS("+init=epsg:4326") #reference from http://spatialreference.org/ref/epsg/4326/
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(ForC_sites)<-BNG
ForC_wgs84<- spTransform(ForC_sites, CRS=p4s)
writeOGR(ForC_wgs84, dsn="ForC_sites_GoogleEarth.kml", layer= "ForC_sites", driver="KML", dataset_options=c("NameField=name", all=TRUE))
