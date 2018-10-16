#adapted from http://spatialanalysis.co.uk/wp-content/uploads/2012/07/exporting-to-kml.txt
library(sp)
library(maptools)
library(rgdal)
library(mapview)
setwd()
ForC_sites <- read.csv("C:/Users/mcgregori/Dropbox (Smithsonian)/Github_Ian/ForC/data/ForC_sites.csv")
coordinates(ForC_sites)<- c("lon", "lat")
BNG<- CRS("+init=epsg:4326") #reference from http://spatialreference.org/ref/epsg/4326/
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(ForC_sites)<-BNG
ForC_wgs84<- spTransform(ForC_sites, CRS=p4s)
mapview(ForC_wgs84, label=ForC_sites$sites.sitename)

#we can use the basic open Esri.WorldImagery behind this and it works. Other maps can be obtained below.

#with mapview it is easy to change symbology, add legends, etc. This can be an option for the future.

library(ggmap)
get_map(source="google", maptype="hybrid")

#for whatever reason it won't let me download the map because it is "403 Forbidden." I have no idea if this is an IT issue or not.