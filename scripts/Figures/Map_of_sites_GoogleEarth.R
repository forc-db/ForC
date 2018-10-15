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
writeOGR(ForC_wgs84[, c("site.ID", "sites.sitename", "network", "masl", "geography.notes", "site.notes", "site.ref", "ref.notes", "lacks.info.from.ori.pub", "loaded.from", "loaded.by", "potential_duplicate_group", "potential_duplicate_group_parsed", "confirmed.unique", "potential.duplicates.manual", "duplicate.notes")], dsn="ForC_sites_GoogleEarth.kml", layer= "ForC_sites", driver="KML", dataset_options=c("NameField=name", all=TRUE))

#The issue now is trying to have all the columns displayed for each point with an overarching label that is sites.sitename. Multiple searches have hit brick walls since the dataset_options argument of the writeOGR command is labeled as "experimental."

#alternatively, loading the following package plus doing MapView provides an interactive map that we could then publish as an html. This would skip the .kml file use.

library(mapview)
mapview(ForC_wgs84)

#we can use the basic open Esri.WorldImagery behind this and it works. Other maps can be obtained here:

library(ggmap)
get_map(source="google", maptype="hybrid")

#for whatever reason it won't let me download the map because it is "403 Forbidden." I have no idea if this is an IT issue or not.