######################################################
# Purpose: Prepare data used by the script that plots a World map with Biogeographic regions and ForC sites.
# Inputs: - SYNMAP shapefile (polygons, dissolved to b smaller) - You can download SYNMAP maps here: ftp://nacp.ornl.gov/synthesis/2009/frescati/model_driver/land_use_change/analysis/global/Hurtt_SYNMAP/ 
#         - SYNMAP legend file
#         - WWF ECOREGIONS shapefile (polygons of ecoregions) - You can download this map here: https://www.sciencebase.gov/catalog/item/508fece8e4b0a1b43c29ca22
#         - ForC SITES table
#         - ForC MEASUREMENTS table
#         - ForC HISTORY table
# outputs: .Rdata file with all objects needed to create map
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-24)
######################################################


# Clean environment ####
rm(list = ls())

# Setup working directory ####
setwd(".")

# Load libraries ####
library(rgdal)
library(rgeos)


# LOAD SYNMAP MAP AND LEGEND ####


SYNMAP <- readOGR("S:/Global Maps Data/SYNMAP_Hurtt/synmap_polygon_dissolve.shp")

SYNMAP$GRIDCODE <- as.numeric(as.character(SYNMAP$GRIDCODE))


SYNMAP_legend <- read.csv("S:/Global Maps Data/SYNMAP_Hurtt/SYNMAP_Legend.csv")

## Separate gricodes of synmap into forest_cover types

Evegreen_GRIDCODE <- c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 31, 34)
Deciduous_GRIDCODE <- c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35)
MixedED_GRIDCODE <- c(3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36)

SYNMAP$Forest_Cover <- ifelse(SYNMAP$GRIDCODE  %in% Evegreen_GRIDCODE, "Evergreen",
                              ifelse(SYNMAP$GRIDCODE %in% Deciduous_GRIDCODE, "Deciduous",
                                     ifelse(SYNMAP$GRIDCODE %in% MixedED_GRIDCODE, "MixedED", "Other")))

## Disolve 
SYNMAP <- gUnaryUnion(SYNMAP, id = SYNMAP$Forest_Cover)

## Any self-intersection ??

which(!gIsValid(SYNMAP, byid = TRUE))

# LOAD WWF ECOREGIONS MAP ####

ECOREGIONS <-  readOGR("S:/Global Maps Data/Shapefiles/WWF Terrestrial Ecoregions/modifications for TropForC fig/wwf_terr_ecos.shp")

unique(ECOREGIONS$REALM_1)

ECOREGIONS <- gUnaryUnion(ECOREGIONS, id = ECOREGIONS$REALM_1)
summary(ECOREGIONS)

## Any self-intersection ??

which(!gIsValid(ECOREGIONS, byid = TRUE)) # should be empty


# LOAD COUNTOUR MAP ####



# CREATE COLOR VECTORS
names(ECOREGIONS)
ECOREGIONS_colors = c("goldenrod3", "grey", "violetred3", "cadetblue4", "chocolate", "darkgreen", "darkorchid4", "darkred")

names(SYNMAP)
SYNMAP_density <- rgb(c(255,255,255,255), c(255,255,255,255), c(255,255,255,255), alpha =  c(100,0,150,200), maxColorValue = 255)


# ADD SITES

MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

No.of.records <- tapply(MEASUREMENTS$mean, MEASUREMENTS$sites.sitename, function(x) sum(!is.na(x)))
No.of.records <- data.frame(sites.sitename = rownames(No.of.records), No.of.records = as.vector(No.of.records))
str(No.of.records)

SITES <- SITES[, c("sites.sitename", "lat", "lon", "FAO.ecozone")]
SITES <- merge(SITES, No.of.records, by = "sites.sitename")



coordinates(SITES) <- c("lon", "lat")
str(SITES)
proj4string(SITES) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


rbPal <- colorRampPalette(c("yellow", "red4"))
SITES$Color <- rbPal(6)[as.numeric(cut(SITES$No.of.records, breaks = c(0,1,10,20,40,80,277)))]


## make inset plot ####

SITES$Ecoregion <- over(SITES,ECOREGIONS) 
SITES$Ecoregion <- names(ECOREGIONS)[SITES$Ecoregion]

HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)


HISTORY_Summary <- merge(HISTORY[, c("sites.sitename", "plot.name")], SITES[, c("sites.sitename", "Ecoregion")])
MEASUREMENTS_Summary <- merge(MEASUREMENTS[, c("sites.sitename", "plot.name")], SITES[, c("sites.sitename", "Ecoregion")])


### forested area
forested_area <- table(SITES$Ecoregion[grepl("forest", SITES$FAOecozone)])

### No. of sites
No._of_sites <- table(SITES$Ecoregion)

### No. of plots
No._of_plots <- table(HISTORY_Summary$Ecoregion)

### No. of records
No._of_records <- table(MEASUREMENTS_Summary$Ecoregion)



save.image("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")



