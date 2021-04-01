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
library(sp)


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

# LOAD KOEPPEN and make Biomes column ####
KOEPPEN <- readOGR("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/koeppen_dissolved.shp", layer = "koeppen_dissolved", stringsAsFactors = F)
categoriesKOEPPEN <- read.table("S:/Global Maps Data/Shapefiles/Koeppen-Geiger-GIS/Legend.txt", stringsAsFactors = F)
categoriesKOEPPEN <- categoriesKOEPPEN[, c(1,3)]
colnames(categoriesKOEPPEN) <- c("GRIDCODE", "Koeppen")

categoriesKOEPPEN$Biome <- ifelse(grepl("^A", categoriesKOEPPEN$Koeppen ), "Tropical",
                                    ifelse(grepl("(^C)|(^D.a$)|(^D.b$)", categoriesKOEPPEN$Koeppen ), "Temperate",
                                           ifelse(grepl("(^D.c$)|(^D.d$)", categoriesKOEPPEN$Koeppen ), "Boreal", "Other")))

KOEPPEN$Biome <- categoriesKOEPPEN$Biome[match(KOEPPEN$GRIDCODE, categoriesKOEPPEN$GRIDCODE)]

# LOAD FAO ecozones ####
FAO <- readOGR("S:/Global Maps Data/Shapefiles/FAO global eco_zone/gez_2010_wgs84.shp", layer = "gez_2010_wgs84", stringsAsFactors = F)

# CREATE COLOR VECTORS ####
names(ECOREGIONS)
ECOREGIONS_colors = c("goldenrod3", "grey", "violetred3", "cadetblue4", "chocolate", "darkgreen", "darkorchid4", "darkred")

names(SYNMAP)
SYNMAP_density <- rgb(c(255,255,255,255), c(255,255,255,255), c(255,255,255,255), alpha =  c(100,0,150,200), maxColorValue = 255)


KOEPPEN_biome_colors <- c(Boreal = "cadetblue",
                          Temperate = "darkorange",
                          Tropical = "tomato",
                          Other = "grey")


save.image("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")



