######################################################
# Purpose: Plot a World map with FAO Ecozones and ForC sites, with n records as color and shape different if we sent any data from IPCC from it.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 4.0.3 (2020-10-10)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(sf)
library(tidyverse)

# Load pre-saved R environment ####
load("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")

# load data
sf::sf_use_s2(F)

SYNMAP <- st_read("S:/Global Maps Data/SYNMAP_Hurtt/synmap_polygon_dissolve.shp") %>% 
  filter(LandCovTex %in% c("Trees","Trees & Grasses")) %>%
  group_by(LandCovTex) %>%
  summarize() %>% 
  st_as_sf()


MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

V_mapping <- read.csv("https://raw.githubusercontent.com/forc-db/IPCC-EFDB-integration/main/doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")

Meas_IDs_sent_to_EFDB <- read.csv("https://raw.githubusercontent.com/forc-db/IPCC-EFDB-integration/main/data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

FAO_colors <- read.csv("supplementary_resources/World Map data/Biogegraphic_Regions/FAO_ names_and_codes.csv")



# keep only variables that we would send to IPCC
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$variable.name %in%  V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1],]

SITES <- SITES[SITES$sites.sitename %in% MEASUREMENTS$sites.sitename, ]

# indicate if site had some data sent to IPCC at some point
SITES$some_data_sent_to_EFDB <- SITES$sites.sitename %in% MEASUREMENTS$sites.sitename[MEASUREMENTS$measurement.ID %in% Meas_IDs_sent_to_EFDB$measurement.ID]

# count records ####
No.of.records <- tapply(MEASUREMENTS$mean, MEASUREMENTS$sites.sitename, function(x) sum(!is.na(x)))
No.of.records_sent <- tapply(MEASUREMENTS$mean[MEASUREMENTS$measurement.ID %in% Meas_IDs_sent_to_EFDB$measurement.ID], MEASUREMENTS$sites.sitename[MEASUREMENTS$measurement.ID %in% Meas_IDs_sent_to_EFDB$measurement.ID], function(x) sum(!is.na(x)))


SITES <- SITES[!is.na(SITES$lat) & !is.na(SITES$lon), c("sites.sitename", "lat", "lon", "FAO.ecozone", "some_data_sent_to_EFDB")]
SITES$No.of.records <- No.of.records[SITES$sites.sitename]
SITES$No.of.records_sent <- No.of.records_sent[SITES$sites.sitename]



coordinates(SITES) <- c("lon", "lat")
str(SITES)
proj4string(SITES) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


rbPal <- colorRampPalette(c("yellow", "red4"))
bin = 9
SITES$No.of.records_Group <- findInterval(SITES$No.of.records, unique(quantile(SITES$No.of.records, seq(0, 1, length.out = bin + 1))), rightmost.closed = T)
SITES$No.of.records_Group[SITES$No.of.records == 1] <- 0
SITES$No.of.records_Group <- factor(SITES$No.of.records_Group)
levels(SITES$No.of.records_Group) <- tapply(SITES$No.of.records, SITES$No.of.records_Group, function(x) paste(min(x), max(x), sep = "-"))

levels(SITES$No.of.records_Group) <-  gsub("(^\\d)-\\1", "\\1", levels(SITES$No.of.records_Group))
                                          
SITES$Color <- rbPal(bin)[as.numeric(SITES$No.of.records_Group)]



# make site's FAO.ecozone a factor
SITES$FAO.ecozone <- factor(SITES$FAO.ecozone, levels = FAO_colors$gez_name)


# get info for barplot
order_FAO_zones <- FAO_colors$gez_name 

### No. of sites
No._of_sites <- table(SITES$FAO.ecozone[SITES$some_data_sent_to_EFDB])[order_FAO_zones]

### No. of records
No._of_records <- tapply(SITES$No.of.records[SITES$some_data_sent_to_EFDB], factor(SITES$FAO.ecozone[SITES$some_data_sent_to_EFDB], levels = FAO_colors$gez_name), sum, default = 0)[order_FAO_zones]

### No. of records sent
No._of_records_sent <- tapply(SITES$No.of.records_sent[SITES$some_data_sent_to_EFDB], factor(SITES$FAO.ecozone[SITES$some_data_sent_to_EFDB], levels = FAO_colors$gez_name), sum, default = 0)[order_FAO_zones]



### FAO areas within SYNMAP
FAO_areas <- st_intersection(SYNMAP, FAO %>% st_as_sf) %>% group_by(gez_name) %>% summarize()
FAO_areas$AREA <- st_area(FAO_areas)

FAO_areas <- setNames(FAO_areas$AREA, FAO_areas$gez_name)[order_FAO_zones]




# Plot ####

png("figures/World_Map_of_sites_with_FAO_and_IPCC_data_sent.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)

par(mar = c(0,0,0,0))


# plot(SYNMAP, col = SYNMAP_forest_color,  border = "transparent")
plot(FAO, col = FAO_colors$Color, border = "transparent")
axis(2)
box()

points(SITES[!SITES$some_data_sent_to_EFDB, ], bg = SITES$Color, pch =21, lwd = 0.5, cex = 1, col = "black")
points(SITES[SITES$some_data_sent_to_EFDB, ], bg = SITES$Color, pch =24, lwd = 1, cex = 1.3, col = "black") # these will be drawn on top


rect(xleft = -180, xright = 180, ybottom = -91, ytop = -60, col = "white", border = "transparent")
# legend


legend(-185, -35, pch = 21, pt.bg = rbPal(bin), pt.lwd = 0.5, legend = levels(SITES$No.of.records_Group), bty = "n", title = expression(bold("No. of records")))

legend(-185, -85, pch = 24, pt.cex = 1.3, pt.bg = "white", legend = c("site with some data submitted\nto EFDB"), bty = "n", title = "")

legend(-75, -45, fill =FAO_colors$Color, border = "transparent", legend = FAO_colors$gez_abbrev, bty = "n", title = expression(bold("FAO ecozone")), ncol = 3) # removed other




## add barplot of ecoozone availability + number of records and sites


par()$fig
par(fig = c(0.63, 1, 0, 0.41), new = T)
par(mar = c(3.1, 5.1, 4.1, 2.1))
par(oma = c(0,5,0,0))


b <- barplot(cbind(prop.table(No._of_records_sent), prop.table(No._of_sites), prop.table(FAO_areas)), col = FAO_colors$Color[match(order_FAO_zones, FAO_colors$gez_name)], horiz = T, xaxt = "n", border = "transparent")


axis(1, at = c(0, .5, 1), labels = c("0%", "50%"," 100%"))
mtext(c("tree cover area", "sites submitted", "records submitted"), side = 2, at = rev(b), las = 1, line = 0.2, cex = 0.9)
# mtext(c("(Polar region ignored)"), side = 1, at =0.5, las = 1, line = 1.8, cex = 0.6)


dev.off()
