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
library(rgdal)
library(rgeos)

# Load pre-saved R environment ####
load("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")

# load data

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


SITES <- SITES[!is.na(SITES$lat) & !is.na(SITES$lon), c("sites.sitename", "lat", "lon", "FAO.ecozone", "some_data_sent_to_EFDB")]
SITES$No.of.records <- No.of.records[SITES$sites.sitename]



coordinates(SITES) <- c("lon", "lat")
str(SITES)
proj4string(SITES) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


rbPal <- colorRampPalette(c("yellow", "red4"))
bin = 9
SITES$No.of.records_Group <- findInterval(SITES$No.of.records, unique(quantile(SITES$No.of.records, seq(0, 1, length.out = bin + 1))), rightmost.closed = T)
SITES$No.of.records_Group[SITES$No.of.records == 1] <- 0
SITES$No.of.records_Group <- factor(SITES$No.of.records_Group)
levels(SITES$No.of.records_Group) <- tapply(SITES$No.of.records, SITES$No.of.records_Group, function(x) paste(min(x), max(x), sep = "-"))

levels(SITES$No.of.records_Group) <-  gsub("(^\\d&)-\\1", "\\1", levels(SITES$No.of.records_Group))
                                          
SITES$Color <- rbPal(bin)[as.numeric(SITES$No.of.records_Group)]








# Plot ####

png("figures/World_Map_of_sites_with_FAO_and_IPCC_data_sent.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)
par(mar = c(0,0,0,0))


# plot(SYNMAP, col = SYNMAP_forest_color,  border = "transparent")
plot(FAO, col = FAO_colors$Color, border = "transparent")
axis(2)
box()
points(SITES, bg = SITES$Color, pch = ifelse(SITES$some_data_sent_to_EFDB, 24, 21), lwd = ifelse(SITES$some_data_sent_to_EFDB, 1, 0.5), cex = ifelse(SITES$some_data_sent_to_EFDB, 1.5, 1), col = "black")

rect(xleft = -180, xright = 180, ybottom = -91, ytop = -60, col = "white", border = "transparent")
# legend


legend(-182, -35, pch = 21, pt.bg = rbPal(bin), pt.lwd = 0.5, legend = levels(SITES$No.of.records_Group), bty = "n", title = expression(bold("No. of records")))

legend(-182, -90, pch = 24, pt.cex = 1.5, pt.bg = "white", legend = c("Site with some data sent to EFDB"), bty = "n", title = "")

legend(-20, -40, fill =FAO_colors$Color, border = "transparent", legend = FAO_colors$gez_abbrev, bty = "n", title = expression(bold("FAO ecozone")), ncol = 3) # removed other

dev.off()

