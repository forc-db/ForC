######################################################
# Purpose: Plot a World map with biomes shown in ERL review, with sites used in that review.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.6.2 (2019-12-12)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(rgdal)

# Load pre-saved R environment ####
load("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")


# load and prepare ForC_simplified sites ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)

## Filter out managed, disturbed and no hisotry info sites
ForC_simplified <- ForC_simplified[ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0 & ForC_simplified$history.no.info %in% 0, ]

## filter out suspected duplicates ####
ForC_simplified <-ForC_simplified[ForC_simplified$suspected.duplicate %in% 0, ]


## Make stand.age a numeric variable
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)

## Remove rows with no Age

ForC_simplified <- droplevels(ForC_simplified[!is.na(ForC_simplified$stand.age), ])

## make geographic area and plot.name factors
ForC_simplified$geographic.area <- addNA(ForC_simplified$geographic.area)
ForC_simplified$plot.name <- addNA(ForC_simplified$plot.name)

## Prepare Forest Biomes
### Koeppen
ForC_simplified$Koeppen <- ifelse(grepl("^A", ForC_simplified$Koeppen), "Tropical",
                  ifelse(grepl("(^C)|(^D.a$)|(^D.b$)", ForC_simplified$Koeppen), "Temperate",
                         ifelse(grepl("(^D.c$)|(^D.d$)", ForC_simplified$Koeppen), "Boreal", "Other")))


### Broadleaf vs Conifer

broadleaf_codes <- c("2TEB", "2TDB", "2TB")
conifer_codes <- c("2TEN", "2TDN", "2TN")

ForC_simplified$Leaf_Trait <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
                     ifelse(ForC_simplified$dominant.veg %in% conifer_codes, "conifer", "Other"))

### combine to get tropical, temperate_broadleaf, temperate_evergreen and boreal

ForC_simplified$Biome <-  paste(ForC_simplified$Koeppen, ForC_simplified$Leaf_Trait)

ForC_simplified$Biome <- factor(ifelse(grepl("Boreal", ForC_simplified$Biome), "Boreal",
                                       ifelse(grepl("Tropical", ForC_simplified$Biome), "Tropical",
                                              ifelse(ForC_simplified$Biome %in% "Temperate broadleaf", ForC_simplified$Biome,
                                                     ifelse(ForC_simplified$Biome %in% "Temperate conifer", ForC_simplified$Biome, "Other")))))

table(ForC_simplified$Biome)

## Remove Biome Other
ForC_simplified <- droplevels(ForC_simplified[!ForC_simplified$Biome %in% "Other", ])

## get number of records per sites
No.of.records_per_site <- tapply(ForC_simplified$mean, ForC_simplified$sites.sitename, function(x) sum(!is.na(x)))

### No. of records
No.of.records <-  table(ForC_simplified$Koeppen)

### No. of sites
No.of.sites <- table(ForC_simplified[!duplicated(ForC_simplified$sites.sitename),]$Koeppen)

### No. of plots
No.of.plots <- table(ForC_simplified[!duplicated(ForC_simplified[, c("sites.sitename", "plot.name")]),]$Koeppen)

### get only on record per site and make spatial
SITES <- ForC_simplified[!duplicated(ForC_simplified$sites.sitename), c("sites.sitename", "lat", "lon", "FAO.ecozone")]
SITES$No.of.records <- No.of.records_per_site[SITES$sites.sitename]

coordinates(SITES) <- c("lon", "lat")
str(SITES)
proj4string(SITES) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")


rbPal <- colorRampPalette(c("yellow", "red4"))
SITES$Color <- rbPal(6)[as.numeric(cut(SITES$No.of.records, breaks = c(0,1,10,20,40,80,277)))]


# Plot ####

## With biogeographic regions and chart####

png("figures/World_Map_with_Biomes_and_ForC_simplified_sites_for_ERL_review.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)
par(mar = c(0,0,0,0))

plot(SYNMAP, col = "grey",  border = "transparent")
plot(KOEPPEN, add = T, col = KOEPPEN_biome_colors[KOEPPEN$Biome], border = "transparent")
plot(SYNMAP, add = T, col = SYNMAP_density, border = "transparent")


points(SITES, bg = SITES$Color, pch = 21, col = "black", lwd = 0.5)

rect(xleft = -180, xright = 180, ybottom = -91, ytop = -60, col = "white", border = "transparent")

# legend


legend(-182, -40, pch = 21, pt.bg = rbPal(6), legend = c("1", "2-10", "11-20", "21-40", "41-80", "81-277"), bty = "n", title = expression(bold("No. of records")))

legend(-130, -50, fill = rgb(matrix(0, nrow = 3, ncol = 3), alpha = c(255, 150, 100), maxColorValue = 255), border = "transparent", legend = c("Evegreen", "Decidous", "MixedED"), bty = "n", title = expression(bold("Dominant\nTree Type")), xpd = NA) # removed "other" with alpha = 50 (and nrow matrix 4)

legend(-60, -50, fill = rgb(t(col2rgb( KOEPPEN_biome_colors)), alpha = rep(200, 8), maxColorValue = 255), border = "transparent", legend = names(KOEPPEN_biome_colors), bty = "n", title = expression(bold("Biome")), xpd = NA)

par()$fig
par(fig = c(0.63, 1, 0, 0.41), new = T)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0,5,0,0))

b <- barplot(t(rbind( No.of.sites/sum(No.of.sites), No.of.plots/sum(No.of.plots), No.of.records/sum(No.of.records))), col = rgb(t(col2rgb(KOEPPEN_biome_colors[order(names(No.of.sites))])), alpha = rep(200, 8), maxColorValue = 255), horiz = T, xaxt = "n", border = "transparent")


axis(1, at = c(0, .5, 1), labels = c("0%", "50%"," 100%"))
mtext(c("records", "plots", "sites"), side = 2, at = rev(b), las = 1, line = 1)

dev.off()


# copy plot to ERL review repo ####

# file.copy("figures/World_Map_with_Biomes_and_ForC_simplified_sites_for_ERL_review.png",
#           paste0(dirname(getwd()), "/ERL-review/manuscript/tables_figures/World_Map_records_in_Biomes.png"), overwrite = T)
