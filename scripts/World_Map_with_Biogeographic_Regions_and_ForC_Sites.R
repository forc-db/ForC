######################################################
# Purpose: Plot a World map with Biogeographic regions and ForC sites, with or without Biogeographic zones and chart.
# Inputs: - pre-saved R environment (data_for_World_Map_with_Biogeographic_regions_and_sites.Rdata), created by script. Both script and .Rdata file are in supplementary_resources/World Map data
# outputs: 2 .png files saved in figures folder as 1/ World_Map_with_Biogeographic_regions_and_sites.png and 2/World_Map_of_sites.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-23)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####
library(rgdal)

# Load pre-saved R environment ####
load("supplementary_resources/World Map data/data_for_World_Map_with_Biogeographic_Regions_and_ForC_Sites.Rdata")

# Plot ####

## With biogeographic regions and chart####

png("figures/World_Map_with_Biogeographic_regions_and_sites.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)
par(mar = c(0,0,0,0))

plot(SYNMAP, col = "grey",  border = "transparent")
plot(ECOREGIONS, add = T, col = ECOREGIONS_colors, border = "white")
plot(SYNMAP, add = T, col = SYNMAP_density, border = "transparent")


points(SITES, bg = SITES$Color, pch = 21, col = "black", lwd = 0.5)

rect(xleft = -180, xright = 180, ybottom = -91, ytop = -60, col = "white", border = "transparent")
# legend


legend(-182, -40, pch = 21, pt.bg = rbPal(6), legend = c("1", "2-10", "11-20", "21-40", "41-80", "81-277"), bty = "n", title = expression(bold("No. of records")))

legend(-130, -50, fill = rgb(matrix(0, nrow = 4, ncol = 3), alpha = c(255, 150, 100, 50), maxColorValue = 255), border = "transparent", legend = c("Evegreen", "Decidous", "MixedED", "Other"), bty = "n", title = expression(bold("Forest Cover")))

legend(-60, -50, fill = rgb(t(col2rgb(c("goldenrod3", "violetred3", "cadetblue4", "chocolate", "darkgreen", "darkorchid4", "darkred"))), alpha = rep(200, 8), maxColorValue = 255), border = "transparent", legend = names(ECOREGIONS)[-2], bty = "n", title = expression(bold("Biogeographic zone")), ncol = 2)

par()$fig
par(fig = c(0.63, 1, 0, 0.41), new = T)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0,5,0,0))

b <- barplot(t(rbind(forested_area/sum(forested_area), No._of_sites/sum(No._of_sites), No._of_plots/sum(No._of_plots), No._of_records/sum(No._of_records))), col = rgb(t(col2rgb(c("goldenrod3", "grey", "violetred3", "cadetblue4", "chocolate", "darkgreen", "darkorchid4", "darkred")[order(names(ECOREGIONS))][-2])), alpha = rep(200, 8), maxColorValue = 255), horiz = T, xaxt = "n", border = "transparent")


axis(1, at = c(0, .5, 1), labels = c("0%", "50%"," 100%"))
mtext(c("No. of records", "No. of plots", "No. of sites", "forested area"), side = 2, at = rev(b), las = 1, line = 1)

dev.off()


## Without biogeographic regions and chart (Just colors for forest cover)####

png("figures/World_Map_of_sites.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)
par(mar = c(0,0,0,0))



names(SYNMAP)
SYNMAP_forest_color <- c("darkolivegreen3", "darkolivegreen", "darkolivegreen4", "gainsboro")


plot(SYNMAP, col = SYNMAP_forest_color,  border = "transparent")
axis(2)
box()
points(SITES, bg = SITES$Color, pch = 21, col = "black", lwd = 0.5)

rect(xleft = -180, xright = 180, ybottom = -91, ytop = -60, col = "white", border = "transparent")
# legend


legend(-182, 0, pch = 21, pt.bg = rbPal(6), legend = c("1", "2-10", "11-20", "21-40", "41-80", "81-277"), bty = "n", title = expression(bold("No. of records")))

legend(-130, -10, fill =SYNMAP_forest_color[c(2,3,1,4)], border = "transparent", legend = c("Evegreen", "MixedED", "Decidous", "Other"), bty = "n", title = expression(bold("Forest Cover")))

dev.off()

