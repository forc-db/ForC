######################################################
# Purpose: Plot a two pannel figure with a) Precipitation against Temperature of Forc sites and b) histogram of Köppen-Geiger climate zones represented in the database (Koeppen field in SITES)
# Inputs: - ForC_db SITES table
#         - summary of CRU data for 1990-2014 given by Ben Bond-Lamberty thought GitHub on 8/23/2017 
#         - A subset of sites (in SITES table) that have their mean annual temperature and precipitation extracted from WoldClim data base. This file (as well as the script that creates it) is in supplementary_resources\WorldClim
# outputs: 1. png file, saved in figures folder as Climate_of_forC_sites.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-23)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(reshape)

# Load sites table ####
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

# Load CRU data (summary of CRU data for 1990-2014 given by Ben Bond-Lamberty thought GitHub on 8/23/2017)
crudata <- read.csv("supplementary_resources/crudata_period.csv", stringsAsFactors = F)

# Load file with subset of sites for which we extracted WorldClim MAT and MAP

WorldClim.missing.map.mat <- read.csv("supplementary_resources/WorldClim data/SITES.missing.map.mat.WorldClim.csv")


# set up ploting device for the 2 pannel plot + figure size
old.par <- par()

png("figures/Climate_of_forC_sites.png", width=600, height = 300, units = "mm", res = 300, pointsize = 20)
layout(matrix(c(1,2), nrow = 1), c(1,1.5))
par(mar = c(5.1,4.1,3,1) )

# plot pannel a) ####

## Prepapre data ####

### bin CRU by temp and precip

crudata$tmp_round <- round(crudata$tmp / 2, 0) * 2
crudata$pre_round <- round(crudata$pre / 300, 0) * 300


crudata_summary <- tapply(crudata$area_km2, list(crudata$tmp_round, crudata$pre_round), sum, na.rm = T)
crudata_summary <- melt(crudata_summary)
names(crudata_summary) <- c("MAT", "MAP", "Area_km2")



## prepare coordinates and colors of tiles ####
rectangles.x <- sort(unique(crudata_summary$MAT))
rectangles.y <-  sort(unique(crudata_summary$MAP))

crudata_summary$xleft <- crudata_summary$MAT
crudata_summary$xright <- rectangles.x[match(crudata_summary$xleft, rectangles.x) + 1]
crudata_summary$ybottom <- crudata_summary$MAP
crudata_summary$ytop <- rectangles.y[match(crudata_summary$ybottom, rectangles.y) + 1]

my.colors  <- colorRampPalette(c('gray87','gray26'))
crudata_summary$Color <- my.colors(10)[as.numeric(cut(crudata_summary$Area_km2,breaks = 10))]

## prepare labels for legend ####
labels.gradient <- levels(cut(crudata_summary$Area_km2,breaks = 10, dig.lab = 10))
labels.gradient <- gsub("]", "", labels.gradient)
labels.gradient <- gsub("\\(", "", labels.gradient)
labels.gradient <- strsplit(x = labels.gradient, ",")
labels.gradient <- unlist(labels.gradient)
labels.gradient <- unique(labels.gradient)
labels.gradient <- round(as.numeric(labels.gradient))
labels.gradient[1] <- 0

## plot ####

plot(SITES$map ~ SITES$mat, xlab="Average Temperature (°C)", ylab="Annual Precipitation (mm)" ,  xlim = c(-30, 35), ylim = c(0,8000), col=rgb(0,0,0,1), las = 1)

for(i in 1:nrow(crudata_summary)){
  x <- crudata_summary[i, ]
  with(x, rect(xleft = xleft, xright = xright, ybottom = ybottom, ytop = ytop, col = Color, border = NA))
}


rect(xleft = -28, xright = -26, ybottom = seq(2000, 6500, length.out = 11)[c(1:10)], ytop = seq(2000, 6500, length.out = 11)[c(2:11)], col = my.colors(10), border = NA)

text(x = -25, y = seq(2000, 6500, length.out = 11), labels = as.character(round(labels.gradient/1000)), adj = 0)

text(x = -28, y = 7000, labels = expression("Global Climate space (x 1000 km"^2~")"), adj = 0)

     
points(SITES$map ~ SITES$mat, pch = 16)
points(WorldClim.missing.map.mat$map ~ WorldClim.missing.map.mat$mat, pch = 1)
legend("topleft", pch = c(16, 1), c("ForC", "WorldClim"), bty = "n")

mtext("a)", side = 3, line = 1, adj = 0.01)








# plot pannel b) ####

KOEPPEN_regions <- data.frame(Koeppen = unique(SITES$Koeppen))
KOEPPEN_regions$Climate_zone <- NA
KOEPPEN_regions <- KOEPPEN_regions[KOEPPEN_regions$Koeppen != "NAC",]
KOEPPEN_regions <- KOEPPEN_regions[order(KOEPPEN_regions$Koeppen),]
KOEPPEN_regions$Climate_zone <- ifelse(grepl("A", KOEPPEN_regions$Koeppen, ignore.case = F), "A",
                                       ifelse(grepl("B", KOEPPEN_regions$Koeppen, ignore.case = F), "B",
                                              ifelse(grepl("C", KOEPPEN_regions$Koeppen, ignore.case = F), "C",
                                                     ifelse(grepl("D", KOEPPEN_regions$Koeppen, ignore.case = F), "D",
                                                            ifelse(grepl("E", KOEPPEN_regions$Koeppen, ignore.case = F), "E",NA)))))

SITES <- merge(SITES, KOEPPEN_regions, by = "Koeppen", all.x = T)

table.to.plot <- table(SITES$Koeppen, SITES$Climate_zone)

# table.to.plot <- head(table.to.plot, -1)

colors.to.plot <- c(colorRampPalette(c('dodgerblue4','dodgerblue'))(sum(grepl("A", KOEPPEN_regions$Climate_zone))),
                    colorRampPalette(c('firebrick4','indianred1'))(sum(grepl("B", KOEPPEN_regions$Climate_zone))),
                    colorRampPalette(c('green4','olivedrab3','khaki'))(sum(grepl("C", KOEPPEN_regions$Climate_zone))),
                    colorRampPalette(c('mediumpurple4', 'maroon4','plum2'))(sum(grepl("D", KOEPPEN_regions$Climate_zone))),
                    colorRampPalette(c('ivory4','ivory2'))(sum(grepl("E", KOEPPEN_regions$Climate_zone)))                   )

x.labels <- c("A - Tropical", "B - Arid", "C - Temperate", "D - Cold\n(continental)", "E - Polar")

b <- barplot(table.to.plot, xlab = "Climate zone", ylab = "Number of sites", las = 1, col = colors.to.plot, xlim = c(0,6.5), names.arg = rep("", 5))

text(x = b - .25 , y = -35, labels = x.labels, xpd = TRUE, srt = 15)


legend("topright", fill = colors.to.plot, legend = rownames(table.to.plot), bty = "n", title = "Koeppen climate")

mtext("b)", side = 3, line = 1, adj = -0.05)

dev.off()

par(old.par)

