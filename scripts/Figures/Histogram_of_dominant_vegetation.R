######################################################
# Purpose: Plot Histogram of dominante vegetation type
# Inputs: - ForC MEASUREMENTS table
# outputs: 1 .png file saved in figures folder as Histogram_of_dominant_vegetation.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-24)
######################################################


# Clean environment ####
rm(list = ls())

# Setup working directory ####
setwd(".")

# Load libraries ####

# Load forC MEASUREMENTS table ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# Prepare data to plot ####


category <-ifelse(MEASUREMENTS$dominant.life.form %in% "woody+grass", "savanna", MEASUREMENTS$dominant.veg)

category <-ifelse(category %in% "2TB", "2TDB",
                  ifelse(category %in% "2TN", "2TDN",
                         ifelse(category %in%  c("2TD", "2TE"), "2TM",
                                ifelse(category %in% c("2BARE", "2GRAM"), "no woody vegetation",
                                       ifelse(category %in% c("NAC", "2TREE", "2SHRUB"), "woody other/unclassified",
                                              category)))))



data.to.plot <- table(category, dnn = NULL)

data.to.plot <- sort(data.to.plot, decreasing = T)

names(data.to.plot)
names.data.to.plot <- c("2TEN\nevergreen needleleaf", "2TDB\nbroadleaf deciduous", "2TEB\nbroadleaf evergreen", "2TM\nbroadleaf needleleaf mix", "2TDN\nneedleleaf deciduous", "woody\nother/unclassified", 
                        "savanna", "no woody\nvegetation")


# Plot ####

png(file="figures/Histogram_of_dominant_vegetation.png", width=200, height = 150, units = "mm", res = 300, pointsize = 10)

par(mar = c(8,5,2.1,2.1))

b <- barplot(data.to.plot, ylab = "", xlab = "", las = 1, xaxt = "n" )

text(x = b-0.5 , y = rep(-1000, length(names.data.to.plot)), labels = names.data.to.plot, xpd = TRUE, srt = 45)

mtext("Number of records", side = 2, line = 4)


dev.off()


