######################################################
# Purpose: Plot Histogram of dominante vegetation type
# Inputs: - ForC MEASUREMENTS table
# outputs: 1 .png file with histogram
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-24)
######################################################


# Clean environment ####
rm(list = ls())

# Setup working directory ####
setwd(".")

# Load libraries ####
)

# Load forC MEASUREMENTS table ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# Plot ####

# tiff(file="Fig4.tiff", width=169, height = 100, units = "mm", res = 300, pointsize = 10)

category <-ifelse(MEASUREMENTS$dominant.lifeform %in% "woody+grass", "savanna", MEASUREMENTS$dominantveg)
                  
category <-ifelse(category %in% "2TB", "2TDB",
                  ifelse(category %in% "2TN", "2TDN",
                         ifelse(category %in%  c("2TD", "2TE"), "2TM",
                                ifelse(category %in% c("2BARE", "2GRAM"), "no woody vegetation",
                                       ifelse(category %in% c("NAC", "2TREE", "2SHRUB"), "woody other/unclassified",
                                              category)))))

unique(cbind(category, MEASUREMENTS$dominant.lifeform))


data.to.plot <-  sort(tapply(MEASUREMENTS$measurementID, dominant.lifeform.cat, length), decreasing = T)
data.to.plot <-   data.to.plot[names(data.to.plot) != "NAC"]

data.to.plot <- table(category, dnn = NULL)

b <- barplot(category, ylab = "Number of records", xlab = "Dominant lifeform", las = 1, xaxt = "n" )

text(x = b , y = c(rep(-800, 6), -1200), labels = names(data.to.plot), xpd = TRUE, srt = 45)

dev.off()


