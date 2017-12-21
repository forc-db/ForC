######################################################
# Purpose: Plot histogram of stand age
# Inputs: - ForC MEASUREMENTS table
# outputs:  1. png file, saved in figures folder as Histogram_of_Stand_age.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-23)
######################################################



# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")


# Load libraries ####

# Load ForC MEASUREMENTS table ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# Prepare data to plot ####
max.age <- 200



MEASUREMENTS$ageclass <- as.factor(ifelse(as.numeric(MEASUREMENTS$stand.age) == 999, "old-growth/undisturbed stand",
                                          ifelse(as.numeric(MEASUREMENTS$stand.age) > max.age, "known > max.age",
                                                 "known <= max.age")))

MEASUREMENTS$age.round <- round(as.numeric(as.character(MEASUREMENTS$stand.age))/ 5, 0) * 5


## known age <= max.age
table.to.plot.known <- with(MEASUREMENTS[MEASUREMENTS$ageclass == "known <= max.age",], table(tropical.extratropical, age.round))

## known age > max.age
table.to.plot.older.mas.age <- with(droplevels(MEASUREMENTS[MEASUREMENTS$ageclass == "known > max.age",]), table(tropical.extratropical, ageclass))

## old.growth or undisturbed stands
table.to.plot.old.growth <- with(droplevels(MEASUREMENTS[MEASUREMENTS$ageclass == "old-growth/undisturbed stand",]), table(tropical.extratropical, ageclass))


## Combine all
table.to.plot <- cbind(table.to.plot.known, table.to.plot.older.mas.age, table.to.plot.old.growth)


# Plot ##

x.labels <- dimnames(table.to.plot)[[2]][1:dim(table.to.plot.known)[2]]
x.labels <- c(x.labels, paste(">", max.age), "old-growth\nor undisturbed")

legend.labels <- c("Extratropical", "Tropical")
colscale <- c(gray(0.5), gray(0.8))

png(file="figures/Histogram_of_Stand_age.png", width=169, height = 100, units = "mm", res = 300, pointsize = 8)
par(mar = c(6, 4.1, 4.1, 2.1))
b <- barplot(table.to.plot, xlab = "Age (years)", ylab = "Number of Measurements", las = 1, col = colscale, names.arg = rep("", length(x.labels)))

text(x = c(head(b, -1), 52) , y = -20, labels = x.labels, xpd = TRUE, srt = 80, adj = 1)

legend(x = 40, y = 2000, fill = colscale, legend = legend.labels, bty = "n")

dev.off()
