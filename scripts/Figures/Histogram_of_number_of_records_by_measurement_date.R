######################################################
# Purpose: Plot Histogram of of number of records by measurement date
# Inputs: - ForC MEASUREMENTS table
# outputs: 1 .png file saved in figures folder as Histogram_of_number of records_by_measurement_date.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-30)
######################################################


# Clean environment ####
rm(list = ls())

# Setup working directory ####
setwd(".")

# Load libraries ####

# Load forC MEASUREMENTS table ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# Prepare data to plot ####


dates.collection <- substr(MEASUREMENTS$end_date, 1,4)
dates.collection <- as.numeric(ifelse(dates.collection %in% c("NI", "NRA", "NaN", "NAC") | is.na(dates.collection), NA, dates.collection))

dates.citation <- ifelse(is.na(dates.collection), MEASUREMENTS$citations.year, NA)


all.dates <- range(dates.collection, dates.citation, na.rm = T)
all.dates <-all.dates[1]:all.dates[2]


dates.collection <- factor(dates.collection, levels = all.dates)
dates.citation <- factor(dates.citation, levels = all.dates)



table.collection <- table(dates.collection)
table.citation <- table(dates.citation)

data.to.plot <- rbind(table.collection, table.citation)

## make 5-year bins
bins <- rep(1:(ncol(data.to.plot)/5), each = 5)
bins.names <- paste(all.dates[c(TRUE, rep(FALSE, 4))], all.dates[c(rep(FALSE, 4), TRUE)], sep = " - ")

data.to.plot <- t(apply(data.to.plot, 1, function(x) tapply(x, bins, sum)))
colnames(data.to.plot) <- bins.names

# plot ####


png(file="figures/Histogram_of_number_of_records_by_measurement_date.png", width=200, height = 150, units = "mm", res = 300)

par(mar = c(5.1,5,4.1,2.1))

b <- barplot(data.to.plot, las = 1, xaxt = "n", col = c("grey20", "grey"))

# axis(1, at =  b, labels = F, tck = -0.005)
text(x = b-0.5, y = rep(-600, length(b)), labels = bins.names, xpd = TRUE, srt = 45)

legend("topleft", fill = c("grey20", "grey"), legend = c("year of measurement", "year of publication"), bty = "n")

mtext("Number of records", side = 2, line = 3.5)


dev.off()


