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


dates.collection <- as.numeric(substr(MEASUREMENTS$end.date, 1,4))
dates.collection <- ifelse(is.na(dates.collection), as.numeric(substr(MEASUREMENTS$date, 1,4)), dates.collection)

citations.DOI.to.exclude <- c("10.1111/j.1365-2486.2007.01420.x", "10.1111/j.1365-2486.2007.01439.x", "10.1071/BT07151", "10.1111/geb.12113", "10.1073/pnas.1317065111") # DOI corresponding to Litton et al. 2007, Luyssaert et al. 2007, Baldocchi 2008, Liu et al. 2014, Yu et al. 2014.

dates.citation <- ifelse(is.na(dates.collection) & !MEASUREMENTS$citations.doi %in% citations.DOI.to.exclude, MEASUREMENTS$citations.year, NA) # Replace NA by citation year excluding publication dates of big compilation listed above.

all.dates <- range(dates.collection, dates.citation, na.rm = T)
all.dates <- all.dates[1]:all.dates[2]


dates.collection <- factor(dates.collection, levels = all.dates)
dates.citation <- factor(dates.citation, levels = all.dates)



table.collection <- table(dates.collection)
table.citation <- table(dates.citation)

data.to.plot <- rbind(table.collection, table.citation)

## make x-year bins

bin_size <- 5
year0 <- ifelse(bin_size == 5, 1961, 1960)

# bins <- c(rep(1:(ncol(data.to.plot) %/% bin_size), each = bin_size), rep(ncol(data.to.plot) %/% bin_size +1, ncol(data.to.plot) %% 5)) 

bins <- c(rep(1, year0-1934), rep(2:c((2015 - year0+1) %/% bin_size +1) , each = bin_size), rep(c(2015- year0+1) %/% bin_size + 2, c(2015- year0+1)  %% bin_size))


bins.names <- tapply(all.dates, bins, function(x) paste(range(x), collapse = " - "))
bins.names[1] <- "pre-1960"

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


