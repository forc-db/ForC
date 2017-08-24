######################################################
# Purpose: Plot histogram of stand age
# Inputs: - ForC MEASUREMENTS table
# outputs: 2 .png files with figure of World Map with our without Biogegraphic zones and chart.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.0 (2017-08-23)
######################################################



# Clean environment ####
rm(list = ls())

# Setup working directory ####
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
table.to.plot.known <- with(MEASUREMENTS[MEASUREMENTS$ageclass == "known <= max.age",], table(tropical_extratropical, age.round))

## known age > max.age
table.to.plot.older.mas.age <- with(droplevels(MEASUREMENTS[MEASUREMENTS$ageclass == "known > max.age",]), table(tropical_extratropical, ageclass))

## old.growth or undisturbed stands
table.to.plot.old.growth <- with(droplevels(MEASUREMENTS[MEASUREMENTS$ageclass == "old-growth/undisturbed stand",]), table(tropical_extratropical, ageclass))


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

# Prepare ploting divice
old.par <- par()
layout(matrix(c(1,2,3), nrow = 1), c(0.5,0.5,4))
par(mar = c(4,4,1,1), oma = c(0,4,0,0))

barplot(table.to.plot.old.growth, ylim = c(0, 2300), axes = F)
axis(2, line = 0.5, las = 1)
mtext(side = 2, text = "Number of measurements", line = 4, cex = 0.7)
barplot(table.to.plot.older.mas.age,  ylim = c(0, 2300), axes = F)

barplot(table.to.plot.known, xlab = "age (year)", las = 1)



par(old.par)


MEASUREMENTS$ageclass <- as.factor(ifelse(as.numeric(MEASUREMENTS$stand.age) < 20.5, "young",
                                          ifelse(as.numeric(MEASUREMENTS$stand.age) > 99.9, "mature", "intermediate")))
MEASUREMENTS$stand.name <- paste(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name, sep = ":")
MEASUREMENTS$TROPICS <- ifelse(MEASUREMENTS$tropical_extratropical ==  "tropical", 1, 0)
MEASUREMENTS$age.r <- round(as.numeric(MEASUREMENTS$stand.age), digits=0)



nplotbyage <- MEASUREMENTS %>% group_by(age.r) %>% summarize(nplot=n_distinct(stand.name)) 
nplotbyage <- nplotbyage[!is.na(nplotbyage$age.r),]
nplotbyage <- rbind(nplotbyage[nplotbyage$age.r <= 200,], apply(nplotbyage[nplotbyage$age.r > 200,], 2, sum, na.rm = T))

nplotbyage.tropic <- MEASUREMENTS %>% group_by(age.r, TROPICS) %>% summarize(nplot=n_distinct(stand.name))  %>% tbl_df %>% spread(TROPICS, nplot)
nplotbyage.tropic <- nplotbyage.tropic[!is.na(nplotbyage.tropic$age.r),]

nplotbyage.old.tropics <- MEASUREMENTS %>% group_by(age.r, TROPICS) %>% summarize(nplot=n_distinct(stand.name)) %>% filter(age.r > max.age) %>% tbl_df %>% spread(TROPICS, nplot)

nplotbyage.tropic <- rbind(nplotbyage.tropic[nplotbyage.tropic$age.r <= 200,], apply(nplotbyage.tropic[nplotbyage.tropic$age.r > 200,], 2, sum, na.rm = T))

nplotbyage2 <- data.frame(age.r = c(0:max.age, 30609))
nplotbyage2 <- left_join(nplotbyage2, nplotbyage) %>% left_join(nplotbyage.tropic)
nplotbyage2 <- nplotbyage2 %>% left_join(nplotbyage.old.tropics)
nplotbyage2[is.na(nplotbyage2)] <- 0
nplotbyage2$total <- nplotbyage2[,3] + nplotbyage2[,4] 
all.equal(nplotbyage2$total, nplotbyage2$nplot) # TRUE -- > good
nplotbyage3 <- nplotbyage2[, c(1,3,4)]

nplotbyage3$age.r[202] <- 220
tail(nplotbyage3)

#### Fig 6 ####

nplotbyage3$age.r <- as.factor(as.character(nplotbyage3$age.r))
x <- as.matrix(nplotbyage3)
x <- t(x) #transpose
colnames(x) <- x[1,]
x <- x[-1,]
rownames(x)
legendfig6 <- c("Extratropical", "Tropical")
colscale <- c(gray(0.5), gray(0.8))

str(legendfig6)
par()
default <- par()

tiff(file="Fig6.tiff", width=169, height = 100, units = "mm", res = 300, pointsize = 12)
par(mar=c(3,3,1,1.5))

b <- barplot(x, col = colscale, ylim=c(0,700), width=3, border="black", space=0, axes = F, axisnames = F)
box(bty = "O")
axis(2, tck = 0.02)
axis(1, at = b[1+seq(0, max.age, by = 20)], labels = seq(0, max.age, by = 20))
title(xlab= "Age (years)", ylab= "No. of plots", line=2)
legend("topright", bty="n", fill = rev(colscale), legend = rev(legendfig6), cex=0.9) 
dev.off()
par(default)

