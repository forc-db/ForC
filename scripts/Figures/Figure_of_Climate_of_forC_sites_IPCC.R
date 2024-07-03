######################################################
# Purpose: whittaker-like plot of ForC sites with data sent to IPCC.
# Developped by: Valentine Herrmann - HerrmannV@si.edu
#R version 4.4.0 (2024-04-24)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory ####
setwd(".")

# Load libraries ####
library(tidyverse)
library(sf)
library(plotbiomes) # devtools::install_github("valentinitnelav/plotbiomes")

# load data


# Load CRU data (summary of CRU data for 1990-2014 given by Ben Bond-Lamberty thought GitHub on 8/23/2017)
crudata <- read.csv("supplementary_resources/crudata_period.csv", stringsAsFactors = F)


# load WorldClim data to extract info from sites we don't have info from
WorldClim <- rast("S:/Global Maps Data/WorldClim/tiff/1bioclim_stacked_all.tif")

# # Download CRU data for precip and tmean
# precip <- rast("S:/Global Maps Data/CRU/CRU_v4_04/ncfiles/cru_ts4.04.1901.2019.pre.dat.nc")
# tmean <- rast("S:/Global Maps Data/CRU/CRU_v4_04/ncfiles/cru_ts4.04.1901.2019.tmp.dat.nc")


# ForC data
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

V_mapping <- read.csv("https://raw.githubusercontent.com/forc-db/IPCC-EFDB-integration/main/doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")

Meas_IDs_sent_to_EFDB <- read.csv("https://raw.githubusercontent.com/forc-db/IPCC-EFDB-integration/main/data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")


# keep only variables that we would send to IPCC
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$variable.name %in%  V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1],]

SITES <- SITES[SITES$sites.sitename %in% MEASUREMENTS$sites.sitename, ]

# indicate if site had some data sent to IPCC at some point
SITES$some_data_sent_to_EFDB <- SITES$sites.sitename %in% MEASUREMENTS$sites.sitename[MEASUREMENTS$measurement.ID %in% Meas_IDs_sent_to_EFDB$measurement.ID]


# extract temp and precip for sites that don't have it
idx_missing_map <- which(is.na(as.numeric(SITES$map))) 
idx_missing_mat <- which(is.na(as.numeric(SITES$mat))) 

idx_missing <- unique(c(idx_missing_map, idx_missing_mat))

new_data <- terra::extract(WorldClim, SITES[idx_missing, c("lon", "lat")]) #takes 10? mins
names(new_data)

new_data <- new_data[, c("1bioclim_stacked_all_1", "1bioclim_stacked_all_12")]
names(new_data) <- c("mat", "map")
new_data$mat <- new_data$mat/10


SITES[idx_missing, c("mat", "map")] <- new_data[,c("mat", "map")]# replacing even if there is a value there. just so mat and map "match" data source

# 
# # average CRU data
# # sum pre per year + average
# 
# precipYear <- format(terra:::time(precip), "%Y")
# precip <- app(tapp(precip, precipYear, sum), mean)/10 # to put in cm
# 
# # average temp
# tmean <- app(tmean, mean) 
# 
# 
# # get CRU data everywhere
# precip_global <- as.data.frame(precip, xy = TRUE) %>% drop_na() %>% rename(map = mean)
# temp_global <- as.data.frame(tmean, xy = TRUE) %>% drop_na() %>% rename(mat = mean)
# 
# map_mat_global <- left_join(precip_global, temp_global)
# 
# # rescale map and mat to match Whittaker range
# precip_from <- range(precip_global$map)
# temp_from <- range(temp_global$mat)
# 
# precip_to <- range(Whittaker_biomes$precp_cm)
# temp_to <- range(Whittaker_biomes$temp_c)
# 
# map_mat_global <- map_mat_global %>% mutate(
#   map = scales:: rescale(map, from = precip_from, to = precip_to),
#   mat = scales:: rescale(mat, from = temp_from, to = temp_to)
# )
# 
# # add biome to CRU data
# Whittaker_biomes_sf <- Whittaker_biomes %>% st_as_sf(coords = c("temp_c", "precp_cm")) %>% group_by(biome) %>% summarize() %>% st_convex_hull
# 
# map_mat_global_sp <- st_join(st_as_sf(map_mat_global, coords = c("mat", "map")), Whittaker_biomes_sf, left = TRUE, largest = T) 
# 
# 
# map_mat_global_sp$biome[is.na(map_mat_global$biome)] <- Whittaker_biomes_sf$biome[st_nearest_feature(y= Whittaker_biomes_sf, x = st_as_sf(map_mat_global_sp[is.na(map_mat_global_sp$biome),], coords = c("mat", "map")))]
# 
# map_mat_global$biome <- map_mat_global_sp$biome
# 
# ggplot() +
#   geom_hex(data = map_mat_global,
#            aes(x = mat, y = map ,  fill = biome, alpha = after_stat(count)), bins = 200) +
#   scale_alpha(range = c(0.5,1)) +
#   geom_polygon(data = Whittaker_biomes, aes(x = temp_c, y = precp_cm), fill = NA, col = "white")
# 
# 
# 
# # get CRU data for our sites
# SITES$




## Prepare data ####

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

my.colors  <- viridis::viridis_pal(begin = 0.85, end = 0) #colorRampPalette(c('gray87','gray26'))
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

png("figures/Climate_of_forC_sites_IPCC.png", width=8, height = 6, units = "in", res = 300)



plot(map ~ mat, data = SITES[!SITES$some_data_sent_to_EFDB, ], xlab="Average Temperature (°C)", ylab="Annual Precipitation (mm)" ,  xlim = c(-30, 35), ylim = c(0,8000), col=rgb(0,0,0,1), las = 1, type = "n")

for(i in 1:nrow(crudata_summary)){
  x <- crudata_summary[i, ]
  with(x, rect(xleft = xleft, xright = xright, ybottom = ybottom, ytop = ytop, col = Color, border = NA))
}


rect(xleft = -28, xright = -26, ybottom = seq(2000, 6500, length.out = 11)[c(1:10)], ytop = seq(2000, 6500, length.out = 11)[c(2:11)], col = my.colors(10), border = NA)

text(x = -25, y = seq(2000, 6500, length.out = 11), labels = as.character(round(labels.gradient/1000)), adj = 0)

text(x = -28, y = 7000, labels = expression("Global Climate space (x 1000 km"^2~")"), adj = 0)


points(map ~ mat, data = SITES[!SITES$some_data_sent_to_EFDB, ], pch = ".")
points(map ~ mat, data = SITES[SITES$some_data_sent_to_EFDB, ], pch = 2)

legend("topleft", pch = c(2), c("site with some data submitted to EFDB"), bty = "n")


dev.off()





