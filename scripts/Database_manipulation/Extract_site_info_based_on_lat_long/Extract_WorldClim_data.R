##### extract WorldClim and CRU data for ForC_simplified

rm(list = ls())
library(raster)
library(ncdf4)
library(Hmisc)
library(sp)

ForC_sites <- read.csv("C:/Users/gyrcbm/Documents/GitHub/ForC/data/ForC_sites.csv")

### initially just remove sites without lat/lon
ForC_sites <- ForC_sites[!is.na(ForC_sites$lat),]
ForC_sites <- ForC_sites[!is.na(ForC_sites$lon),]

### set columns with co-ordinates, set projection and plot to check
coordinates(ForC_sites)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(ForC_sites)<-proj
plot(ForC_sites)

### first extract MAT and MAP
setwd("C:/Users/gyrcbm/Documents/GitHub/ForC/supplementary_resources/WorldClim data/world_clim/wc2.1_30s_bio/")
filenames<- c("wc2.1_30s_bio_1.tif","wc2.1_30s_bio_12.tif")
files <- stack(filenames)

sites <- raster::extract(files, ForC_sites)
df <- data.frame(ForC_sites)
df_clim <- data.frame(siteID = df[,1], sites.sitename = as.character(df[,2]), sites)
names(df_clim)[3:4] <- c("MAT", "MAP")

### then extract mean temperature of warmest and coldest months
setwd("C:/Users/gyrcbm/Documents/GitHub/ForC/supplementary_resources/WorldClim data/world_clim/wc2.1_30s_tavg/")
MAT_filenames<- paste("wc2.1_30s_tavg_", c(paste(0,1:9, sep=""), 10, 11, 12), ".tif",sep="")
MAT <- stack(MAT_filenames) 
month <- c("01 Jan 2010", "01 Feb 2010", "01 Mar 2010", "01 Apr 2010", "01 May 2010", "01 Jun 2010", "01 Jul 2010", "01 Aug 2010", "01 Sep 2010", "01 Oct 2010", "01 Nov 2010", "01 Dec 2010")

names(MAT) <- month

MAT_sites <- raster::extract(MAT, ForC_sites)
df <- data.frame(ForC_sites)
df_MAT <- data.frame(siteID = df[,1], sites.sitename = as.character(df[,2]), MAT_sites)

warmest <- apply(MAT_sites, 1, function(x){return(max(x))})
coldest <- apply(MAT_sites, 1, function(x){return(min(x))})

### bind data
df <- cbind(df, sites)
df <- cbind(df, warmest)
df <- cbind(df, coldest)

names(df)[45:46] <- c("MAT", "MAP")

### save
write.csv(df,"C:/Users/gyrcbm/Documents/GitHub/ForC/data/extracted_site_data/ForC_sites_climate_data.csv", row.names = F)
