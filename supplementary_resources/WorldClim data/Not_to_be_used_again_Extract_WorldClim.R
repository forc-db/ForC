

SITES <- read.csv("FINAL_VERSION_KAT/ForC_db_sites.csv", stringsAsFactors = F)

missing.map.mat <- SITES[(SITES$map %in% c( "NAC", "NI", "NRA", " ", "")) | is.na(SITES$map) | (SITES$mat %in% c( "NAC", "NI", "NRA", " ", "")) | is.na(SITES$mat), c("sites.sitename", "lat", "lon")]


WorldClim <- stack("R:/Global Maps Data/WorldClim/tiff/1bioclim_stacked_all.tif")

points <- missing.map.mat
coordinates(points)<-c("lon", "lat")
proj<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
proj4string(points)<-proj
plot(points)

WorldClim.missing.map.mat <- raster::extract(WorldClim, points) #takes 10? mins
WorldClim.missing.map.mat <- as.data.frame(WorldClim.missing.map.mat)
colnames(WorldClim.missing.map.mat) <- c("AnnualMeanTemp", "MeanDiurnalRange", "Isothermality","TempSeasonality", "MaxTWarmestMonth", "MinTColdestMonth", "TempRangeAnnual", "MeanTWetQ", "MeanTDryQ","MeanTWarmQ","MeanTColdQ", "AnnualPre","PreWetMonth", "PreDryMonth", "PreSeasonality", "PreWetQ", "PreDryQ", "PreWarmQ", "PreColdQ")
head(WorldClim.missing.map.mat)


WorldClim.missing.map.mat$AnnualMeanTemp  <- WorldClim.missing.map.mat$AnnualMeanTemp / 10

missing.map.mat <- cbind(missing.map.mat, WorldClim.missing.map.mat[, c("AnnualMeanTemp", "AnnualPre")])
                         
names(missing.map.mat) <- c("sites.sitename", "lat", "lon", "mat", "map")

write.csv(missing.map.mat, "FINAL_VERSION_KAT/SITES.missing.map.mat.WorldClim.csv", row.names = F)
