
# fix some coordinates 
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)

SITES[SITES$sites.sitename %in% "Yewak, Kosrae" & SITES$lat %in% 5.29 & SITES$lon %in% 163.03, c("lat", "lon") ] <- c(5.2904, 163.0286)
# SITES[SITES$sites.sitename %in% "Tomakomai Experimental Forest" & SITES$lat %in% 42.40 & SITES$lon %in% 141.40, c("lat", "lon") ] <- c(42.733333, 141.516667)
SITES[SITES$sites.sitename %in% "Bocas del Toro-dwarf" & SITES$lat %in% 9.35 & SITES$lon %in% -82.25, c("lat", "lon") ] <- c(9.35,	-82.26)
SITES[SITES$sites.sitename %in% "Bocas del Toro-fringe" & SITES$lat %in% 9.35 & SITES$lon %in% -82.25, c("lat", "lon") ] <- c(9.35,	-82.26)
SITES[SITES$sites.sitename %in% "Twin Cays-dwarf" & SITES$lat %in% 16.50 & SITES$lon %in% -88.10, c("lat", "lon") ] <- c(16.5, -88.4)
SITES[SITES$sites.sitename %in% "Twin Cays-fringe" & SITES$lat %in% 16.50 & SITES$lon %in% -88.10, c("lat", "lon") ] <- c(16.5, -88.4)
SITES[SITES$sites.sitename %in% "Giralia-dwarf" & SITES$lat %in% -21.73 & SITES$lon %in% 114.58000, c("lat", "lon") ] <- c(-22.45,	114.28)
SITES[SITES$sites.sitename %in% "Giralia-fringe" & SITES$lat %in% -21.73 & SITES$lon %in% 114.58000, c("lat", "lon") ] <- c(-22.45,	114.28)
SITES[SITES$sites.sitename %in% "Golden Gate Park" & SITES$lat %in% 37.70  & SITES$lon %in% -122.50000, c("lat", "lon") ] <- c(37.7,	-122.498)
SITES[SITES$sites.sitename %in% "Barro Colorado National Monument" & SITES$lat %in% 9.10  & round(SITES$lon, 2) %in% -79.83, c("lat", "lon") ] <- c(9.129,	-79.84)
SITES[SITES$sites.sitename %in% "Kesan Forest District" & round(SITES$lat, 2) %in% 40.58  & round(SITES$lon, 2) %in% 26.52, c("lat", "lon") ] <- c(40.61,	26.537)
SITES[SITES$sites.sitename %in% c("M1", "M2", "M3", "M5", "H1", "H2") & SITES$lat %in% 28.10000  & SITES$lon %in% -15.40000, c("lat", "lon") ] <- rep(c(28.1,	-15.414), each = length(c("M1", "M2", "M3", "M5", "H1", "H5")))
SITES[SITES$sites.sitename %in% "Takayama experimental forest" & round(SITES$lat, 2) %in% 37.13  & round(SITES$lon, 2) %in% 137.42, c("lat", "lon") ] <- c(36.15,	137.333)
SITES[SITES$sites.sitename %in% "Gahagan_2015_cfsa research site in Michigan USA" & round(SITES$lat, 2) %in% 47.10  & round(SITES$lon, 2) %in% -88.87, c("lat", "lon") ] <- c(47.09,	-88.87)
SITES[SITES$sites.sitename %in% "Coastal Forest Park" & round(SITES$lat, 2) %in% 30.97  & round(SITES$lon, 2) %in% 121.92, c("lat", "lon") ] <- c(30.96666667,	121.89)


write.csv(SITES, file = "data/ForC_sites.csv", row.names = F)
