
# remove stand.density measurements

# load file 
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

# remove stand.density but keep a file of it
MEASUREMENTS_stand.density <- MEASUREMENTS[MEASUREMENTS$variable.name %in% "stand.density", ]

MEASUREMENTS <- MEASUREMENTS[!MEASUREMENTS$variable.name %in% "stand.density", ]

# save 
write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names =  F)
write.csv(MEASUREMENTS_stand.density, "database_management_records/ForC_measurements_stand_density_only.csv", row.names =  F)
