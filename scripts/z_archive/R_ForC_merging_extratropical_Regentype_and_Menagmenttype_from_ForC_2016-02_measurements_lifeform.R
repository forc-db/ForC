# Resolving https://github.com/forc-db/ForC/issues/21
# --> Add Regentype and Managementtype codes by merging one of Maria's version of the db into the public one.

rm(list = ls())
setwd("C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/forc-db/")

# Load maria's version of measurements table

db_lifeform <- read.csv("ForC_private/archived database versions/ForC_2016-02/ForC_measurements_lifeform.csv", stringsAsFactors = F)

# Load public verion of measurements table

db <- read.csv("ForC/data/ForC_measurements.csv", stringsAsFactors = F)


#Find out what field can be used for merging --> sites.sitename and plot.name

which(tapply(db_lifeform$UM, list(db_lifeform$sites.sitename, db_lifeform$plot.name), function(x) length(unique(x))) > 1, arr.ind = TRUE)

which(tapply(db_lifeform$Regentype, list(db_lifeform$sites.sitename, db_lifeform$plot.name), function(x) length(unique(x))) > 1, arr.ind = TRUE)



# MERGE Managementtype and Regentype into extratropicals of public version, leaving NA when no match

dim(db)
dim(unique(db[, c("sites.sitename", "plot.name")]))

db$ID <-  paste(db$sites.sitename, db$plot.name)
# removing tropical sites and extratropicals that already have managementtype and regentype
db_extratropical <- db[db$tropical.extratropical == "extratropical",]
db_extratropical <- db_extratropical[db_extratropical$Managementtype == "NAC" & db_extratropical$Regentype == "NAC",]


db_lifeform$ID <- paste(db_lifeform$sites.sitename, db_lifeform$plot.name)
db_lifeform$Managementtype <- db_lifeform$UM

table(db_extratropical$Managementtype)
table(db_extratropical$Regentype)

db_extratropical$Managementtype <- NULL
db_extratropical$Regentype <- NULL


## Fixing sites.sitename to be able to merge

sum(!db_extratropical$ID  %in% db_lifeform$ID)
unique(db_extratropical$ID[!db_extratropical$ID  %in% db_lifeform$ID])
good_ID <- unique(db_extratropical$ID[!db_extratropical$ID  %in% db_lifeform$ID])


## Ailao
unique(grep("Ailao", db_lifeform$ID, value = T))
db_lifeform[grep("Ailao Lithocarpus chintungensis", db_lifeform$ID),]
db_lifeform[grep("Ailao Lithocarpus chintungensis", db_lifeform$ID),]$ID <- good_ID[1]

## Chippewa
unique(grep("Chippewa", db_lifeform$ID, value = T))
db_lifeform[grep("Chippewa National Forest chronosequence 131 years", db_lifeform$ID),]
db_extratropical[grep("Chippewa National Forest chronosequence 131 years", db_extratropical$ID),]
a <- db_extratropical[grep("Chippewa National Forest chronosequence 131 years_a", db_extratropical$ID),]$mean
b <- db_extratropical[grep("Chippewa National Forest chronosequence 131 years_b", db_extratropical$ID),]$mean

db_lifeform[db_lifeform$ID == "Chippewa National Forest chronosequence 131 years" & db_lifeform$mean %in% a,]$ID <- good_ID[2]

db_lifeform[db_lifeform$ID == "Chippewa National Forest chronosequence 131 years" & db_lifeform$mean %in% b,]$ID <- good_ID[3]

# Harvard
unique(grep("Harvard", db_lifeform$ID, value = T))
unique(db_lifeform[grep("Harvard", db_lifeform$ID),]$ID)
db_lifeform[db_lifeform$ID == "Harvard Forest EMS, Prospect Hill UM",]$ID <- good_ID[4]

# Holme
unique(grep("Holme", db_lifeform$ID, value = T))
db_extratropical[grep("Holme", db_extratropical$ID),]
db_lifeform[db_lifeform$ID == "Holme Fen Nature Reserve Woodland establishment on a drained fen",]$ID <- "Holme Fen Nature Reserve birch"
db_lifeform[db_lifeform$ID == "Holme Fen Nature Reserve birch",]$ID <- paste0(db_lifeform[db_lifeform$ID == "Holme Fen Nature Reserve birch",]$ID, " stand_age", db_lifeform[db_lifeform$ID == "Holme Fen Nature Reserve birch",]$stand.age)

# Lucknow
db_lifeform[grep("10000 plants", db_lifeform$ID),]$ID <- good_ID[14]
db_lifeform[grep("20000 plants", db_lifeform$ID),]$ID <- good_ID[15]
db_lifeform[grep("30000 plants", db_lifeform$ID),]$ID <- good_ID[16]



# Loop through each ID to fill in the Managementtype and Regentype

for(ID in unique(db_extratropical$ID)){
  print(ID)
  if(nrow(db_lifeform[db_lifeform$ID == ID,]) > 0) {
  Regentype <- db_lifeform[db_lifeform$ID == ID,]$Regentype
  Managementtype <- db_lifeform[db_lifeform$ID == ID,]$Managementtype
  if(!(all(Regentype == Regentype[1]) & all(Managementtype == Managementtype[1]))) { stop("Problem with multiple choices")}
  db[db$ID == ID, ]$Regentype <- Regentype[1]
  db[db$ID == ID, ]$Managementtype <- Managementtype[1]
  }
  
}


# SAVE
db$ID <- NULL

# write.csv(db, file = "ForC/data/ForC_measurements.csv", row.names = FALSE)



