# Resolving https://github.com/forc-db/ForC/issues/22
# --> Check for and correct any records where different-aged stands are identified by a single plot


rm(list = ls())
setwd("C:/Users/HerrmannV/Dropbox (Smithsonian)/GitHub/forc-db/")

# Load public verion of measurements table

db <- read.csv("ForC/data/ForC_measurements.csv", stringsAsFactors = F)

# Load public version of plot history

ForC_history <- read.csv("ForC/data/ForC_history.csv", stringsAsFactors = F)

# Find records that have same sites.sitename, same plot.name same date of record but with more than 1 unique stand age
db$ID <- paste(db$sites.sitename, db$plot.name, db$date)
ForC_history$ID <- paste(ForC_history$sites.sitename, ForC_history$plot.name, ForC_history$date)

db_all <- db # save complete db
db <- db[!is.na(db$date) & !db$date %in% c("NI", "NRA", "date", "NAC"),] # remove unknown dates from db


ID_with_problems <- rownames(which(tapply(db$stand.age, db$ID, function(x) length(unique(x))) > 1, arr.ind = TRUE))

# Give new plot.name to records with same sites.sitename, same plot.name same date of record but with more than 1 unique stand age  + add corresponding rows into ForC_history

db <- db_all # to make changes to full db
template_new_history <- ForC_history[1,]; template_new_history[1,] <- c(rep(NA, 16))


for(problematic_id in ID_with_problems){
print(problematic_id)
db[db$ID == problematic_id,]$plot.name <- paste0(db[db$ID == problematic_id,]$plot.name, "_age", db[db$ID == problematic_id,]$stand.age)

x <- db[db$ID == problematic_id,]

for(i in unique(x$plot.name)){
  
  x1 <- x[x$plot.name == i,]
  
  template_new_history$sites.sitename <- unique(x1$sites.sitename)
  template_new_history$plot.name  <-  unique(x1$plot.name)
  template_new_history$date <- unique(x1$date)
 
  ForC_history<- rbind(ForC_history, template_new_history) 
}
}

tail(ForC_history, 47)


# save
## first remove ID column
db$ID <- NULL
ForC_history$ID <- NULL

## Then save

# write.csv(db, file = "ForC/data/ForC_measurements.csv", row.names = FALSE)
# write.csv(ForC_history, file = "ForC/data/ForC_history.csv", row.names = FALSE)
