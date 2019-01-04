#compare master version of MEASUREMENTS and beckybanbury's version just after she ran ID_sets_of_duplicate_records.R. This code is to make sure that overwritting D.precedence (for now, and other duplicated related columns for later) happened only where it was meant to happen.

library(RCurl)

meas.becky <- read.csv(text = getURL("https://raw.githubusercontent.com/forc-db/ForC/beckybanbury/data/ForC_measurements.csv"),  stringsAsFactors = F)
meas.master <- read.csv(text = getURL("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_measurements.csv"),  stringsAsFactors = F)


duplicate.related.columns <- c("measurement.ID","conflicts", "R.group", "S.group", "D.group", 
                               "D.precedence", "conflict.type", "D.precedence.measurement.ID", 
                               "conflicts.notes", "checked.ori.pub", "loaded.by", "flag.suspicious")

all(meas.becky$measurement.ID == meas.master$measurement.ID) # has yo be TRUE to move forward

original.duplicate.related.column.values <- meas.master[, duplicate.related.columns]
new.duplicate.related.column.values <- meas.becky[, duplicate.related.columns]


not.the.same.ones <- which(apply(original.duplicate.related.column.values == new.duplicate.related.column.values, 1, function(x) any(!x[!is.na(x)])))

what.not.the.same.ones <- apply(original.duplicate.related.column.values == new.duplicate.related.column.values, 1, function(x) names(x)[!x & !is.na(x)])
table(unlist(what.not.the.same.ones[not.the.same.ones]))

what.not.the.same.ones.D.precedence <- apply(original.duplicate.related.column.values == new.duplicate.related.column.values, 1, function(x) "D.precedence" %in% names(x)[!x & !is.na(x)])


table(original.duplicate.related.column.values[what.not.the.same.ones.D.precedence,]$loaded.by) # different D.precendence with newer entries is normal.

#lets look at D.groups that involve older data: ####

original.duplicate.related.column.values[what.not.the.same.ones.D.precedence & original.duplicate.related.column.values$loaded.by %in% c("Kristina J. Anderson-Teixeira" ,  "Maria M. H. Wang" ),]$D.group
new.duplicate.related.column.values[what.not.the.same.ones.D.precedence & original.duplicate.related.column.values$loaded.by %in% c("Kristina J. Anderson-Teixeira" ,  "Maria M. H. Wang" ),]$D.group

## D.group 432 and others related to it ####
X.group = 432
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
original.duplicate.related.column.values[grepl(pattern.X.group, original.duplicate.related.column.values$D.group), ]
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]

X.group = 1411
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])

X.group1 = 432;  X.group2= 1411; X.group3 = 1421; X.group4 = 1422; X.group5 = 1423; X.group6 = 1424; X.group7 = 1426; X.group8 = 1427; X.group9 = 1428; X.group10 = 14219
all.X.group <- unlist(mget(paste0("X.group", 1:10)))
pattern.X.group <- paste0(paste0("(^",all.X.group, "$)|(^",all.X.group, ";)|(;", all.X.group, ";)|(;", all.X.group, "$)"), collapse = "|")
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])

# D.group 433 and others related to it ####
X.group = 433
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
original.duplicate.related.column.values[grepl(pattern.X.group, original.duplicate.related.column.values$D.group), ]
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]

new.duplicate.related.column.values[new.duplicate.related.column.values$measurement.ID %in% original.duplicate.related.column.values[grepl(pattern.X.group, original.duplicate.related.column.values$D.group), ]$measurement.ID,] # group 433 has been replaced by group 1411 because of new data from Becky Banbury Morgan (Beckybanbury)

X.group = 1411
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])



# D.group 489 and others related to it ####
X.group = 489
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
original.duplicate.related.column.values[grepl(pattern.X.group, original.duplicate.related.column.values$D.group), ]
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]# group 490 has been replaced by group 1616 because of new data from Becky Banbury Morgan (Beckybanbury)

X.group = 1616
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])



X.group1 = 489;  X.group2= 1616; X.group3 = 1625; X.group4 = 1629
all.X.group <- unlist(mget(paste0("X.group", 1:4)))
pattern.X.group <- paste0(paste0("(^",all.X.group, "$)|(^",all.X.group, ";)|(;", all.X.group, ";)|(;", all.X.group, "$)"), collapse = "|")
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])

# D.group 475 and others related to it ####
X.group = 475
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
original.duplicate.related.column.values[grepl(pattern.X.group, original.duplicate.related.column.values$D.group), ]
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]# group 476 has been replaced by group 1577 because of new data from Becky Banbury Morgan (Beckybanbury)

X.group = 1577
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])



X.group1 = 475;  X.group2= 1577; X.group3 = 1585; X.group4 = 1586; X.group5 = 1587; X.group6 = 1588; X.group7 = 1589; X.group8 = 1590; X.group9 = 1591; X.group10 = 1592; X.group11 = 1593
all.X.group <- unlist(mget(paste0("X.group", 1:11)))
pattern.X.group <- paste0(paste0("(^",all.X.group, "$)|(^",all.X.group, ";)|(;", all.X.group, ";)|(;", all.X.group, "$)"), collapse = "|")
new.duplicate.related.column.values[grepl(pattern.X.group, new.duplicate.related.column.values$D.group), ]
View(meas.becky[grepl(pattern.X.group, meas.becky$D.group), ])


# D.group NAC and others related to it ####
View(original.duplicate.related.column.values[what.not.the.same.ones.D.precedence & original.duplicate.related.column.values$loaded.by %in% c("Kristina J. Anderson-Teixeira" ,  "Maria M. H. Wang")& original.duplicate.related.column.values$D.group %in% "NAC",])
View(meas.becky[what.not.the.same.ones.D.precedence & original.duplicate.related.column.values$loaded.by %in% c("Kristina J. Anderson-Teixeira" ,  "Maria M. H. Wang" ) & original.duplicate.related.column.values$D.group %in% "NAC",])


save(MEASUREMENTS.split, file = "scripts/z_archive/MEASUREMENTS.split.Rdata")
load("scripts/z_archive/MEASUREMENTS.split.Rdata")
