######################################################
# Purpose: fill in the conflicts, R.group, S.group, D.group, D.precedence, conflict.type and conflict.notes fields of MEASUREMENTS table to make it easier to ID the duplicates sets that we should look at to reconcile duplicates records. D.precedence is given but some records are left to Krista to assign manually
# Inputs:
# - MEASUREMENTS table
# outputs: - MEASUREMENTS table with updated duplicate system coding
# IMPORTANT NOTE: - THE OUTPUT WAS EDITED BY HAND BY KRISTINA ANDERSON-TEIXEIRA, "I've finished assigning D.precedence. I edited some records by hand (and added conflict.notes). There were a couple instances where I changed fields other than D.precedence.". SO WHEN RUNNING THIS CODE AGAIN, OUTPUT SHOULD BE COMPARED TO ORIGINAL AND DECISIONS NEEDS TO BE MADE ON THE CASE BY CASE BASIS FOR CONFLICTS.
# Developped by: Valentine Herrmann - HerrmannV@si.edu in April 2018
# R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)


# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

sets.of.columns.to.keep.at.the.end <- names(MEASUREMENTS)

# Prepare data ####

## Ignore _OM and _Oc in variables names ####
MEASUREMENTS$variable.name.combined <- MEASUREMENTS$variable.name
MEASUREMENTS$variable.name.combined <- gsub("(\\w*)(_C$|_OM$)", "\\1", MEASUREMENTS$variable.name.combined, perl = T)

## Create a better date column ####
MEASUREMENTS$my.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS$date)))
MEASUREMENTS$my.start.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS$start.date)))
MEASUREMENTS$my.end.date <- as.Date(date_decimal(as.numeric(MEASUREMENTS$end.date)))

### consider my.date as NA if start and end date are given
MEASUREMENTS$my.date <- ifelse(!my_is.na(MEASUREMENTS$my.start.date) & !my_is.na(MEASUREMENTS$my.end.date), NA,  MEASUREMENTS$my.date)
class(MEASUREMENTS$my.date) <- "Date"

### take floor of all dates (take January first)
MEASUREMENTS$my.date <- year(MEASUREMENTS$my.date)
MEASUREMENTS$my.date <- ifelse(my_is.na(MEASUREMENTS$my.date), NA, paste0(MEASUREMENTS$my.date, "-01-01"))
MEASUREMENTS$my.date <- as.Date(MEASUREMENTS$my.date)

MEASUREMENTS$my.start.date <- year(MEASUREMENTS$my.start.date)
MEASUREMENTS$my.start.date <- ifelse(my_is.na(MEASUREMENTS$my.start.date), NA, paste0(MEASUREMENTS$my.start.date, "-01-01"))
MEASUREMENTS$my.start.date <- as.Date(MEASUREMENTS$my.start.date)

MEASUREMENTS$my.end.date <- year(MEASUREMENTS$my.end.date)
MEASUREMENTS$my.end.date <- ifelse(my_is.na(MEASUREMENTS$my.end.date), NA, paste0(MEASUREMENTS$my.end.date, "-01-01"))
MEASUREMENTS$my.end.date <- as.Date(MEASUREMENTS$my.end.date)

### if start and end date are the same day (january 1st of the same year) remove them and put that date in date ####
idx <- !is.na(MEASUREMENTS$my.start.date) & (MEASUREMENTS$my.start.date == MEASUREMENTS$my.end.date)
MEASUREMENTS$my.date[idx] <- MEASUREMENTS$my.start.date[idx]
MEASUREMENTS$my.start.date[idx] <- NA
MEASUREMENTS$my.end.date[idx] <- NA

## consider NA as a factor level for plot.names (otherwise doesn't work when splitting the data) ####
MEASUREMENTS$plot.name <- addNA(MEASUREMENTS$plot.name)

# Id and remove orphan conflicts (S,R,D groups that have only one records because other records in the group were deleted at some point)####

## ID orphan conflicts 
orphan.R.group <- NULL
MEASUREMENTS$R.group <- as.character(MEASUREMENTS$R.group)
for(X.group in sort(my_na.omit(unique(unlist(strsplit(MEASUREMENTS$R.group, ";")))))) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$R.group), ]
  if(nrow(X) == 1) orphan.R.group <- c(orphan.R.group, X.group)
}

orphan.S.group <- NULL
for(X.group in sort(my_na.omit(unique(unlist(strsplit(MEASUREMENTS$S.group, ";")))))) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$S.group), ]
  if(nrow(X) == 1) orphan.S.group <- c(orphan.S.group, X.group)
}

orphan.D.group <- NULL
for(X.group in sort(my_na.omit(unique(unlist(strsplit(MEASUREMENTS$D.group, ";")))))) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$D.group), ]
  if(nrow(X) == 1) orphan.D.group <- c(orphan.D.group, X.group)
}

## remove orphan conflicts

for(X.group in orphan.R.group) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$R.group), ]
  X$conflicts <- gsub("R,|,R|R", "",  X$conflicts)
  X$conflicts <- ifelse(X$conflicts %in% "", "I", X$conflicts)
  if(all(is.na(X[, c("S.group", "D.group")]))) X$conflict.type <- NA
  X$R.group <- NA
  MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$R.group), ] <- X
}

for(X.group in orphan.S.group) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$S.group), ]
  X$conflicts <- gsub("S,|,S|S", "",  X$conflicts, ignore.case = T)
  X$conflicts <- ifelse(X$conflicts %in% "", "I", X$conflicts)
  if(all(is.na(X[, c("R.group", "D.group")]))) X$conflict.type <- NA
  X$S.group <- NA
  MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$S.group), ] <- X
}

for(X.group in orphan.D.group) {
  pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
  X <- MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$D.group), ]
  X$conflicts <- gsub("D,|,D|D", "",  X$conflicts)
  X$conflicts <- ifelse(X$conflicts %in% "", "I", X$conflicts)
  if(all(is.na(X[, c("R.group", "S.group")]))) X$conflict.type <- NA
  X$D.group <- NA
  X$D.precedence <- NA
  X$D.precedence.measurement.ID <- NA
  MEASUREMENTS[grepl(pattern.X.group, MEASUREMENTS$D.group), ] <- X
}

# Keep a copy of original values in the duplicate related columns ####
duplicate.related.columns <- c("measurement.ID","conflicts", "R.group", "S.group", "D.group", 
                               "D.precedence", "conflict.type", "D.precedence.measurement.ID", 
                               "conflicts.notes", "checked.ori.pub", "loaded.by", "flag.suspicious")

original.duplicate.related.column.values <- MEASUREMENTS[, duplicate.related.columns]

# remove "NAC" in columns "conflicts", "R.group", "S.group", "D.group", "conflict.type"
# MEASUREMENTS[, c("conflicts", "R.group", "S.group", "D.group", "conflict.type")][MEASUREMENTS[, c("conflicts", "R.group", "S.group", "D.group", "conflict.type")] == "NAC"] <- NA

# Keep a copy inside the object of old group ID so that we can reuse them in the process
old_columns <- MEASUREMENTS[,  c("conflicts", "R.group", "S.group", "D.group", "conflict.type", "conflicts.notes", "D.precedence", "D.precedence.measurement.ID")]
names(old_columns) <- paste0("old_", names(old_columns))
MEASUREMENTS <- cbind(MEASUREMENTS, old_columns)

# Put NA in "conflicts", "R.group", "S.group", "D.group", "conflict.type" to not be influenced by old values (but we will be able to look at ID thanks to lines above)
MEASUREMENTS[, c("conflicts", "R.group", "S.group", "D.group", "conflict.type", "conflicts.notes")] <- NA # not D.precedence nore D.precedence.measurement.ID



# Split Measurement by site plot and variables name ####

MEASUREMENTS.split <- split(MEASUREMENTS, list(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name, MEASUREMENTS$variable.name.combined), drop = TRUE)


# RUN THE CODE TO ID SETS OF DUPLICATE RECORDS ####

MEASUREMENTS.final <- NULL

new.R.group.ID <- ifelse(any(!my_is.na(MEASUREMENTS$old_R.group)), max(as.numeric(MEASUREMENTS$old_R.group), na.rm = T), 0)
new.S.group.ID <- ifelse(any(!my_is.na(MEASUREMENTS$old_S.group)), max(as.numeric(MEASUREMENTS$old_S.group), na.rm = T), 0)
new.D.group.ID <- ifelse(any(!my_is.na(MEASUREMENTS$old_D.group)), max(as.numeric(MEASUREMENTS$old_D.group), na.rm = T), 0)

more.thn.one.D.precedence.measurement.ID.given.and.not.easy.case.split.ID <- NULL

for(i in 1:length(MEASUREMENTS.split)){
  
  X <- MEASUREMENTS.split[[i]]
  X$split.ID <- i
  
  any.duplicated.dates <- any(duplicated(na.omit(X$my.date)))
  # any.duplicated.start.dates <- any(duplicated(na.omit(X$my.start.date)))
  any.duplicated.stand.age <- any(duplicated(my_na.omit(X$stand.age)))
  
  all.types.dates.NA <- all(my_is.na(c(X$my.date, X$my.start.date)))
  all.dates.NA <- all(my_is.na(X$my.date))
  all.start.dates.NA <-  all(my_is.na(X$my.start.date))
  all.stand.age.NA <- all(my_is.na(X$stand.age))
  
  
  any.all.types.dates.NA <- any(paste0(X$my.date, X$my.start.date) %in% "NANA")
  any.dates.NA <- any(my_is.na(X$my.date))
  any.start.dates.NA <-  any(my_is.na(X$my.start.date))
  any.stand.age.NA <- any(my_is.na(X$stand.age))
  
  only.dates <- !all.dates.NA & all.start.dates.NA
  only.range <- !all.start.dates.NA & all.dates.NA
  
  any.range <- !all.start.dates.NA
  
  dates.and.ranges <- !all.dates.NA & !all.start.dates.NA
  
  sum.of.range.records <- sum(!my_is.na(X$my.start.date))
  
  if (nrow(X) == 1) X$conflicts <- paste(X$conflicts, "I", sep = ";") # if only one record for that plot and that variable --> independant record
  
  if (nrow(X) > 1) { # if more than one record...  
    
    ## If any duplicated dates or start.date (excluding NA) OR, if all dates are missing, if any duplicated stand.gae (excluding NA)
    if (any.duplicated.dates | (all.types.dates.NA & any.duplicated.stand.age)) { # if any duplicated dates or start.date or stand.age (if missing all dates),  (excluding NA)
      
      if (any.duplicated.dates)  date.to.look.at <- "my.date"
      # if (any.duplicated.start.dates)  date.to.look.at <- "my.start.date"; n.loop = 1
      if (all.types.dates.NA & any.duplicated.stand.age) date.to.look.at <- "stand.age"
      
      # if (any.duplicated.dates & any.duplicated.start.dates) date.to.look.at <- "my.date"; n.loop = 2
      #   {
      #   print(X)
      #   warning("need to improve code for that case...", immediate. = T)
      #   readline("press [enter]")
      # }
      
      # for(l in 1:n.loop) { # loop if there is both duplicated dates and duplicated start.dates
      #   
      #   if (l == 1) date.to.look.at <- date.to.look.at
      #   if (l == 2) date.to.look.at <- "my.start.date"
      
      X.split <- split(X, addNA(X[, date.to.look.at])) # split by date
      
      for(s in 1:length(X.split)){ # look into each date subset
        
        x <- X.split[[s]]
        
        if (!my_is.na(names((X.split))[s])) { # if date is not NA, care about. otherwise, don't do anything, it will be taken care of in another case
          if (nrow(x) == 1) x$conflicts = paste(x$conflicts, "I", sep = ";" ) # if only one record per date --> independant record
          
          if (nrow(x) > 1 ) { # if more than one record, need to look more into it...
            
            #   if (length(unique(x$stand.age)) != length(unique(x[, ifelse(date.to.look.at == "my.date", "my.date", "start.date")]))) { # if one stand.age is not associated to one date
            #   print(x)
            #   warning(paste("Problem of stand.age and date for i =", i))
            #   readline("press[enter]")
            # } # if one stand.age is not associated to one date
            
            all.same.citation <- length(unique(x$citation.ID)) == 1 # are all record the from the same study ?
            all.same.method <- length(unique(x$method.ID)) == 1  # are all record the from the same method ?
            all.same.notes <-  length(unique(x$notes)) == 1 # have all record the same note ?
            all.same.C.units <- length(unique(x$variable.name)) == 1 # have all record the same carbon units ?
            all.same.depth <- length(unique(x$depth)) == 1 # have all record the same depth ?
            all.same.dbh <- length(unique(x$min.dbh)) == 1# have all record the same min.dbh ?
            
            
            # only replicates (all same study & all same method & all same notes & all same carbon units)
            # if (all.same.citation & all.same.method & all.same.notes & all.same.C.units & all.same.start.date & all.same.end.date & all.same.depth)
            if (all.same.citation & all.same.method & all.same.notes & all.same.C.units & all.same.depth & all.same.dbh) { # if only replicates
              
              # if(length(unique(x$R.group)) == 1 & all(!my_is.na(x$R.group))) R.group.ID.to.add <- unique(x$R.group)
              if(any(table(unlist(strsplit(unique(x$old_R.group), ";"))) == length(unique(x$old_R.group))) & all(!my_is.na(x$old_R.group))) {
                
                # R.group.ID.to.add <- unique(x$R.group)
                R.group.ID.to.add <- names(which(table(unlist(strsplit(unique(x$old_R.group), ";"))) == length(unique(x$old_R.group))))
                if(length(R.group.ID.to.add) > 1) stop()
              } 
              
              if(!(any(table(unlist(strsplit(unique(x$old_R.group), ";"))) == length(unique(x$old_R.group))) & all(!my_is.na(x$old_R.group)))) {
                # print(x)
                # readline("case were we need for a new R.group")
                new.R.group.ID <- new.R.group.ID +1
                R.group.ID.to.add <- new.R.group.ID
              }
              
              x$R.group <- paste(x$R.group, R.group.ID.to.add, sep = ";") # paste(i, s, "only replicates") # give a R.group_ID
              x$conflicts <- paste(x$conflicts, "R", sep = ";" ) # give a conflict value (beside existing one)
            }  # if only replicates
            
            # Not only replicates (and maybe not at all)...
            # if (any(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.start.date, !all.same.end.date, !all.same.depth))
            if (any(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh)) { # if not only replicates
              # if(length(unique( x$D.group )) == 1 & all(!my_is.na( x$D.group ))) D.group.ID.to.add <- unique( x$D.group )
              if(any(table(unlist(strsplit(unique( x$old_D.group), ";"))) == length(unique( x$old_D.group))) & all(!my_is.na( x$old_D.group))) {
                # D.group.ID.to.add <- unique( x$D.group )
                D.group.ID.to.add <- names(which(table(unlist(strsplit(unique(x$old_D.group), ";"))) == length(unique(x$old_D.group))))
                if(length(D.group.ID.to.add) > 1) stop()
              }  
              
              if(!(any(table(unlist(strsplit(unique( x$old_D.group), ";"))) == length(unique( x$old_D.group))) & all(!my_is.na( x$old_D.group)))) {
                # print(x)
                # readline("case where we need for a new D.group")
                new.D.group.ID <- new.D.group.ID +1
                D.group.ID.to.add <- new.D.group.ID
              }
              
              ## give D conflict value and D.group ID to all
              x$D.group <- paste(x$D.group, D.group.ID.to.add, sep = ";") # paste(i, s, "duplicates")
              x$conflicts <-  paste(x$conflicts, "D", sep = ";" ) # give a conflict value (beside existing one)
              
              ## ID sub groups of replicates, if any
              
              all.fields.to.look.at.pasted <- paste(x$variable.name, x$notes, x$method.ID, x$citation.ID, x$depth)
              # all.fields.to.look.at.pasted <- paste(x$variable.name, x$notes, x$method.ID, x$citation.ID, x$depth, ifelse(any.duplicated.start.dates, paste(x$start.date, x$end.date), ""))
              
              unique.all.fields.to.look.at.combinations <- as.data.frame(table(all.fields.to.look.at.pasted), stringsAsFactors = F)
              
              replicates.all.fields.to.look.at.combinations <- unique.all.fields.to.look.at.combinations[unique.all.fields.to.look.at.combinations$Freq > 1, ]$Var1 # look at combinations that are given to more than one record
              
              idx.replicates.amongs.duplicates <- all.fields.to.look.at.pasted %in% replicates.all.fields.to.look.at.combinations # identify those records that are not alone to have the method and citation combinations
              
              if (any(idx.replicates.amongs.duplicates)) { # if there are replicates in the group of duplicates
                
                # if(length(unique( x[idx.replicates.amongs.duplicates, ]$R.group)) == 1 & all(!my_is.na( x[idx.replicates.amongs.duplicates, ]$R.group))) R.group.ID.to.add <- unique( x[idx.replicates.amongs.duplicates, ]$R.group)
                if(any(table(unlist(strsplit(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group), ";"))) == length(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group))) & all(!my_is.na(x[idx.replicates.amongs.duplicates, ]$old_R.group))) {
                  # R.group.ID.to.add <- unique( x[idx.replicates.amongs.duplicates, ]$R.group)
                  R.group.ID.to.add <- names(which(table(unlist(strsplit(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group), ";"))) == length(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group))))
                  if(length(R.group.ID.to.add) > 1) stop()
                }
                
                if(!(any(table(unlist(strsplit(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group), ";"))) == length(unique(x[idx.replicates.amongs.duplicates, ]$old_R.group))) & all(!my_is.na(x[idx.replicates.amongs.duplicates, ]$old_R.group)))) {
                  # print(x[idx.replicates.amongs.duplicates,])
                  # readline("case were we need for a new R.group")
                  
                  new.R.group.ID <- new.R.group.ID +1
                  R.group.ID.to.add <- new.R.group.ID
                }
                
                x[idx.replicates.amongs.duplicates, ]$R.group <- paste(x[idx.replicates.amongs.duplicates, ]$R.group, R.group.ID.to.add, sep = ";") # paste(i, s, "replicates amongst duplicates") # give a R.group_ID
                
                x[idx.replicates.amongs.duplicates, ]$conflicts <- paste( x[idx.replicates.amongs.duplicates,]$conflicts, "R", sep = ";" ) # give a conflict value (beside existing one)
              } # if there are replicates in the group of duplicates
              
              ## Add conflict.type (same for all in group of duplicates, depends on what is not unique)
              
              # conflict.types <- unique(c("M", "M", "M", "C", "M", "T", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth,  !all.same.start.date, !all.same.end.date)])
              conflict.types <- unique(c("M", "M", "M", "C", "M", "M") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh)])
              x$conflict.type <- paste(x$conflict.types, paste(conflict.types, collapse = ";") , sep = ";" )
              
            } # if not only replicates
            
          }  # if more than one record, need to look more into it
        }  # if date is not NA, care about. otherwise, don't do anything, it will be taken car of in another case
        
        X.split[[s]] <- x
      } # look into each date subset
      
      X <- do.call(rbind, X.split)
      # } # loop if there is both duplicated dates and duplicated start.dates
      
      
    } # if any duplicated dates or start.date or stand.gae (if missing all dates),  (excluding NA)
    
    ## if any range record
    if (any.range) { # if dates and ranges together
      
      ### first look at if some ranges are in conflict
      
      idx.range.date <- which(!my_is.na(X$my.start.date))
      
      #### order by start date then end date.
      order.range.date <- order(X[idx.range.date,]$my.start.date, X[idx.range.date,]$my.end.date)
      idx.range.date <- idx.range.date[order.range.date]
      
      while(length(idx.range.date) > 1) { # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
        
        idx.x <- idx.range.date[1]
        idx.y <- idx.range.date[-1]
        
        x <- X[idx.x, ]
        y <- X[idx.y, ]
        
        # overlap <- !(x$my.start.date +1 > y$my.end.date  | y$my.start.date +1 > x$my.end.date)
        overlap <- (x$my.start.date -1 >= y$my.start.date & x$my.start.date -1<= y$my.end.date ) | (y$my.start.date + 1 >= x$my.start.date & y$my.start.date + 1 <= x$my.end.date ) | (x$my.start.date == y$my.start.date & x$my.end.date  == y$my.end.date )
        
        if (any(overlap)) { # if there is any overlap
          
          overlap.idx <- idx.range.date[-1][overlap]
          
          overlap.already.found <- any(table(as.numeric(unlist(sapply(as.character(X[c(idx.x, overlap.idx), "D.group"]), strsplit, ";")))) > 1)
          
          if (!overlap.already.found) { # if the overlap was not already found in previous rounds
            
            
            ## get this case index
            
            this.case.idx <- c(idx.x, overlap.idx)
            
            ## get what type of conflict we have 
            
            
            all.same.citation <- length(unique(X[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
            all.same.method <- length(unique(X[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
            all.same.notes <-  length(unique(X[this.case.idx, ]$notes)) == 1 # have all record the same note ?
            all.same.C.units <- length(unique(X[this.case.idx,]$variable.name)) == 1 # have all record the same carbon units ?
            all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
            all.same.dbh <- length(unique(X[this.case.idx,]$min.dbh)) == 1# have all record the same min.dbh ?
            
            all.same.start.date <- length(unique(X[this.case.idx,]$my.start.date)) == 1
            all.same.end.date <- length(unique(X[this.case.idx,]$my.end.date)) == 1
            exact.overlap <- all.same.start.date & all.same.end.date
            
            conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, !exact.overlap)])
            
            ## If everything is the same, it is a replicate
            if(length(conflict.types) == 0) { # if it is a replicate
              
              # if(length(unique(X[this.case.idx, "R.group"])) == 1 & all(!my_is.na(X[this.case.idx, "R.group"]))) R.group.ID.to.add <- unique(X[this.case.idx, "R.group"])
              if(any(table(unlist(strsplit(as.character(unique(X[this.case.idx, "old_R.group"])), ";"))) == length(unique(X[this.case.idx, "old_R.group"]))) & all(!my_is.na(X[this.case.idx, "old_R.group"]))) {
                # R.group.ID.to.add <- unique(X[this.case.idx, "R.group"])
                R.group.ID.to.add <- names(which(table(unlist(strsplit(unique(X[this.case.idx, "old_R.group"]), ";"))) == length(unique(X[this.case.idx, "old_R.group"]))))[1]
                # if(length(R.group.ID.to.add) > 1) stop()
              } 
              
              if(!(any(table(unlist(strsplit(as.character(unique(X[this.case.idx, "old_R.group"])), ";"))) == length(unique(X[this.case.idx, "old_R.group"]))) & all(!my_is.na(X[this.case.idx, "old_R.group"])))) {
                # print(  X[this.case.idx, ])
                # readline("case where we need for a new R.group")
                new.R.group.ID <- new.R.group.ID +1
                R.group.ID.to.add <- new.R.group.ID
              }
              
              
              X[this.case.idx, "R.group"] <- paste( X[this.case.idx, "R.group"], R.group.ID.to.add, sep = ";" )
              X[this.case.idx, "conflicts"] <- paste( X[this.case.idx, "conflicts"], "R", sep = ";" )
              
            } # if it is a replicate
            
            
            ## If NOT everything is the same, it is a duplicate
            if(length(conflict.types) > 0) { # if it is a duplicate
              
              # if(length(unique(X[this.case.idx, "D.group"])) == 1 & all(!my_is.na(X[this.case.idx, "D.group"]))) D.group.ID.to.add <- unique(  X[this.case.idx, "D.group"])
              if(any(table(unlist(strsplit(unique(X[this.case.idx, "old_D.group"]), ";"))) == length(unique(X[this.case.idx, "old_D.group"]))) & all(!my_is.na(X[this.case.idx, "old_D.group"]))) {
                # D.group.ID.to.add <- unique(X[this.case.idx, "D.group"])
                D.group.ID.to.add <- names(which(table(unlist(strsplit(unique(X[this.case.idx, "old_D.group"]), ";"))) == length(unique(X[this.case.idx, "old_D.group"]))))
                if(length(D.group.ID.to.add) > 1) {
                  new.D.group.ID <- new.D.group.ID +1
                  D.group.ID.to.add <- new.D.group.ID
                }
              }  
              
              if(!(any(table(unlist(strsplit(unique(X[this.case.idx, "old_D.group"]), ";"))) == length(unique(X[this.case.idx, "old_D.group"]))) & all(!my_is.na(X[this.case.idx, "old_D.group"])))) {
                # print( X[this.case.idx, ])
                # readline("case where we need for a new D.group")
                new.D.group.ID <- new.D.group.ID +1
                D.group.ID.to.add <- new.D.group.ID
              }
              
              
              X[this.case.idx, "D.group"] <- paste( X[this.case.idx, "D.group"], D.group.ID.to.add, sep = ";" )
              X[this.case.idx, "conflicts"] <- paste( X[this.case.idx, "conflicts"], "D", sep = ";" )
              X[this.case.idx, "conflict.type"] <- paste(X[this.case.idx, "conflict.type"], paste(conflict.types, collapse = ";") , sep = ";" )
              
              
            } # if it is a duplicate
            
            
          } # if the overlap was not already found in previous rounds
          
        } # if there is any overlap
        
        idx.range.date <- idx.range.date[-1] # remove range of focus so that we don't look at it again
        
      }  # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
      
      ### Second, for each range, look at if there is a 1-to-1 or 1-to-many
      idx.range.date <- which(!my_is.na(X$my.start.date))
      
      for(r in 1:length(idx.range.date)) { # loop through each range
        
        idx.y <- idx.range.date[-r]
        
        if (length(idx.range.date) == 1) x <- X
        if (length(idx.range.date) > 1) x <- X[-idx.y, ]
        
        ### get the range of dates
        the.one.start.date <- na.omit(unique(x$my.start.date))
        the.one.end.date <- na.omit(unique(x$my.end.date))
        
        ### get the idx of the different type of dates
        idx.range.date.subset <- which(x$my.start.date %in% the.one.start.date)
        idx.non.range.dates <- which(my_is.na(x$my.start.date)) #including NA
        
        ### what dates are within the range or NA ?
        dates.within.range <- !is.na(x$my.date[idx.non.range.dates] ) & (x$my.date[idx.non.range.dates] >= the.one.start.date & x$my.date[idx.non.range.dates] <= the.one.end.date)
        dates.oustide.range <- !is.na(x$my.date[idx.non.range.dates] ) & (x$my.date[idx.non.range.dates] < the.one.start.date | x$my.date[idx.non.range.dates] > the.one.end.date)
        dates.NA <- my_is.na(x$my.date[idx.non.range.dates])
        
        ## what type of conflict do we have
        no.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) == 0
        one.to.one.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) == 1
        one.to.many.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) > 1 
        
        if (length(idx.range.date.subset) != 1) stop("Error: problem in coding, length(idx.range.date.subset) should be 1")
        
        if (no.conflict)  x$conflicts <- paste(x$conflicts, "I", sep = ";" )
        
        if (one.to.one.conflict) { # if 1-to-1 conflict
          
          ## give D conflict value and D.group ID to all non-NA
          
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          # if(length(unique(x$D.group[this.case.idx])) == 1 & all(!my_is.na(x$D.group[this.case.idx]))) D.group.ID.to.add <- unique(x$D.group[this.case.idx])
          if(any(table(unlist(strsplit(unique(x$old_D.group[this.case.idx]), ";"))) == length(unique(x$old_D.group[this.case.idx]))) & all(!my_is.na(x$old_D.group[this.case.idx]))) {
            
            # D.group.ID.to.add <- unique(x$D.group[this.case.idx])
            D.group.ID.to.add <- names(which(table(unlist(strsplit(unique(x$old_D.group[this.case.idx]), ";"))) == length(unique(x$old_D.group[this.case.idx]))))
            if(length(D.group.ID.to.add) > 1) {
              new.D.group.ID <- new.D.group.ID +1
              D.group.ID.to.add <- new.D.group.ID
            }
          } 
          
          if(!(any(table(unlist(strsplit(unique(x$old_D.group[this.case.idx]), ";"))) == length(unique(x$old_D.group[this.case.idx]))) & all(!my_is.na(x$old_D.group[this.case.idx])))) {
            # print(x[this.case.idx,])
            # readline("case were we need for a new D.group")
            new.D.group.ID <- new.D.group.ID +1
            D.group.ID.to.add <- new.D.group.ID
          }
          
          
          x$D.group[this.case.idx] <- paste( x$D.group[this.case.idx], D.group.ID.to.add, sep = ";") # paste(i, s, "duplicates")
          x$conflicts[this.case.idx] <-  paste(x$conflicts[this.case.idx], "D", sep = ";" ) # give a conflict value (beside existing one)
          
          ## give the conflict.type values T (besides others)
          
          all.same.citation <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx,]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          all.same.dbh <- length(unique(X[this.case.idx,]$min.dbh)) == 1# have all record the same min.dbh ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ";") , sep = ";" )
          
        } # if 1-to-1 conflict
        
        if (one.to.many.conflict) { # if 1-to-many conflict
          
          # if(length(unique(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])])) == 1 & all(!my_is.na(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]))) S.group.ID.to.add <- unique(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])])
          
          if(any(table(unlist(strsplit(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]), ";"))) == length(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]))) & all(!my_is.na(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]))) {
            # S.group.ID.to.add <- unique(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])])
            S.group.ID.to.add <- names(which((table(unlist(strsplit(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]), ";"))) == length(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])])))))
            if(length(S.group.ID.to.add) > 1) stop()
          }
          
          if(!(any(table(unlist(strsplit(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]), ";"))) == length(unique(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])]))) & all(!my_is.na(x$old_S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])])))) {
            # print(x[c(idx.range.date.subset,idx.non.range.dates[dates.within.range]),])
            # readline("case where we need for a new S.group")
            new.S.group.ID <- new.S.group.ID +1
            S.group.ID.to.add <- new.S.group.ID
          }
          
          ## give s code to the dates that are within range
          x$conflicts[idx.non.range.dates[dates.within.range]] <- paste(x$conflicts[idx.non.range.dates[dates.within.range]], "s", sep = ";" ) # give a small s for those dates that are within the range (beside existing one)
          
          ## give S code to the range record
          x$conflicts[idx.range.date.subset] <- paste(x$conflicts[idx.range.date.subset], "S", sep = ";" ) # give a capital s for the record that is the range
          x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] <- paste(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] , S.group.ID.to.add, sep = ";") # paste(i, "one-to-many-conflict") # give a S.group_ID
          
          ## give the conflict.type values T (besides others)
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          all.same.citation <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx, ]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          all.same.dbh <- length(unique(X[this.case.idx,]$min.dbh)) == 1# have all record the same min.dbh ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ";") , sep = ";" )
        } # if 1-to-many conflict
        
        ### put back in X
        
        if (length(idx.range.date) == 1) X <- x
        if (length(idx.range.date) > 1) X[-idx.y, ] <- x
        
      } # for(r in 1:length(idx.range.date))
      
    } # if dates and ranges together
    
    
    ## missing dates amongst other records with dates
    if ((!all.types.dates.NA & any.all.types.dates.NA) | (all.types.dates.NA & any.stand.age.NA)) { # if any missing dates, give s category
      
      if (!all.types.dates.NA & any.all.types.dates.NA) idx.NA <- which(paste0(X$my.date, X$my.start.date) %in% "NANA")
      if (all.types.dates.NA & any.stand.age.NA) idx.NA <- which(my_is.na(X$stand.age))
      
      ## give S code to the dates that are NA
      X$conflicts[idx.NA] <- paste(X$conflicts[idx.NA], "S", sep = ";" )
      
      ## give s code to the range record
      X$conflicts[-idx.NA] <- paste(X$conflicts[-idx.NA], "s", sep = ";" )
      
      ## give S.group ID
      
      # if(length(unique(X$S.group)) == 1 & all(!my_is.na(X$S.group))) S.group.ID.to.add <- unique(X$S.group)
      if(any(table(unlist(strsplit(unique(X$old_S.group), ";"))) == length(unique(X$old_S.group))) & all(!my_is.na(X$old_S.group))) {
        # S.group.ID.to.add <- unique(X$S.group)
        S.group.ID.to.add <- names(which(table(unlist(strsplit(unique(X$old_S.group), ";"))) == length(unique(X$old_S.group))))
        if(length(S.group.ID.to.add) > 1) stop()
      }
      
      if(!(any(table(unlist(strsplit(unique(X$old_S.group), ";"))) == length(unique(X$old_S.group))) & all(!my_is.na(X$old_S.group)))) {
        # print(X)
        # if(!"Becky Banbury Morgan (Beckybanbury)" %in% X$loaded.by) readline("case where we need for a new S.group")
        new.S.group.ID <- new.S.group.ID +1
        S.group.ID.to.add <- new.S.group.ID
      }
      
      X$S.group <- paste(X$S.group, S.group.ID.to.add, sep = ";") # give a S.group_ID (same as others if any within this subset of data)
      
      ## give the conflict.type values T (besides others)
      
      ## give the conflict.type values T (besides others)
      
      all.same.citation <- length(unique(X$citation.ID)) == 1 # are all record the from the same study ?
      all.same.method <- length(unique(X$method.ID)) == 1  # are all record the from the same method ?
      all.same.notes <-  length(unique(X$notes)) == 1 # have all record the same note ?
      all.same.C.units <- length(unique(X$variable.name)) == 1 # have all record the same carbon units ?
      all.same.depth <- length(unique(X$depth)) == 1 # have all record the same depth ?
      all.same.dbh <- length(unique(X$min.dbh)) == 1# have all record the same min.dbh ?
      
      conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
      
      X$conflict.type <- paste(X$conflict.types, paste(conflict.types, collapse = ";") , sep = ";" )
      
    } # if any missing dates, give s category
    
    
  } # if more than one record...  
  
  ### Clean up the group codes ####
  
  #### conflicts
  x <- gsub("NA;|NAC;", "", X$conflicts)
  x <- gsub(";", "", x)
  x <-  gsub('(.)\\1+', '\\1', x)
  x <- sapply(strsplit(x, ""), function(a) paste(sort(unique(a)), collapse = ";")) # order letters and paste them with comma inbetween
  x <- gsub("(;I)|(I;)", "", x)
  x <- ifelse(x %in% "", NA, x)
  x <- ifelse(my_is.na(x), "I", x) # to get all the records that were not ID-ed as duplicates
  X$conflicts <- x
  
  #### conflict.type 
  x <- gsub("NA;|NAC;", "", X$conflict.type)
  x <- gsub(";", "", x)
  x <- gsub('(.)\\1+', '\\1', x)
  x <- sapply(strsplit(x, ""), function(a) paste(sort(unique(a)), collapse = ";")) # order letters and paste them with comma inbetween
  x <- ifelse(x %in% "", NA, x)
  x <- ifelse(x %in% "NA", NA, x)
  X$conflict.type <- x
  
  #### R.group 
  x <- gsub("NA;|NAC;", "", X$R.group)
  x <- sapply(strsplit(x, ";"), function(x) paste(unique(x), collapse = ";"))
  x <- ifelse(x %in% "NA", NA, x)
  X$R.group <- x
  
  #### S.group 
  x <- gsub("NA;", "", X$S.group)
  x <- sapply(strsplit(x, ";"), function(x) paste(unique(x), collapse = ";"))
  x <- ifelse(x %in% "NA", NA, x)
  X$S.group <- x
  
  #### D.group 
  x <- gsub("NA;", "", X$D.group)
  x <- sapply(strsplit(x, ";"), function(x) paste(unique(x), collapse = ";"))
  x <- ifelse(x %in% "NA", NA, x)
  X$D.group <- x
  
  
  #### Reduce D.group
  # sometimes, when some records end up in mutliple D.groups, it turns out that all records can be lumped into one big D.group. It is usually the case when there is 2 duplicate ranges of dates and a third record with a date that is within the range. So first the code IDs the 2 duplicates and then it gives another D.group for a one to one relationship for each range-date pair.
  idx.non.na.D.group <- which(!is.na(X$D.group))
  if(length(idx.non.na.D.group) > 0) {
    
    x <- X$D.group[idx.non.na.D.group]
    
    if(length(unique(unlist(strsplit(x, ";")))) == length(x)) {
      print(X)
      readline("case where all x$D.group would be lumped into one - before. Press [enter].")
      X$D.group[idx.non.na.D.group] <- min(unique(unlist(strsplit(x, ";"))))
      
      print(X)
      readline("case where all x$D.group would be lumped into one - after. Press [enter]. Talk to Valentine Herrmann if you don't understand what just happened.")
    }
  }
  
  
  
  ### Add prevalence ####
  # For each D.group,Prevalence is added following this list (We keep going down the list if D.precedence is still NA or if multiple "1" were assigned) :
  ## 0. Never give precedence to a record with a capital S in the conflict field 
  ## 1. Take biggest depth (deepest record)
  ## 2. Take smallest min.dbh
  ## 3. Take most inclusive looking at notes within a same study
  ## 4. Take longer study when length_longer_record = 1.75 * length_of_its_duplicates
  ## 5. Take OM over C
  ## 6. If a record was checked against original publication, give it precedence over others
  ## 7. Take later study over older
  ## 8. Look at dup.num and use it, but add something about it in the notes field -- NOT RELEVANT ANYMORE
  ## 9. If still not been able to pin point precedence (including records that only differ in method.ID.), give NAC to all precedence for future manual rating + append "manual D.precendence rating" to the notes
  # Once done with each "D.group"
  ## Deal with records with several D.groups. 
  ## Check if the D.precedence given matches the  D.precedence.measurement.ID (which is entered manually when needed, automated if not).
  ## If D.precedence given does not match D.precedence.measurement.ID....  Look for "manually" in conflict.notes and see if D.precendence.meauserment.ID is non ambiguous. If yes, use it, if no, give NAC to all precedence for future manual rating + append "manual D.precendence rating" to the notes.
  
  
  idx.D.group <- which(grepl("D", X$conflicts))
  
  if (length(idx.D.group) > 0) { # if there is any duplicates
    
    unique.D.groups <- unique(unlist(sapply(X[idx.D.group, ]$D.group, strsplit, ";")))
    
    collecting.x <- NULL # This is an object that will hold the outputs of the loop below, in case some records belong to multiple groups of duplicates 
    
    for (d in unique.D.groups) { # loop through each D.group
      
      pattern.D.group <- paste0("(^",d, "$)|(^",d, ";)|(;", d, ";)|(;", d, "$)")
      x <- X[grepl(pattern.D.group, X$D.group),]
      
      # get what the records are the same for ####
      all.same.citation <- length(unique(x$citation.ID)) == 1 # are all record the from the same study ?
      all.same.method <- length(unique(x$method.ID)) == 1  # are all record the from the same method ?
      all.same.notes <-  length(unique(x$notes)) == 1 # have all record the same note ?
      all.same.C.units <- length(unique(x$variable.name)) == 1 # have all record the same carbon units ?
      all.same.depth <- length(unique(x$depth)) == 1 # have all record the same depth ?
      all.same.dbh <- length(unique(x$min.dbh)) == 1# have all record the same min.dbh ?
      all.same.record.duration <- !(any(grepl("T", x$conflict.type)) | all(grepl("T", x$conflict.type) & grepl("(s)|(S)", x$conflicts)))
      
      any.checked.original.pub <- any(x$checked.ori.pub == 1)
      any.capital.S.in.conflicts <- any(grepl("S", x$conflicts, ignore.case = F))
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      # remove D.precedence to start from scratch (if there is any no NA in D.precedence at this points that means that the record belongs to several D.groups, this will be taken care of at the end (look for collecting.x)) ####
      
      # if(!(still.more.than.one.1 | still.only.NAs) & any(my_is.na(x$D.precedence.measurement.ID))) stop()
      if(!(still.more.than.one.1 | still.only.NAs) & any(my_is.na(x$D.precedence.measurement.ID))) {
        
        x$D.precedence <- NA
        print(i)
        # print(x)
        warning("This is a case where we erased  D.precedence at the begining", immediate. = T)
        # readline("press [enter]")
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 0. Never give precedence to a record with a capital S in the conflict field ####
      
      if((still.more.than.one.1 | still.only.NAs) & any.capital.S.in.conflicts) {
        
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.cap.S.in.conflicts <- grep("S", x[idx.to.look.at, ]$conflicts, ignore.case = F)
        
        if(length(idx.cap.S.in.conflicts) > 0){
          x[idx.to.look.at, ][idx.cap.S.in.conflicts, ]$D.precedence <- 0
          
        }
        
        
        if(((nrow(x) - length(idx.cap.S.in.conflicts)) > 0) & ((nrow(x) - length(idx.cap.S.in.conflicts)) < nrow(x))) {
          x[idx.to.look.at, ][-idx.cap.S.in.conflicts, ]$D.precedence <- 1
        }
        
        
      }
      
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      # 1. If records differ in depth, the one with greatest depth gets precedence. ####
      # records.differ.only.in.depth <- c(all.same.citation & all.same.method & all.same.notes & all.same.C.units & !all.same.depth & all.same.dbh)
      
      if((still.more.than.one.1 | still.only.NAs) & !all.same.depth) {
        
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.max.depth <- which(x[idx.to.look.at,]$depth == max(x[idx.to.look.at,]$depth, na.rm = T) | my_is.na(x[idx.to.look.at,]$depth))
        
        if(length(idx.max.depth) < nrow(x[idx.to.look.at,])) {
          x[idx.to.look.at,][idx.max.depth, ]$D.precedence <- 1 ; x[idx.to.look.at,][-idx.max.depth, ]$D.precedence <- 0 
        }
        
        
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      # 2. If records differ in min.dbh, the one with smallest min.dbh gets precedence. ####
      
      if((still.more.than.one.1 | still.only.NAs) & !all.same.dbh) { # if still need to go down the list
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.min.min.dbh <- which(x[idx.to.look.at,]$min.dbh == min(x[idx.to.look.at,]$min.dbh, na.rm = T) | my_is.na(x[idx.to.look.at,]$min.dbh))
        
        if(length(idx.min.min.dbh) < nrow(x[idx.to.look.at,])) {
          x[idx.to.look.at,][idx.min.min.dbh, ]$D.precedence <- 1
          x[idx.to.look.at,][-idx.min.min.dbh, ]$D.precedence <- 0 
        }
        
        
      } # if still need to go down the list
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 3. Take the most inclusive looking at notes within a same study ####
      
      if((still.more.than.one.1 | still.only.NAs) & !all.same.notes){
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        same.study <- length(unique(x[idx.to.look.at, ]$citation.ID)) == 1
        
        idx.more.inclusive <- grep("(\\+)|(\\ball)|(includes)|(including)", x[idx.to.look.at,]$notes, ignore.case = T, perl = T) # (\\ball) is to get "all" and not "small" that we don't necessaryly want to pick up
        idx.less.inclusive <- grep("(only)", x[idx.to.look.at,]$notes, ignore.case = T, perl = T)
        
        
        clear.cut <- ifelse(any(my_is.na(x[idx.to.look.at,]$notes)) | length(idx.more.inclusive) == 0 | length(idx.less.inclusive) == 0, FALSE, all(sort(c(idx.more.inclusive, idx.less.inclusive)) == 1:nrow(x[idx.to.look.at,])))
        
        if(clear.cut & same.study) {
          x[idx.to.look.at,][idx.more.inclusive, ]$D.precedence <- 1
          x[idx.to.look.at,][idx.less.inclusive, ]$D.precedence <- 0 
        }
        
        if(!clear.cut | !same.study){
          print(i)
          # print(x$notes)
          # print(x)
          warning("not a clear cut in notes (may be because not same study)", immediate. = T)
          # readline("press[enter]")
        }
        
      }
      
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 4. If records differ in length. Give precedence to multiple-year measurement periods-- If one record measurement period is >1.75 x the length of its duplicate, go with that one. ####
      
      if ((still.more.than.one.1 | still.only.NAs) & !all.same.record.duration) { # if still need to go down the list and durations are different
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        record.duration <- difftime(x[idx.to.look.at,]$my.end.date, x[idx.to.look.at,]$my.start.date)
        record.duration <- ifelse(my_is.na(record.duration), 1, record.duration) # consider a record with only "date" to be 1 year long
        
        idx.max.record.duration <- which(record.duration == max(record.duration))
        
        if(length(idx.max.record.duration) < nrow(x[idx.to.look.at,])) {
          one.record.is.1.75.times.longer.than.all.the.others <- unique(record.duration[idx.max.record.duration] > 1.75 * record.duration[-idx.max.record.duration])
          if(length(one.record.is.1.75.times.longer.than.all.the.others) > 1) stop("code more here")
          if (one.record.is.1.75.times.longer.than.all.the.others) {
            x[idx.to.look.at,][idx.max.record.duration, ]$D.precedence <- 1
            x[idx.to.look.at,][-idx.max.record.duration, ]$D.precedence <- 0
          }
        }
        
        # if(! one.record.is.1.75.times.longer.than.all.the.others) {
        #   print(i)
        #   print(x)
        #   warning("records differ in duration but not 1.75 x more", immediate. = T)
        #   readline("press [enter]")
        # }
        
        
        
        
      } # if still need to go down the list and durations are different
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 5. If records differ only in units (C or OM) and C = 0.45 to 0.55 * OM, give precedence to OM. The logic there is that researchers use slightly varying conversion factors, so when there's a choice its best to do the conversion ourselves. ####
      
      if ((still.more.than.one.1 | still.only.NAs) & !all.same.C.units) { # if still need to go down the list
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.C <- grep("_C", x[idx.to.look.at,]$variable.name)
        idx.OM <- grep("_OM", x[idx.to.look.at,]$variable.name)
        
        if(length(idx.OM) > 1 & length(idx.C) > 1) {
          print(i)
          # print(x)
          warning("work in unit stuff when more than one record in both units", immediate. = T)
          # readline("press [enter]")
        }
        
        C.within.0.45.to.0.55.times.OM <- x[idx.to.look.at,][idx.C, ]$mean >= 0.45 * x[idx.to.look.at,][idx.OM, ]$mean & x[idx.to.look.at,][idx.C, ]$mean <= 0.55 * x[idx.to.look.at,][idx.OM, ]$mean
        
        if(any(C.within.0.45.to.0.55.times.OM)){
          
          if(length(idx.OM) == 1) {
            x[idx.to.look.at,][idx.OM, ]$D.precedence <- 1 # give one to the one OM
            x[idx.to.look.at,][idx.C, ]$D.precedence[C.within.0.45.to.0.55.times.OM] <- 0 # give 0 to the C that is within 0.45 to 0.55 times the OM
            x[idx.to.look.at,][idx.C, ]$D.precedence[!C.within.0.45.to.0.55.times.OM] <- 1 # give 1 to the C that is NOT within 0.45 to 0.55 times the OM
          }
          
          if(length(idx.C) == 1) {
            x[idx.to.look.at,][idx.OM, ]$D.precedence <- 1 # give one to all the OM records
            x[idx.to.look.at,][idx.C, ]$D.precedence <- 0 # give 0 to the one C that is within 0.45 to 0.55 times the OM
          }
          
        }
        
        # if(!C.within.0.45.to.0.55.times.OM) {
        #   print(i)
        #   print(x)
        #   warning("work in unit stuff, C is not within 0.45 to 0.55 times OM", immediate. = T)
        #   readline("press [enter]")
        # }
        
        
        
      } # if still need to go down the list
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 6. Give precedence to records that were checked against original pub ####
      
      if ((still.more.than.one.1 | still.only.NAs) & any.checked.original.pub) {
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.checked.original.pub <- which(x[idx.to.look.at, ]$checked.ori.pub == 1)
        
        if(length(idx.checked.original.pub) < nrow(x[idx.to.look.at, ])){
          x[idx.to.look.at, ][idx.checked.original.pub, ]$D.precedence <- 1
          x[idx.to.look.at, ][-idx.checked.original.pub, ]$D.precedence <- 0
        }
        
        print(i)
        # print(x)
        warning("This is a case of 'checked against original pub'", immediate. = T)
        
        
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      # 7.After resolving all of the above, if duplicates are from different studies, the later study gets precedence. [NOTE: This isn't always ideal, as it may give precedence to an intermediary review over an original publication. However, it works as a start. Precedence can always be edited upon consultation of original pub.] ####
      
      if ((still.more.than.one.1 | still.only.NAs) & !all.same.citation) { # if still need to go down the list
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        x.year <- gsub("(.*)([0-9]{4})(.*)",'\\2', x[idx.to.look.at,]$citation.ID, perl = T)
        
        idx.max.citation.year <- which(x.year == max(x.year, na.rm = T))
        
        if(length(idx.max.citation.year) < nrow(x[idx.to.look.at,])){ # if not all published same year
          
          x[idx.to.look.at,][idx.max.citation.year, ]$D.precedence <- 1
          x[idx.to.look.at,][-idx.max.citation.year, ]$D.precedence <- 0 
        }
        
      } # if still need to go down the list
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(my_is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        # print(x)
        warning("some NA left in D.precedence", immediate. = T)
        # readline("press [enter]")
      }
      
      
      # 8. If still need to be pinned down, look at dup.num and use it , but add something about it in the notes field ####
      ## ***---- This is not relevant anymore since column dup.num does not exist anymore ----**** ##
      # if ((still.more.than.one.1 | still.only.NAs)) {
      #   
      #   if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
      #   if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
      #   
      #   idx.max.dup.num <- which(x[idx.to.look.at, ]$dup.num %in% max(x[idx.to.look.at, ]$dup.num))
      #   
      #   if(length(idx.max.dup.num) %in% 1) {
      #     x[idx.to.look.at, ][idx.max.dup.num, ]$D.precedence <- 1
      #     x[idx.to.look.at, ][-idx.max.dup.num, ]$D.precedence <- 0
      #     # x[idx.to.look.at, ]$conflicts.notes <- ifelse(my_is.na(x[idx.to.look.at, ]$conflicts.notes), "D.precedence based on previously dup.num column.", paste(x[idx.to.look.at, ]$conflicts.notes, "D.precedence based on previously dup.num column.", sep = ". "))
      #   }
      #   
      # }
      # 
      # still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      # still.only.NAs <- all(my_is.na(x$D.precedence))
      # still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(my_is.na(x$D.precedence))
      # 
      # if(still.some.NAs) {
      #   print(i)
      #   print(x)
      #   warning("some NA left in D.precedence", immediate. = T)
      #   # readline("press [enter]")
      # }
      
      # 9. As a last resort, put NAC in D.predence for Krista to do it manually ####
      
      if (still.more.than.one.1 & all(!my_is.na(x$D.precedence))) {
        
        idx.to.look.at <-  x$D.precedence %in% 1
        x$D.precedence[idx.to.look.at] <- "NAC"
        x$conflicts.notes[idx.to.look.at] <- ifelse(my_is.na(x$conflicts.notes[idx.to.look.at]), "D.precedence given manually.", paste(x$conflicts.notes[x$D.precedence %in% 1], "D.precedence given manually.", sep = ". "))
        
        
        print(i)
        # print(x)
        warning("D.precedence to be given manually.", immediate. = T)
        # readline("press [enter]")
        
      }
      
      if (still.only.NAs) {
        
        x$D.precedence <- "NAC"
        x$conflicts.notes <- ifelse(my_is.na(x$conflicts.notes), "D.precedence given manually.", paste(x$conflicts.notes, "D.precedence given manually.", sep = ". "))
        
        
        print(i)
        # print(x)
        warning("D.precedence to be given manually.", immediate. = T)
        # readline("press [enter]")
        
      }
      
      # rbind into collecting.x
      
      collecting.x <- rbind(collecting.x, x)
      
      # print(x); readline()
    } # loop through each D.group
    
    # deal with records that belong to 2 groups ####
    
    if(any(duplicated(collecting.x$measurement.ID))) {
      
      duplicated.measurement.ID <- unique(collecting.x$measurement.ID[duplicated(collecting.x$measurement.ID)])
      
      idx.to.look.at <- which(collecting.x$measurement.ID %in% duplicated.measurement.ID)
      
      all.same.precedence <- length(unique(collecting.x[idx.to.look.at, ]$D.precedence)) == 1
      
      if(all.same.precedence)     collecting.x <- collecting.x[!duplicated(collecting.x$measurement.ID), ]
      
      if(!all.same.precedence){
        
        collecting.x[idx.to.look.at, ]$D.precedence <- "NAC"
        collecting.x[idx.to.look.at, ]$conflicts.notes <- ifelse(my_is.na(collecting.x[idx.to.look.at, ]$conflicts.notes), "D.precedence given manually.", paste(collecting.x[idx.to.look.at, ]$conflicts.notes, "D.precedence given manually.", sep = ". "))
        
        collecting.x <- collecting.x[!duplicated(collecting.x$measurement.ID), ]
        
        print(i)
        print(collecting.x)
        warning("not an easy fix for records that belong to multiple D.groups...", immediate. = T)
        # readline("press [enter]")
      }
      
    }
    
    # Check if the D.precedence given matches the D.precedence.measurement.ID (which is entered manually when needed, automated if not). If it does, great. If it doesn't but that is because there is no D.precedence.measurement.ID (new record), if the D.precedence was given with no ambiguity, give D.precedence.measurement.ID. If it doesn't and there IS a D.precedence.measurement.ID, give an error.
    
    for (d in unique.D.groups) {
      pattern.D.group <- paste0("(^",d, "$)|(^",d, ";)|(;", d, ";)|(;", d, "$)")
      x <- collecting.x[grepl(pattern.D.group,collecting.x$D.group),]
      
      if(length(my_na.omit(unique(x$D.precedence.measurement.ID))) > 1 & all(unique(x$D.precedence.measurement.ID) %in% x$measurement.ID)) {
        # if one of the record is a replicate and the measurement.ID of the other one is the same as the replicate of the first, then it is fine and we can leave it this way. Same if there is 2 groups of duplictes and they both point to the independant records (no ";" in D.group records)
        if(any(grepl("R", x$conflicts))) {
          if(any(grepl(";", x$R.group))) stop() # this is to give an error to me because I have not coded this eventuality but I don't think it exists...
          idx.of.the.other.replicate.outside.of.this.D.group <- which(collecting.x$R.group %in% x$R.group[grepl("R", x$conflicts)] & !collecting.x$measurement.ID %in% x$measurement.ID[grepl("R", x$conflicts)])
          if(x$D.precedence.measurement.ID[!grepl("R", x$conflicts)] == collecting.x$measurement.ID[idx.of.the.other.replicate.outside.of.this.D.group])   warning("All is good!", immediate. = T)
        } #  if(any(grepl("R", x$conflicts))) 
        
        if(any(grepl(";", x$D.group))) {
          other.D.group <- unique(gsub(pattern.D.group, "", x$D.group))
          other.D.group <- other.D.group[! other.D.group %in% ""]
          
          x.both.D.group <- rbind(x, collecting.x[collecting.x$D.group %in% other.D.group,])
          if(x.both.D.group$D.precedence[grepl(";", x.both.D.group$D.group)] == 0 & all(x.both.D.group$measurement.ID[!grepl(";", x.both.D.group$D.group)] == x.both.D.group$D.precedence.measurement.ID[!grepl(";", x.both.D.group$D.group)]))  warning("All is good!", immediate. = T) else { more.thn.one.D.precedence.measurement.ID.given.and.not.easy.case.split.ID <- c(more.thn.one.D.precedence.measurement.ID.given.and.not.easy.case.split.ID, i) #stop("There is more than one D.precedence.measurement.ID given and it is not an easy case")
          }
        } #   if(any(grepl(";", x$D.group))) 
        
        if((!any(grepl("R", x$conflicts)) & !any(grepl(";", x$D.group))) & !any(grepl("manually", x$conflicts.notes))) stop("There is more than one D.precedence.measurement.ID given and it is not a case of replicates or multiple duplicates.")
      } 
      
      if(length(my_na.omit(unique(x$D.precedence.measurement.ID))) > 1 & sum(unique(x$D.precedence.measurement.ID) %in% x$measurement.ID) == 1) {  # I added this after replacing my.date by my.start.date when my.start.date and my.end.dates are the same.
        x$D.precedence.measurement.ID <- x$measurement.ID[x$D.precedence %in% "1"]
        warning("new (or wrong old) record with no ambiguity and D.precedence.measurement.ID can be given!", immediate. = T)
      }
      
      if(length(my_na.omit(unique(x$D.precedence.measurement.ID))) == 1 & ifelse(sum(as.numeric(x$D.precedence)) %in% 1, x$measurement.ID[x$D.precedence %in% 1] %in% my_na.omit(unique(x$D.precedence.measurement.ID)), FALSE)) {
        warning("All is good!", immediate. = T) # this is when the code works on its own on older data
        x$D.precedence.measurement.ID <- my_na.omit(unique(x$D.precedence.measurement.ID)) # this is because sometimes there is a NAC in there and Ithink it can safely be changed to the unique x$D.precedence.measurement.ID...
      }
      
      if((length(my_na.omit(unique(x$D.precedence.measurement.ID))) == 0 & sum(as.numeric(x$D.precedence)) %in% 1) | (length(my_na.omit(unique(x$D.precedence.measurement.ID))) == 1 & any(my_is.na(x$D.precedence.measurement.ID)) & sum(as.numeric(x$D.precedence)) %in% 1)) { # this is where D.precendence is given to new record with no ambiguity and D.precedence.measurement.ID can be given
        x$D.precedence.measurement.ID <- x$measurement.ID[x$D.precedence %in% "1"]
        warning("new (or wrong old) record with no ambiguity and D.precedence.measurement.ID can be given!", immediate. = T)
      }
      
      if(length(my_na.omit(unique(x$D.precedence.measurement.ID))) == 1 & ifelse(sum(as.numeric(x$D.precedence)) %in% 1, !x$measurement.ID[x$D.precedence %in% 1] %in% unique(x$D.precedence.measurement.ID), FALSE)) {
        print(x)
        readline("This is an example where D.precedence does not match D.precedence.measurement.ID")
        print(X)
        # readline("This is the whole data for that group of record")
        stop("This is an example where D.precedence does not match D.precedence.measurement.ID")
      } 
      
      collecting.x[grepl(pattern.D.group, collecting.x$D.group),] <- x
    } #for (d in unique.D.groups)
    
    # put back into X ####
    
    if(!all(X[idx.D.group, ]$measurement.ID %in% collecting.x$measurement.ID)) stop("make sure we've got all records") # this is to make sure we are putting records back in the main flow correctly
    
    X[idx.D.group, ] <- collecting.x
    
    
  } # if there is any duplicates
  
  # save output into final object####
  MEASUREMENTS.final[[i]] <- X
} # for(i in 1:length(MEASUREMENTS.split))


# re-formate output ####
MEASUREMENTS.final.split <- MEASUREMENTS.final
MEASUREMENTS.final <- do.call(rbind, MEASUREMENTS.final)
rownames(MEASUREMENTS.final) <- NULL
MEASUREMENTS.final <- MEASUREMENTS.final[match(MEASUREMENTS$measurement.ID, MEASUREMENTS.final$measurement.ID),]

# compare original with new values in the duplicate related columns ####
new.duplicate.related.column.values <- MEASUREMENTS.final[, duplicate.related.columns]

## ID split.ID that are not the same as the original ####

not.the.same.ones <- which(apply(original.duplicate.related.column.values == new.duplicate.related.column.values, 1, function(x) any(!x[!is.na(x)])))

what.not.the.same.ones <- apply(original.duplicate.related.column.values == new.duplicate.related.column.values, 1, function(x) names(x)[!x & !is.na(x)])
table(unlist(what.not.the.same.ones[not.the.same.ones]))

what.not.the.same.ones.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[not.the.same.ones]))



### ignore new records or groups that have only one record left because the rest was deleted ####
newest.loaded.by <- ""# c("Becky Banbury Morgan (Beckybanbury)", "Ian McGregor") # , "Abby Ferson"
what.not.the.same.ones.split.ID.new.not.new <- sapply(what.not.the.same.ones.split.ID, function(split.ID) any(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$loaded.by %in% newest.loaded.by)) 
what.not.the.same.ones.split.ID.new <- what.not.the.same.ones.split.ID[what.not.the.same.ones.split.ID.new.not.new]
what.not.the.same.ones.split.ID.not.new <- what.not.the.same.ones.split.ID[!what.not.the.same.ones.split.ID.new.not.new]


only.one.record.left.split.ID <- what.not.the.same.ones.split.ID[sapply(what.not.the.same.ones.split.ID, function(split.ID) nrow(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]) == 1)] # this is just to ignore the records that were in a group but for which the potential duplicates have been deleted at some point


## look at issues more closely and decide if we delete the old conflict situation or the new one ####

### First create 2 objects that hold character vectors of concatenated measurement.ID of all records within a group of conflicting records for which the code's output differ from what was originally there (usually manually edited). One objects keeps the ID of the groups for wich we need to keep the "old" version (manually edited), the onther one, the ID of the groups for which the older version can be deleted because it is wrong. 

retrieve.old.version.meas.IDs <- c("1224;1225;1226;1227;1228;1229;17532;17542;17552;17562", # to be debated with Krista: "P stands for "plot" and indicates a plot duplicate."... Still not sureso leaving old duplicate codes...
                                   "1024;1025;1026;1027",
                                   "1028;1029;1030;1031",
                                   "1034;1035;1036;1037",
                                   "1038;1039;1040;1041",
                                   "1042;1043;1044;1045",
                                   "4006;4007;4008", # previous script handled this better so leaving it this way
                                   "7536;7537;7538", # suspecting that no overlap and previous script handled this better so leaving it this way
                                   "1218;1219;1220;1221;1222;1223;17512;17522", # this was overwritten at first but now we want to keep that as we manually edited D.precedence
                                   "13646;13647;13648;13649;13650;13651;13652;13653;13654;13661;13662;13663;13674;13675;13678;13679;13682;13687;13690;13693;13696;13699;13700;19033;19034;19035;19036;19037;19038;19039;19040;19041;19042;19043;19044;19045;19046;19047;19048;19049;19050;19965;19966;19967;19968;19969;19970;19971;19972;19973;19974;19975;19976;19977;19978;19979;19980;19981;19982;19983;19984", # this was overwritten at first but now we want to keep that as we manually edited D.precedence
                                   "14806;14807;14808;14809;14810;14811;14812;14813;14814;14815;14839;14840;14841;14842;14843;14858;19469;19470;19471;19472;19473;19474;20236;20237;20238;20239;20240;20241;20242;20243;20244;20245;20248;20249;20250;20251;20252;20253;20254;20255;20256;20257;20454;20456;20458;20460", # we want to keep snice it was manually edited
                                   "1230;1231;1232;1233;1234;1235;17572;17582", # this was overwritten at first but now we want to keep that as we manually edited D.precedence
                                   "25212;25213;25214;25216",
                                   "23707;23719;23731;23743;23752;23761;23770;23779;23788"
) # paste here the measurement.ID (concatenated and separated by a semicolumn) of the all the records in a group for which you think the old version is more approriate than the code's output

delete.old.version.meas.IDs <- c( MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% only.one.record.left.split.ID, ]$measurement.ID, #ignore old conflicts of measurement.ID that are in a conflict groups where only one record is left
                                  "11525;11541", # Abby Ferson weird assignment of R group
                                  "11542;11543;11552;11560", # Abby Ferson weird assignment of R group
                                  "11576;19886;19887;19888", # Abby Ferson weird assignment of R group
                                  "11647;11652", # Abby Ferson weird assignment of R group
                                  "3229;3236;3239;12103",
                                  "3230;3237;3240;12104",
                                  "8816;17416;17480", # different but now it will be like that
                                  "17428;17482", # different but now it will be like that
                                  "10449;10455;10461;10467;10473;10481;10487;10492", # different but now it will be like that
                                  "3510;3511;3512;17378",
                                  "1684;1685",
                                  "6647;6662", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6855;6860", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "7663;7664;7669;7670;7671;7672;7676;7677;7678;7679", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6651;6660", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6848;6858", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6649;6661", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6846;6859", # M,T becomes M in conflict.type because now small range of date is considered as one date
                                  "6669;6686;6689", # will have to manually add D.precedence beacuse S becomes D
                                  "6671;6684;6687", # will have to manually add D.precedence beacuse S becomes D
                                  "6673;6685;6688", # will have to manually add D.precedence beacuse S becomes D
                                  "15060;15063;15066;15069;15072;15079;15080;15081;15088;15089;15090", # simpler thus better now, same D.precedence
                                  "15278;15279;15280;15281;15290;15291;15293;15294;15295", # simpler thus better now, same D.precedence
                                  "15058;15061;15064;15067;15070;15073;15074;15075;15082;15083;15084", # simpler thus better now, same D.precedence
                                  "15059;15062;15065;15068;15071;15076;15077;15078;15085;15086;15087", # simpler thus better now, same D.precedence
                                  "17382;17470", # fixes D.group
                                  "17458;17490", # fixes D.group
                                  "17410;17478", # fixes D.group
                                  "1208;17370;17464", # fixes D.group
                                  "17404;17474", # fixes D.group
                                  "9485;9486;9511;9512;9513", # meaningless change in D.precedence.meas.ID
                                  "17035;17036;17037;17039;17040;17041;17042;17043;17044", # meaningless change in D.precedence.meas.ID
                                  "17029;17030;17031;17038;17045;17046;17047;17048;17049;17050", # meaningless change in D.precedence.meas.ID
                                  "9483;9484;9508;9509;9510", # meaningless change in D.precedence.meas.ID
                                  "13761;18758",  # C is correct in conflict.type
                                  "13776;18759", # C is correct in conflict.type
                                  "13791;18760", # C is correct in conflict.type
                                  "13806;18761", # C is correct in conflict.type
                                  "13821;18762", # C is correct in conflict.type
                                  "13836;18763", # C is correct in conflict.type
                                  "13851;18764", # C is correct in conflict.type
                                  "13866;18765", # C is correct in conflict.type
                                  "13881;18766", # C is correct in conflict.type
                                  "13896;18767", # C is correct in conflict.type
                                  "13911;18768", # C is correct in conflict.type
                                  "13926;18769", # C is correct in conflict.type
                                  "13941;18770", # C is correct in conflict.type
                                  "13956;18771", # C is correct in conflict.type
                                  "13971;18772", # C is correct in conflict.type
                                  "13986;18773", # C is correct in conflict.type
                                  "14001;18774", # C is correct in conflict.type
                                  "8576;8582;18028", # C is correct in conflict.type
                                  "2983;2995", # C is correct in conflict.type
                                  "3017;3025", # C is correct in conflict.type
                                  "15238;20420", # now a conflict (not I anymore)
                                  "14249;14259;14267", # legitimate conflict
                                  "15237;20421", "13023;20410", "15241;20422", "259;20392", "13024;20411", "15242;20423", "16284;20438", "20455;20457;20459;20461", # now a conflict (not I anymore)
                                  "3191;3195;3197;20445", # now a new sort of conflict
                                  "21004;28480", "21248;28743",  # now a conflict (not I anymore)
                                  "26783;26788;26792;26795;26798;26802;26808;26812;26814;26817;26823;26827;26830", # better handled now
                                  "23561;23567;23573;23579;23633;23639;23645;23651", "22473;22481;22489", # better handled now
                                  "26415;26420;26425;26430;26435;26440;26445;26450;26455;26460;26465;26469", # better handled now
                                  "23713;23725;23737;23749;23758;23767;23776;23785;23794", # better handled now
                                  "22684;22686;22688;22690;22692;22694;22696;22698", "22475;22483;22491", "25200;25203;25206;25209", # better handled now
                                  "26417;26422;26427;26432;26437;26442;26447;26452;26457;26462;26466;26471", # better handled now
                                  "23715;23727;23739;23751;23760;23769;23778;23787;23796", # better handled now
                                  "22685;22687;22689;22691;22693;22695;22697;22699","23562;23568;23574;23580;23634;23640;23646;23652", # better handled now
                                  "24335;24337;24339;24341", "25817;25820;25823", "22867;22872;22877;22882;22887", "22476;22484;22492", # better handled now
                                  "22773;22774;22775", "26418;26423;26428;26433;26438;26443;26448;26453;26458;26463;26467;26472", # better handled now
                                  "23716;23728;23740", "22275;22280;22285;22290", "22477;22485;22493", "26363;26365;26367;26369", # better handled now
                                  "26329;26339;26348", "22478;22486;22494", "25201;25204;25207;25210",# better handled now
                                  "26785;26790;26794;26796;26799;26804;26806;26809;26815;26819;26822;26825;26828;26832", # better handled now
                                  "23564;23570;23576;23582;23636;23642;23648;23654", "22276;22281;22286;22291", "26330;26340;26349", # better handled now
                                  "25929;25935;25940;25951","22479;22487;22495", "24405;24407;24409;24411;24413", "23717;23729;23741", # better handled now
                                  "26786;26791;26800;26810;26820", "23565;23571;23577;23583;23637;23643;23649;23655", # better handled now
                                  "26331;26341;26350", "26784;26789;26793;26803;26818;26824;26831", # better handled now
                                  "23714;23726;23738;23750;23759;23768;23777;23786;23795", "22863;22868;22873;22878;22883", # better handled now
                                  "26414;26419;26424;26429;26434;26439;26444;26449;26454;26459;26464;26468", "25928;25934;25939;25950", # better handled now
                                  "23563;23569;23575;23581;23635;23641;23647;23653", "26624;26626;26628;26630;26632;26633", # better handled now
                                  "25818;25821;25824","22865;22870;22875;22880;22885", # better handled now
                                  "23711;23723;23735;23747;23756;23765;23774;23783;23792", "22272;22278;22283;22288", # better handled now
                                  "22866;22871;22876;22881;22886", "23712;23724;23736;23748;23757;23766;23775;23784;23793", # better handled now
                                  "22273;22279;22284;22289","22480;22488;22496", "25202;25205;25208;25211", # better handled now
                                  "24406;24408;24410;24412;24414",  "26364;26366;26368;26370", "23718;23730;23742", # better handled now
                                  "26787;26797;26801;26805;26807;26811;26813;26816;26821;26826;26829", # better handled now
                                  "23566;23572;23578;23584;23638;23644;23650;23656","24604;24613;24616", # better handled now
                                  "22277;22282;22287;22292","23825;23827;23829;23831;23833;23835;23837;23839","22922;22923;22924", # better handled now
                                  "26332;26342;26351",  "25926;25932;25937;25948", # better handled now
                                  "23709;23721;23733;23745;23754;23763;23772;23781;23790", # better handled now
                                  "23924;23930;23934", "26326;26337;26346", "22864;22869;22874;22879;22884", # better handled now
                                  "22592;22593;22594;22595;22596;22597;22598", "23708;23720;23732;23744;23753;23762;23771;23780;23789", # better handled now
                                  "26325;26336;26345",  "25927;25933;25938;25949", "21299;21300;21301;21302", # better handled now
                                  "23710;23722;23734;23746;23755;23764;23773;23782;23791",  "23925;23931;23935", "23234;23242;23244", # better handled now
                                  "26327;26338;26347", "25925;25931;25936;25947",  "23232;23241;23243", "26324;26335;26344", # better handled now
                                  "24605;24614;24617", "24336;24338;24340;24342", "23826;23828;23830;23832;23834;23836;23838;23840", # better handled now
                                  "26333;26343;26352", "23944;23945;23946", "27664;27665;27666;27667", "26623;26625;26627;26629;26631", # better handled now
                                  "24603;24612;24615", "27099;27100;27101;27102;27103", "28413;28414;28415", "22474;22482;22490", # better handled now
                                  "26416;26421;26426;26431;26436;26441;26446;26451;26456;26461;26470", "25816;25819;25822", # better handled now
                                  "27545;30490", "21004;28496", "21274;28955",  "21286;28957",  "20922;28494", "21248;28994",  # now a conflict
                                  "21558;21564;21569;28581;28582;28583", # now a conflict
                                  "26324;26335;26344;30023;30024"
)# paste here the measurement.ID (concatenated and separated by a semicolumn) of the all the records in a group for which you think the code does a better job than what the original conflict situation was.

#### keep new handling of stand age 999 ####
## Valentine to Krista: I believe that when I originally ran the code for duplicated measurements I was considering stand.age "999" as NA. So, 2 measurements of the same variable, at the same plot, with no dates, and with stand.age "999" were considered as S conflict, and both measurements were getting a capital S (which I think deletes them both when creating ForC_simplified). Now I ran the code considering "999" as a 'known' stand.age so the 2 records above would get a conflict R if the method (citation.ID) is the same (see measurements.ID 14061 and 14062 for an example) or D if not (see measurements.ID 7782 and 7786 for an example - and precedence would be for the latest study).That second second solution is better, right?
## Krista: Yes

stand.age.issue.meas.ID <- NULL
for(split.ID in what.not.the.same.ones.split.ID[!what.not.the.same.ones.split.ID %in% only.one.record.left.split.ID]) { 
  print(split.ID)
  if(all(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$stand.age %in% "999")) stand.age.issue.meas.ID <- c(stand.age.issue.meas.ID, paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
}


stand.age.issue.meas.ID <- stand.age.issue.meas.ID[!stand.age.issue.meas.ID %in% retrieve.old.version.meas.IDs] # do not let overwrite things we agreed to keep earlier


delete.old.version.meas.IDs <- c(delete.old.version.meas.IDs, sort(stand.age.issue.meas.ID))

retrieve.old.version.split.ID <- NULL
delete.old.version.split.ID <- NULL
need.user.input.split.ID <- NULL

### not same conflicts.notes ####
what.not.the.same.conflicts.notes.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "conflicts.notes" %in% x)]))

for(split.ID in what.not.the.same.conflicts.notes.split.ID) { 
  print(which(what.not.the.same.conflicts.notes.split.ID %in% split.ID))
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
  
}

### not same conflict.type and not new records ####
what.not.the.same.conflict.type.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "conflict.type" %in% x)]))
what.not.the.same.conflict.type.split.ID <- what.not.the.same.conflict.type.split.ID[! what.not.the.same.conflict.type.split.ID %in% what.not.the.same.ones.split.ID.new]
what.not.the.same.conflict.type.split.ID <- what.not.the.same.conflict.type.split.ID [! what.not.the.same.conflict.type.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.conflict.type.split.ID) { 
  print(which(what.not.the.same.conflict.type.split.ID %in% split.ID))
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
}

### not same conflicts and not new records ####   
what.not.the.same.conflicts.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "conflicts" %in% x)]))
what.not.the.same.conflicts.split.ID <- what.not.the.same.conflicts.split.ID[!what.not.the.same.conflicts.split.ID %in% what.not.the.same.ones.split.ID.new]
what.not.the.same.conflicts.split.ID <- what.not.the.same.conflicts.split.ID [! what.not.the.same.conflicts.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.conflicts.split.ID) { 
  print(which(what.not.the.same.conflicts.split.ID %in% split.ID))
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
}


### not same S.group and not new records ####   
what.not.the.same.S.group.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "S.group" %in% x)]))
what.not.the.same.S.group.split.ID <- what.not.the.same.S.group.split.ID[! what.not.the.same.S.group.split.ID %in% what.not.the.same.ones.split.ID.new]
what.not.the.same.S.group.split.ID <- what.not.the.same.S.group.split.ID [! what.not.the.same.S.group.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.S.group.split.ID) { 
  print(which(what.not.the.same.S.group.split.ID %in% split.ID))
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
}

### ??? Abby Ferson entered some R.group in a strange way

### not same R.group and not new records ####      
what.not.the.same.R.group.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "R.group" %in% x)]))
what.not.the.same.R.group.split.ID <- what.not.the.same.R.group.split.ID[! what.not.the.same.R.group.split.ID %in% what.not.the.same.ones.split.ID.new]
what.not.the.same.R.group.split.ID <- what.not.the.same.R.group.split.ID [! what.not.the.same.R.group.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.R.group.split.ID) { 
  print(which(what.not.the.same.R.group.split.ID %in% split.ID))
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
  # keep.or.retrieve.function()
}

### ??? Abby Ferson entered some R.group in a strange way

### not same D.group and not new records ####
what.not.the.same.D.group.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "D.group" %in% x)]))
what.not.the.same.D.group.split.ID <- what.not.the.same.D.group.split.ID[! what.not.the.same.D.group.split.ID %in% what.not.the.same.ones.split.ID.new]
what.not.the.same.D.group.split.ID <- what.not.the.same.D.group.split.ID [! what.not.the.same.D.group.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.D.group.split.ID) { 
  print(which(what.not.the.same.D.group.split.ID %in% split.ID))
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline() 
}


### not same D.precedence and not new records ####
what.not.the.same.D.precedence.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "D.precedence" %in% x)]))
what.not.the.same.D.precedence.split.ID <- what.not.the.same.D.precedence.split.ID[! what.not.the.same.D.precedence.split.ID %in% what.not.the.same.ones.split.ID.new]

what.not.the.same.D.precedence.split.ID <- what.not.the.same.D.precedence.split.ID [! what.not.the.same.D.precedence.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.D.precedence.split.ID) { 
  print(which(what.not.the.same.D.precedence.split.ID %in% split.ID))
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline() 
}

### not same D.precedence.measurement.ID and not new records ####
what.not.the.same.D.precedence.measurement.ID.split.ID <- sort(unique(MEASUREMENTS.final$split.ID[sapply(what.not.the.same.ones, function(x) "D.precedence.measurement.ID" %in% x)]))
what.not.the.same.D.precedence.measurement.ID.split.ID <- what.not.the.same.D.precedence.measurement.ID.split.ID[! what.not.the.same.D.precedence.measurement.ID.split.ID %in% what.not.the.same.ones.split.ID.new]

what.not.the.same.D.precedence.measurement.ID.split.ID <- what.not.the.same.D.precedence.measurement.ID.split.ID [! what.not.the.same.D.precedence.measurement.ID.split.ID %in% c( retrieve.old.version.split.ID, delete.old.version.split.ID, need.user.input.split.ID)]

for(split.ID in what.not.the.same.D.precedence.measurement.ID.split.ID) { 
  print(which(what.not.the.same.D.precedence.measurement.ID.split.ID %in% split.ID))
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% retrieve.old.version.meas.IDs)  retrieve.old.version.split.ID <- c(retrieve.old.version.split.ID, split.ID)
  
  if(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% delete.old.version.meas.IDs)  delete.old.version.split.ID <- c(delete.old.version.split.ID, split.ID)
  
  if(!paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";") %in% c(retrieve.old.version.meas.IDs, delete.old.version.meas.IDs)) need.user.input.split.ID <- c(need.user.input.split.ID, split.ID)
  
  # print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
  # print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
  # readline()
}




## now look at the split.ID in need of user input ####
library(svDialogs)

if(length(need.user.input.split.ID) > 0) {
  msg_box(paste("There is", length(need.user.input.split.ID), "groups of duplicates to review.\n 
                When you are ready to review them, go back to ID_sets_of_duplicate_records.R,\nfind this message and follow the instructions in the comments bellow it. (around line 1366 of the code)."))
  
  # Welcome to you!
  # You are here to review sets of measurements that are potential duplicates (D.group), replicates (R.group), or subsumed by each other (S.group)
  # You are here because what the code above produced is somehow different than what already existed in the data base.
  # The code could be correct (e.g. if something has changed in one measurement)
  # OR
  # The code could be wrong because it can't do beter than a human brain... or at least I don't know how to tell him how to act like one....
  # So your role here is to decide whether the code is correct or wrong. Whether we should keep what the code says or if we should retrieve what the original data said.
  # To do so:
  # 1 - Run this whole script up until section called "## now look at the split.ID in need of user input ###"
  # 2 - Uncomment the line that says to "readline("Press [enter]")" in the for loop bellow
  # 2 - Run the the for loop bellow. It will stop after each set of measurements to review.
  # 3 - Each time the loop waits for you to press enter, review the dataframe that is printed in the console: 
  #     - Look at all the conflict related columns that are in the output of this code ( "conflicts", "R.group", "S.group", "D.group", "D.precedence", "conflict.type", "D.precedence.measurement.ID", "conflicts.notes",)
  #     - and compare them to the original conflict related columns ( "old_conflicts", "old_R.group",  "old_S.group", "old_D.group", "old_conflict.type", "old_conflicts.notes", "old_D.precedence", "old_D.precedence.measurement.ID")
  #     - If you think the old version is better, copy the last row of the output (which is just the measurement IDs concatenated with a semicolumn inbetween, and paste that into the object called "retrieve.old.version.meas.IDs".
  #     - If you think the code does a better job than the old version, copy the last row of the output (which is just the measurement IDs concatenated with a semicolumn inbetween, and paste that into the object called "delete.old.version.meas.IDs". This will delete the old version FOR EVER, so make sure you know what you are doing here...
  #     - If you don't know, press enter to skip. The old version will be maintained by default.
  
  for(split.ID in need.user.input.split.ID) { 
    print(which(need.user.input.split.ID %in% split.ID))
    
    print(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,])
    print(paste(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]$measurement.ID, collapse = ";"))
    # readline("Press [enter]") # uncomment this when you are ready to review the groups one by one
    
  }
  
  
}

## other checks 'by hand'####

X.group = 2103 # 1397 #1373 # 1323
pattern.X.group <- paste0("(^",X.group, "$)|(^",X.group, ";)|(;", X.group, ";)|(;", X.group, "$)")
MEASUREMENTS.final[grepl(pattern.X.group, MEASUREMENTS.final$D.group), ]


split.ID <-  462
MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID, ]
MEASUREMENTS[MEASUREMENTS.final$split.ID %in% split.ID, ]


MEASUREMENTS[MEASUREMENTS$D.group %in% 661,]
MEASUREMENTS.final[MEASUREMENTS$D.group %in% 661,]

MEASUREMENTS.final[MEASUREMENTS.final$measurement.ID %in% c("17541", "17551", "17561", "17571", "18245"),duplicate.related.columns]


### look for "dup.num" in old_conflicts.notes and copy notes over to the new notes (even if the code worked fine without dupnum). This is because originally, a lot of the D.precedence was given lookin at a column called "dup.num", which does not exists anymore, but we should still report that we used it for D.precedence. ####

MEASUREMENTS.final[grepl("dup.num", MEASUREMENTS.final$old_conflicts.notes),]$conflicts.notes <- MEASUREMENTS.final[grepl("dup.num", MEASUREMENTS.final$old_conflicts.notes),]$old_conflicts.notes 

### look for "manually" in old_conflicts.notes and copy notes over to the new notes (even if the code worked without previous manual help). This is not to loose track of precedence potentially given by hand... ####

MEASUREMENTS.final[grepl("manually", MEASUREMENTS.final$old_conflicts.notes),]$conflicts.notes <- "D.precedence given manually."   

## retrieve old conflict information when we decided it was better that way ####
for(split.ID in c(retrieve.old.version.split.ID, need.user.input.split.ID)) { 
  
  X <- MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,]
  
  ## first we don't need to worry about "conflicts", "D.precedence", "conflict.type", "D.precedence.measurement.ID" or "conflicts.notes"
  X[, c("conflicts", "D.precedence", "conflict.type", "D.precedence.measurement.ID", "conflicts.notes")] <- X[, c("old_conflicts", "old_D.precedence", "old_conflict.type", "old_D.precedence.measurement.ID", "old_conflicts.notes")]
  
  ## Then, we need check that we can use the same X.group (that we don't need to create new group IDs so that we are not mixing up groups)
  
  for(X.group in c("R.group", "S.group", "D.group")) {
    
    unique.X.group <- my_na.omit(unique(unlist(strsplit(X[, paste0("old_", X.group)], ";"))))
    all.other.X.group <- my_na.omit(unique(unlist(strsplit( MEASUREMENTS.final[!MEASUREMENTS.final$split.ID %in% split.ID,][, X.group], ";"))))
    
    if(any(unique.X.group %in% all.other.X.group)) {
      warning("We need to create new a new group ID because the ones in old_X.group already exist")
      new.X.group <- max(as.numeric(my_na.omit(unlist(strsplit(MEASUREMENTS.final[, X.group], ";"))))) + seq(length(unique.X.group))
      
      X[, X.group] <- new.X.group[match(X[, paste0("old_", X.group)], unique.X.group)]
      
    }
    if(!any(unique.X.group %in% all.other.X.group)) X[, X.group] <- X[, paste0("old_", X.group)]
    
  }
  
  MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID,] <- X
}


## double check a few things ####

### make sure we've got all measurement.IDs ####
all(MEASUREMENTS.final$measurement.ID %in% MEASUREMENTS$measurement.ID) & all(MEASUREMENTS$measurement.ID %in% MEASUREMENTS.final$measurement.ID) # HAS TO BE TRUE!!!!

### make sure there is no D.precedence or D.precedence.measurement.ID given when there is not D.group or D in conflicts ####
if( any(!is.na(MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence))) MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),][!is.na(MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence), ]$D.precedence <- NA

if( any(!is.na(MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence.measurement.ID))) MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),][!is.na(MEASUREMENTS.final[!grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence.measurement.ID), ]$D.precedence.measurement.ID <- NA

### make sure there is no "NA" for D.precedence when there is not D.group or D in conflicts ####
missing.D.precedence.split.ID <- sort(unique(MEASUREMENTS.final[grepl("D", MEASUREMENTS.final$conflicts),][is.na(MEASUREMENTS.final[grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence),]$split.ID))

for(split.ID in missing.D.precedence.split.ID) {
  X <- MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID & grepl("D", MEASUREMENTS.final$conflicts), ]
  print(X)
  # readline()
}

## Pop up window to say if there is D.precedence that needs to be manually edited ####

NAC.D.precedence.split.ID <- sort(unique(MEASUREMENTS.final[grepl("D", MEASUREMENTS.final$conflicts),][MEASUREMENTS.final[grepl("D", MEASUREMENTS.final$conflicts),]$D.precedence %in% "NAC",]$split.ID))

for(split.ID in NAC.D.precedence.split.ID) {
  X <- MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% split.ID, ]
  print(X)
  # readline()
}

NAC.D.precedence.D.group <- sort(as.numeric(unique(unlist(strsplit(MEASUREMENTS.final[MEASUREMENTS.final$split.ID %in% NAC.D.precedence.split.ID, ]$D.group, ";")))))

msg_box(paste("There is", length(NAC.D.precedence.D.group), "groups where D precedence needs to be given manually. But don't worry, it will ba faster than you think to fix them manually."))

# remove columns we don't want to keep ####
names(MEASUREMENTS.final)[!names(MEASUREMENTS.final) %in% sets.of.columns.to.keep.at.the.end]

MEASUREMENTS.final <- MEASUREMENTS.final[, sets.of.columns.to.keep.at.the.end]


# SAVE ####
write.csv(MEASUREMENTS.final, "data/ForC_measurements.csv", row.names = F)
