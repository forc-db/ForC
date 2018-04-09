######################################################
# Purpose: fill in the conflicts, R.group, S.group, D.group, D.precedence, conflict.type and conflict.notes fields of MEASUREMENTS table to make it easier to ID the duplicates sets that we should look at to reconcile duplicates records. D.precedence is given but some records are left to Krista to assign manually
# Inputs:
# - MEASUREMENTS table
# outputs: - MEASUREMENTS table with updated duplicate system coding
# IMPORTANT NOTE: - THE OUTPUT WAS EDITED BY HAND BY KRISTINA ANDERSON-TEIXEIRA, "I've finished assigning D.precedence. I edited some records by hand (and added conflict.notes). There were a couple instances where I changed fields other than D.precedence.". SO WHEN RUNNING THIS CODE AGAIN, OUTPUT SHOULD BE COMPARED TO ORIGINAL AND DECISIONS NEED TO BE MADE ON THE CASE BY CASE BASIS FOR CONFLICTS.
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

MEASUREMENTS$my.date <- ifelse(!is.na(MEASUREMENTS$my.start.date) & !is.na(MEASUREMENTS$my.end.date), NA,  MEASUREMENTS$my.date)
class(MEASUREMENTS$my.date) <- "Date"

### take floor of all dates
MEASUREMENTS$my.date <- year(MEASUREMENTS$my.date)
MEASUREMENTS$my.date <- ifelse(is.na(MEASUREMENTS$my.date), NA, paste0(MEASUREMENTS$my.date, "-01-01"))
MEASUREMENTS$my.date <- as.Date(MEASUREMENTS$my.date)

MEASUREMENTS$my.start.date <- year(MEASUREMENTS$my.start.date)
MEASUREMENTS$my.start.date <- ifelse(is.na(MEASUREMENTS$my.start.date), NA, paste0(MEASUREMENTS$my.start.date, "-01-01"))
MEASUREMENTS$my.start.date <- as.Date(MEASUREMENTS$my.start.date)

MEASUREMENTS$my.end.date <- year(MEASUREMENTS$my.end.date)
MEASUREMENTS$my.end.date <- ifelse(is.na(MEASUREMENTS$my.end.date), NA, paste0(MEASUREMENTS$my.end.date, "-01-01"))
MEASUREMENTS$my.end.date <- as.Date(MEASUREMENTS$my.end.date)


## consider NA as a factor level for plot.names (otherwise doesn't work when splitting the data) ####
MEASUREMENTS$plot.name <- addNA(MEASUREMENTS$plot.name)



# SPlit Measurement by site plot and variables name ####

MEASUREMENTS.split <- split(MEASUREMENTS, list(MEASUREMENTS$sites.sitename, MEASUREMENTS$plot.name, MEASUREMENTS$variable.name.combined), drop = TRUE)


# RUN THE CODE TO ID SETS OF DUPLICATE RECORDS ####

MEASUREMENTS.final <- NULL

R.group.ID <- ifelse(any(!is.na(MEASUREMENTS$R.group)), max(MEASUREMENTS$R.group, na.rm = T), 0)
S.group.ID <- ifelse(any(!is.na(MEASUREMENTS$S.group)), max(MEASUREMENTS$S.group, na.rm = T), 0)
D.group.ID <- ifelse(any(!is.na(MEASUREMENTS$D.group)), max(MEASUREMENTS$D.group, na.rm = T), 0)

for(i in 1:length(MEASUREMENTS.split)){
  
  X <- MEASUREMENTS.split[[i]]
  
  any.duplicated.dates <- any(duplicated(na.omit(X$my.date)))
  # any.duplicated.start.dates <- any(duplicated(na.omit(X$my.start.date)))
  any.duplicated.stand.age <- any(duplicated(my_na.omit(X$stand.age)))
  
  all.types.dates.NA <- all(is.na(c(X$my.date, X$my.start.date)))
  all.dates.NA <- all(is.na(X$my.date))
  all.start.dates.NA <-  all(is.na(X$my.start.date))
  all.stand.age.NA <- all(my_is.na(X$stand.age))
  
  
  any.all.types.dates.NA <- any(paste0(X$my.date, X$my.start.date) %in% "NANA")
  any.dates.NA <- any(is.na(X$my.date))
  any.start.dates.NA <-  any(is.na(X$my.start.date))
  any.stand.age.NA <- any(my_is.na(X$stand.age))
  
  only.dates <- !all.dates.NA & all.start.dates.NA
  only.range <- !all.start.dates.NA & all.dates.NA
  
  any.range <- !all.start.dates.NA
  
  dates.and.ranges <- !all.dates.NA & !all.start.dates.NA
  
  sum.of.range.records <- sum(!is.na(X$my.start.date))
  
  if (nrow(X) == 1) X$conflicts <- paste(X$conflicts, "I", sep = ",") # if only one record for that plot and that variable --> independant record
  
  if (nrow(X) > 1) { # if more than one record...  
    
    ## If any duplicated dates or start.date (excluding NA) OR, if all dates are missing, if any duplicated stand.gae (excluding NA)
    if (any.duplicated.dates | (all.types.dates.NA & any.duplicated.stand.age)) { # if any duplicated dates or start.date or stand.gae (if missing all dates),  (excluding NA)

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
        
        if (!is.na(names((X.split))[s])){ # if date is not NA, care about. otherwise, don't do anything, it will be taken car of in another case
          if (nrow(x) == 1) x$conflicts = paste(x$conflicts, "I", sep = "," ) # if only one record per date --> independant record
        
          if (nrow(x) > 1 ){ # if more than one record, need to look more into it...
          
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
            
            # all.same.start.date <- ifelse(any.duplicated.start.dates, length(unique(x$start.date)) == 1, TRUE) # have all range record the exact same start date ?
            # all.same.end.date <- ifelse(any.duplicated.start.dates, length(unique(x$end.date)) == 1, TRUE) # have all range record the exact same end date ?
          
          # only replicates (all same study & all same method & all same notes & all same carbon units)
            # if (all.same.citation & all.same.method & all.same.notes & all.same.C.units & all.same.start.date & all.same.end.date & all.same.depth)
            if (all.same.citation & all.same.method & all.same.notes & all.same.C.units & all.same.depth & all.same.dbh) { # if only replicates
              
              R.group.ID <- R.group.ID +1
              
              x$R.group <- paste(x$R.group, R.group.ID, sep = ",") # paste(i, s, "only replicates") # give a R.group_ID
              x$conflicts <- paste(x$conflicts, "R", sep = "," ) # give a conflict value (beside existing one)
            }  # if only replicates
          
          # Not only replicates (and maybe not at all)...
            # if (any(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.start.date, !all.same.end.date, !all.same.depth))
            if (any(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh)) { # if not only replicates
              
              D.group.ID <- D.group.ID +1
            
              ## give D conflict value and D.group ID to all
              x$D.group <- paste(x$D.group, D.group.ID, sep = ",") # paste(i, s, "duplicates")
              x$conflicts <-  paste(x$conflicts, "D", sep = "," ) # give a conflict value (beside existing one)
              
              ## ID sub groups of replicates, if any
              
              all.fields.to.look.at.pasted <- paste(x$variable.name, x$notes, x$method.ID, x$citation.ID, x$depth)
              # all.fields.to.look.at.pasted <- paste(x$variable.name, x$notes, x$method.ID, x$citation.ID, x$depth, ifelse(any.duplicated.start.dates, paste(x$start.date, x$end.date), ""))
              
              unique.all.fields.to.look.at.combinations <- as.data.frame(table(all.fields.to.look.at.pasted), stringsAsFactors = F)
              
              replicates.all.fields.to.look.at.combinations <- unique.all.fields.to.look.at.combinations[unique.all.fields.to.look.at.combinations$Freq > 1, ]$Var1 # look at combinations that are given to more than one record
              
              idx.replicates.amongs.duplicates <- all.fields.to.look.at.pasted %in% replicates.all.fields.to.look.at.combinations # identify those records that are not alone to have the method and citation combinations
              
              if (any(idx.replicates.amongs.duplicates)) { # if there are replicates in the group of duplicates
                
                R.group.ID <- R.group.ID +1
                
                x[idx.replicates.amongs.duplicates, ]$R.group <- paste(x[idx.replicates.amongs.duplicates, ]$R.group, R.group.ID, sep = ",") # paste(i, s, "replicates amongst duplicates") # give a R.group_ID
                
                x[idx.replicates.amongs.duplicates, ]$conflicts <- paste( x[idx.replicates.amongs.duplicates,]$conflicts, "R", sep = "," ) # give a conflict value (beside existing one)
              } # if there are replicates in the group of duplicates
              
              ## Add conflict.type (same for all in group of duplicates, depends on what is not unique)
              
              # conflict.types <- unique(c("M", "M", "M", "C", "M", "T", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth,  !all.same.start.date, !all.same.end.date)])
              conflict.types <- unique(c("M", "M", "M", "C", "M", "M") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh)])
              x$conflict.type <- paste(x$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
              
            } # if not only replicates
            
        }  # if more than one record, need to look more into it
        }  # if date is not NA, care about. otherwise, don't do anything, it will be taken car of in another case
        
        X.split[[s]] <- x
      } # look into each date subset
        
        X <- do.call(rbind, X.split)
      # } # loop if there is both duplicated dates and duplicated start.dates

        
    } # if any duplicated dates or start.date or stand.gae (if missing all dates),  (excluding NA)
    
    ## if any range record
    if (any.range){ # if dates and ranges together
      
      ### first look at if some ranges are in conflict
      
      idx.range.date <- which(!is.na(X$my.start.date))
      
      #### order by start date then end date.
      order.range.date <- order(X[idx.range.date,]$my.start.date, X[idx.range.date,]$my.end.date)
      idx.range.date <- idx.range.date[order.range.date]
      
      while(length(idx.range.date > 1)) { # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
        
        idx.x <- idx.range.date[1]
        idx.y <- idx.range.date[-1]
        
        x <- X[idx.x, ]
        y <- X[idx.y, ]
        
        overlap <- !(x$my.start.date +1 > y$my.end.date  | y$my.start.date +1 > x$my.end.date)
        
        if (any(overlap)) { # if there is any overlap
          
          overlap.idx <- idx.range.date[-1][overlap]
          
          overlap.already.found <- any(table(as.numeric(unlist(sapply(as.character(X[c(idx.x, overlap.idx), "D.group"]), strsplit, ",")))) > 1)
          
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

              R.group.ID <- R.group.ID +1
              
              
              X[this.case.idx, "R.group"] <- paste( X[this.case.idx, "R.group"], R.group.ID, sep = "," )
              X[this.case.idx, "conflicts"] <- paste( X[this.case.idx, "conflicts"], "R", sep = "," )
              
            } # if it is a replicate
              
            
            ## If NOT everything is the same, it is a duplicate
            if(length(conflict.types) > 0) { # if it is a duplicate
              
              D.group.ID <- D.group.ID +1
              
              
              X[this.case.idx, "D.group"] <- paste( X[this.case.idx, "D.group"], D.group.ID, sep = "," )
              X[this.case.idx, "conflicts"] <- paste( X[this.case.idx, "conflicts"], "D", sep = "," )
              X[this.case.idx, "conflict.type"]<- paste(X[this.case.idx, "conflict.type"], paste(conflict.types, collapse = ",") , sep = "," )
              
              
            } # if it is a duplicate
            
            
          } # if the overlap was not already found in previous rounds
          
        } # if there is any overlap
        
        idx.range.date <- idx.range.date[-1] # remove range of focus so that we don't look at it again
        
      }  # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
      
      ### Second, for each range, look at if there is a 1-to-1 or 1-to-many
      idx.range.date <- which(!is.na(X$my.start.date))
      
      for(r in 1:length(idx.range.date)) { # loop through each range
        
        idx.y <- idx.range.date[-r]
        
        if (length(idx.range.date) == 1) x <- X
        if (length(idx.range.date) > 1) x <- X[-idx.y, ]
        
        ### get the range of dates
        the.one.start.date <- na.omit(unique(x$my.start.date))
        the.one.end.date <- na.omit(unique(x$my.end.date))
        
        ### get the idx of the different type of dates
        idx.range.date.subset <- which(x$my.start.date %in% the.one.start.date)
        idx.non.range.dates <- which(is.na(x$my.start.date)) #including NA
        
        ### what dates are within the range or NA ?
        dates.within.range <- na.omit(x$my.date[idx.non.range.dates] >= the.one.start.date & x$my.date[idx.non.range.dates] <= the.one.end.date)
        dates.oustide.range <- na.omit(x$my.date[idx.non.range.dates] < the.one.start.date | x$my.date[idx.non.range.dates] > the.one.end.date)
        dates.NA <- is.na(x$my.date[idx.non.range.dates])
        
        ## what type of conflict do we have
        no.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) == 0
        one.to.one.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) == 1
        one.to.many.conflict <- length(idx.range.date.subset) == 1 & sum(dates.within.range) > 1 
          
        if (length(idx.range.date.subset) != 1) stop("Error: problem in coding, length(idx.range.date.subset) should be 1")
        
        if (no.conflict)  x$conflicts <- paste(x$conflicts, "I", sep = "," )
        
        if (one.to.one.conflict) { # if 1-to-1 conflict
          
          ## give D conflict value and D.group ID to all non-NA
          
          D.group.ID <- D.group.ID +1
          
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          x$D.group[this.case.idx] <- paste( x$D.group[this.case.idx], D.group.ID, sep = ",") # paste(i, s, "duplicates")
          x$conflicts[this.case.idx] <-  paste(x$conflicts[this.case.idx], "D", sep = "," ) # give a conflict value (beside existing one)
          ## give the conflict.type values T (besides others)
          
          all.same.citation <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx,]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          all.same.dbh <- length(unique(X[this.case.idx,]$min.dbh)) == 1# have all record the same min.dbh ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
          
        } # if 1-to-1 conflict
        if (one.to.many.conflict) { # if 1-to-many conflict
          
          S.group.ID <- S.group.ID +1
          
          ## give s code to the dates that are within range
          x$conflicts[idx.non.range.dates[dates.within.range]] <- paste(x$conflicts[idx.non.range.dates[dates.within.range]], "s", sep = "," ) # give a small s for those dates that are within the range (beside existing one)
          
          ## give S code to the range record
          x$conflicts[idx.range.date.subset] <- paste(x$conflicts[idx.range.date.subset], "S", sep = "," ) # give a capital s for the record that is the range
          x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] <- paste(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] , S.group.ID, sep = ",") # paste(i, "one-to-many-conflict") # give a S.group_ID
          
          ## give the conflict.type values T (besides others)
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          all.same.citation <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx, ]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          all.same.dbh <- length(unique(X[this.case.idx,]$min.dbh)) == 1# have all record the same min.dbh ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
        } # if 1-to-many conflict
        
        ### put back in X
        
        if (length(idx.range.date) == 1) X <- x
        if (length(idx.range.date) > 1) X[-idx.y, ] <- x
       
      }
      
      } # if dates and ranges together
    
    
    ## missing dates amongst other records with dates
    if ((!all.types.dates.NA & any.all.types.dates.NA) | (all.types.dates.NA & any.stand.age.NA)) { # if any missing dates, give s category
      
      if (!all.types.dates.NA & any.all.types.dates.NA) idx.NA <- which(paste0(X$my.date, X$my.start.date) %in% "NANA")
      if (all.types.dates.NA & any.stand.age.NA) idx.NA <- which(my_is.na(X$stand.age))
      
      ## give S code to the dates that are NA
      X$conflicts[idx.NA] <- paste(X$conflicts[idx.NA], "S", sep = "," )
      
      ## give s code to the range record
      X$conflicts[-idx.NA] <- paste(X$conflicts[-idx.NA], "s", sep = "," )
      
      ## give S.group ID
      
      S.group.ID <- S.group.ID +1
      
      X$S.group <- paste(X$S.group, S.group.ID, sep = ",") # give a S.group_ID (same as others if any within this subset of data)
      
      ## give the conflict.type values T (besides others)
      
      ## give the conflict.type values T (besides others)
      
      all.same.citation <- length(unique(X$citation.ID)) == 1 # are all record the from the same study ?
      all.same.method <- length(unique(X$method.ID)) == 1  # are all record the from the same method ?
      all.same.notes <-  length(unique(X$notes)) == 1 # have all record the same note ?
      all.same.C.units <- length(unique(X$variable.name)) == 1 # have all record the same carbon units ?
      all.same.depth <- length(unique(X$depth)) == 1 # have all record the same depth ?
      all.same.dbh <- length(unique(X$min.dbh)) == 1# have all record the same min.dbh ?
      
      conflict.types <- unique(c("M", "M", "M", "C", "M", "M", "T") [c(!all.same.citation, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, !all.same.dbh, TRUE)])
      
      X$conflict.type <- paste(X$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
      
    } # if any missing dates, give s category
    
    
  } # if more than one record...  

  ### Clean up the group codes ####
  
  #### conflicts 
  x <- gsub("NA,", "", X$conflicts)
  x <- gsub(",", "", x)
  x <-  gsub('(.)\\1+', '\\1', x)
  x <- sapply(strsplit(x, ""), function(a) paste(sort(a), collapse = ",")) # order letters and paste them with comma inbetween
  x <- gsub("(,I)|(I,)", "", x)
  x <- ifelse(x %in% "", NA, x)
  x <- ifelse(is.na(x), "I", x) # to get all the records that were not ID-ed as duplicates
  X$conflicts <- x
  
  #### conflict.type 
  x <- gsub("NA,", "", X$conflict.type)
  x <- gsub(",", "", x)
  x <- gsub('(.)\\1+', '\\1', x)
  x <- sapply(strsplit(x, ""), function(a) paste(sort(a), collapse = ",")) # order letters and paste them with comma inbetween
  x <- ifelse(x %in% "", NA, x)
  x <- ifelse(x %in% "NA", NA, x)
  X$conflict.type <- x
  
  #### R.group 
  x <- gsub("NA,", "", X$R.group)
  X$R.group <- x
  
  #### S.group 
  x <- gsub("NA,", "", X$S.group)
  X$S.group <- x
  
  #### D.group 
  x <- gsub("NA,", "", X$D.group)
  X$D.group <- x
  
  
  
 
  
  
  
  ### Add prevalence ####
  # Prevalence is added following this list (We keep going down the list if D.precedence is still NA or if multiple "1" were assigned) :
  # 0. Never give precedence to a record with a capital S in the conflict field 
  # 1. Take biggest depth (deepest record)
  # 2. Take smallest min.dbh
  # 3. Take most inclusive looking at notes within a same study
  # 4. Take longer study when length_longer_record = 1.75 * length_of_its_duplicates
  # 5. Take OM over C
  # 6. If a record was checked against original publication, give it precedence over others
  # 7. Take later study over older
  # 8. If still not been able to pin point precedence (including records that only differ in method.ID.), give NAC to all precedence for future manual ratin + append "manual D.precendence rating to the notes

  idx.D.group <- which(grepl("D", X$conflicts))
  
  if (length(idx.D.group) > 0) { # if there is any duplicates
    
    unique.D.groups <- unique(unlist(sapply(X[idx.D.group, ]$D.group, strsplit, ",")))
    
    collecting.x <- NULL # This is an object that will hold the outputs of the loop below, in case some records belong to multiple groups of duplicates 
    
    for (d in unique.D.groups) { # loop through each D.group
      x <- X[grepl(d, X$D.group),]
      
      # get what the records are the same for ####
      all.same.citation <- length(unique(x$citation.ID)) == 1 # are all record the from the same study ?
      all.same.method <- length(unique(x$method.ID)) == 1  # are all record the from the same method ?
      all.same.notes <-  length(unique(x$notes)) == 1 # have all record the same note ?
      all.same.C.units <- length(unique(x$variable.name)) == 1 # have all record the same carbon units ?
      all.same.depth <- length(unique(x$depth)) == 1 # have all record the same depth ?
      all.same.dbh <- length(unique(x$min.dbh)) == 1# have all record the same min.dbh ?
      all.same.record.duration <- !any(grepl("T", x$conflict.type)) | all(grepl("T", x$conflict.type) & grepl("(s)|(S)", x$conflicts))
      
      any.checked.original.pub <- any(x$checked.ori.pub == 1)
      any.capital.S.in.conflicts <- any(grepl("S", x$conflicts, ignore.case = F))
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      # remove D.precedence to start from scratch (if there is any no NA in D.precedence at this points that means that the record belongs to several D.groups, this will be taken care of at the end (look for collecting.x)) ####
      
      
      if(!(still.more.than.one.1 | still.only.NAs)) {
        
        x$D.precedence <- NA
        print(i)
        print(x)
        warning("This is a case where we erased  D.precedence at the begining", immediate. = T)
        # readline("press [enter]")
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
      
      # 0. Never give precedence to a record with a capital S in the conflict field ####
      
      if((still.more.than.one.1 | still.only.NAs) & any.capital.S.in.conflicts) {
        
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.cap.S.in.conflicts <- grep("S", x[idx.to.look.at, ]$conflicts, ignore.case = F)
        
        if(length(idx.checked.original.pub) > 0){
          x[idx.to.look.at, ][idx.cap.S.in.conflicts, ]$D.precedence <- 0
         
        }
        
      }
      
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
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
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
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
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
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
          print(x$notes)
          print(x)
          warning("not a clear cut in notes (may be because not same study)", immediate. = T)
          # readline("press[enter]")
        }
        
      }
      
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
      
      # 4. If records differ in length. Give precedence to multiple-year measurement periods-- If one record measurement period is >1.75 x the length of its duplicate, go with that one. ####
      
      if ((still.more.than.one.1 | still.only.NAs) & !all.same.record.duration) { # if still need to go down the list and durations are different
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        record.duration <- difftime(x[idx.to.look.at,]$my.end.date, x[idx.to.look.at,]$my.start.date)
        record.duration <- ifelse(is.na(record.duration), 1, record.duration) # consider a record with only "date" to be 1 yaer long
        
        idx.max.record.duration <- which(record.duration == max(record.duration))
        
        if(length(idx.max.record.duration) < nrow(x[idx.to.look.at,])) {
          one.record.is.1.75.times.longer.than.all.the.others <- record.duration[idx.max.record.duration] > 1.75 * record.duration[-idx.max.record.duration]
          
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
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
      
      # 5. If records differ only in units (C or OM) and C = 0.45 to 0.55 * OM, give precedence to OM. The logic there is that researchers use slightly varying conversion factors, so when there's a choice its best to do the conversion ourselves. ####

      if ((still.more.than.one.1 | still.only.NAs) & !all.same.C.units) { # if still need to go down the list
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.C <- grep("_C", x[idx.to.look.at,]$variable.name)
        idx.OM <- grep("_OM", x[idx.to.look.at,]$variable.name)
      
      if(length(idx.OM) > 1 & length(idx.C) > 1) {
        print(i)
        print(x)
        warning("work in unit stuff when more than one record in both units", immediate. = T)
        readline("press [enter]")
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
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
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
        print(x)
        warning("This is a case of 'checked against original pub'", immediate. = T)
        
        
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
      
      # 7.After resolving all of the above, if duplicates are from different studies, the later study gets precedence. [NOTE: This isn't always ideal, as it may give precedence to an intermediary review over an original publication. However, it works as a start. Precedence can always be edited upon consultation of original pub.] ####
      
      if ((still.more.than.one.1 | still.only.NAs) & !all.same.citation) { # if still need to go down the list
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        x.year <- gsub("(.*)([0-9]{4})(.*)",'\\2', x[idx.to.look.at,]$citation.ID, perl = T)
        
        idx.max.citation.year <- which(x.year == max(x.year))
        
        if(length(idx.max.citation.year) < nrow(x[idx.to.look.at,])){ # if not all published same year
        
          x[idx.to.look.at,][idx.max.citation.year, ]$D.precedence <- 1
          x[idx.to.look.at,][-idx.max.citation.year, ]$D.precedence <- 0 
        }
        
      } # if still need to go down the list
       
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
     
      
      # 8. If still need to be pinned down, look at dup.num and use it , but add something about it in the notes field ####
      
      if ((still.more.than.one.1 | still.only.NAs)) {
        
        if(still.more.than.one.1) idx.to.look.at <- which(x$D.precedence == 1)
        if(still.only.NAs) idx.to.look.at <- 1:nrow(x)
        
        idx.max.dup.num <- which(x[idx.to.look.at, ]$dup.num %in% max(x[idx.to.look.at, ]$dup.num))
        
        if(length(idx.max.dup.num) %in% 1) {
          x[idx.to.look.at, ][idx.max.dup.num, ]$D.precedence <- 1
          x[idx.to.look.at, ][-idx.max.dup.num, ]$D.precedence <- 0
          x[idx.to.look.at, ]$conflicts.notes <- ifelse(is.na(x[idx.to.look.at, ]$conflicts.notes), "D.precedence based on previously dup.num column.", paste(x[idx.to.look.at, ]$conflicts.notes, "D.precedence based on previously dup.num column.", sep = ". "))
        }
        
      }
      
      still.more.than.one.1 <- sum(x$D.precedence == 1, na.rm = T) > 1
      still.only.NAs <- all(is.na(x$D.precedence))
      still.some.NAs <- sum(x$D.precedence == 1, na.rm = T) > 0 & any(is.na(x$D.precedence))
      
      if(still.some.NAs) {
        print(i)
        print(x)
        warning("some NA left in D.precedence", immediate. = T)
        readline("press [enter]")
      }
      
      # 9. As a last resort, put NAC in D.predence for Krista to do it manually ####
      
      if ((still.more.than.one.1 | still.only.NAs)) {
        
          x$D.precedence <- "NAC"
          x$conflicts.notes <- ifelse(is.na(x$conflicts.notes), "D.precedence given manually.", paste(x$conflicts.notes, "D.precedence given manually.", sep = ". "))
        
        
        print(i)
        print(x)
        warning("D.precedence given manually.", immediate. = T)
        # readline("press [enter]")
      
      }
      
      # rbind into collecting.x
      
      collecting.x <- rbind(collecting.x, x)
    
    } # loop through each D.group
    
    # deal with records that belong to 2 groups
    
    if(any(duplicated(collecting.x$measurement.ID))) {
      
      duplicated.measurement.ID <- unique(collecting.x$measurement.ID[duplicated(collecting.x$measurement.ID)])
      
      idx.to.look.at <- which(collecting.x$measurement.ID %in% duplicated.measurement.ID)
      
      all.same.precedence <- length(unique(collecting.x[idx.to.look.at, ]$D.precedence)) == 1
      
      if(all.same.precedence)     collecting.x <- collecting.x[!duplicated(collecting.x$measurement.ID), ]
      
      if(!all.same.precedence){
        
        collecting.x[idx.to.look.at, ]$D.precedence <- "NAC"
        collecting.x[idx.to.look.at, ]$conflicts.notes <- ifelse(is.na(collecting.x[idx.to.look.at, ]$conflicts.notes), "D.precedence given manually.", paste(collecting.x[idx.to.look.at, ]$conflicts.notes, "D.precedence given manually.", sep = ". "))
        
        collecting.x <- collecting.x[!duplicated(collecting.x$measurement.ID), ]
        
        print(i)
        print(collecting.x)
        warning("not an easy fix for records that belong to multiple D.groups...", immediate. = T)
        readline("press [enter]")
      }
      
    }
    
    # put back into X ####
    
    if(!all(X[idx.D.group, ]$measurement.ID %in% collecting.x$measurement.ID)) stop("make sure we've got all records") # this is to make sure we are putting records back in the main flow correctly
    
    X[idx.D.group, ] <- collecting.x
    
 
    } # if there is any duplicates
  
  # save output into final object####
  MEASUREMENTS.final[[i]] <- X
}

# re-formate output ####
MEASUREMENTS.final.split <- MEASUREMENTS.final
MEASUREMENTS.final <- do.call(rbind, MEASUREMENTS.final)

## remove columns we don't want to keep
names(MEASUREMENTS.final)[!names(MEASUREMENTS.final) %in% sets.of.columns.to.keep.at.the.end]

MEASUREMENTS.final <- MEASUREMENTS.final[, sets.of.columns.to.keep.at.the.end]

# SAVE ####
# write.csv(MEASUREMENTS.final, "data/ForC_measurements.csv", row.names = F)
