######################################################
# Purpose: fill in the conflicts, R.group, S.group, D.group and conflict.type fields of MEASUREMENTS table to make it easier to ID the duplicates sets that we should look at to reconcile duplicates records. Krista then goes through and gives a precedence ID to individual records within D.group
# Inputs:
# - MEASUREMENTS table
# outputs: - MEASUREMENTS table with updated duplicate system coding
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-29)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)


# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999") 
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

### calculate span between start and end

range.span <- difftime(MEASUREMENTS$my.end.date, MEASUREMENTS$my.start.date, units = "days")
table(range.span)
MEASUREMENTS[which(range.span < 0), ]

### if spans less than a year (and a day), put start date in date and remove range dates
A <- MEASUREMENTS[which(range.span <= 367 & !is.na(MEASUREMENTS$my.date)), ]
A <- A[difftime(A$my.date, A$my.start.date, units = "days") != 0,]
A <- A[, c("measurement.ID", "stand.age", "date", "start.date", "end.date", "my.date", "my.start.date", "my.end.date")] 

MEASUREMENTS[which(range.span <= 367), ]$my.date <- MEASUREMENTS[which(range.span <= 367), ]$my.start.date
MEASUREMENTS[which(range.span <= 367), ]$my.start.date <- NA
MEASUREMENTS[which(range.span <= 367), ]$my.end.date <- NA

warning(paste(nrow(A), "dates were different than start.date"), immediate. = T)

### if spans more than a year (and a day) keep range dates and put NA to dates (in case there is any)
A <- MEASUREMENTS[which(range.span > 367 & !is.na(MEASUREMENTS$my.date)), ]

MEASUREMENTS[which(range.span > 367), ]$my.date <- NA
warning(paste(nrow(A), "record had both range and dates pecified"), immediate. = T)


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
  any.duplicated.start.dates <- any(duplicated(na.omit(X$my.start.date)))
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
  
  if(nrow(X) == 1) X$conflicts <- paste(X$conflicts, "I", sep = ",") # if only one record for that plot and that variable --> independant record
  
  if(nrow(X) > 1) { # if more than one record...  
    
    ## If any duplicated dates or start.date (excluding NA) OR, if all dates are missing, if any duplicated stand.gae (excluding NA)
    if(any.duplicated.dates | any.duplicated.start.dates | (all.types.dates.NA & any.duplicated.stand.age)) { # if any duplicated dates or start.date or stand.gae (if missing all dates),  (excluding NA)

      if(any.duplicated.dates)  date.to.look.at <- "my.date"
      if(any.duplicated.start.dates)  date.to.look.at <- "my.start.date"
      if(all.types.dates.NA & any.duplicated.stand.age) date.to.look.at <- "stand.age"
    
      if(any.duplicated.dates & any.duplicated.start.dates) {
        print(X)
        warning("need to improve code for that case...", immediate. = T)
        readline("press [enter]")
      }
      
      X.split <- split(X, addNA(X[, date.to.look.at])) # split by date
        
      for(s in 1:length(X.split)){ # look into each date subset
        
        x <- X.split[[s]]
        
        if(!is.na(names((X.split))[s])){ # if date is not NA, care about. otherwise, don't do anything, it will be taken car of in another case
          if(nrow(x) == 1) x$conflicts = paste(x$conflicts, "I", sep = "," ) # if only one record per date --> independant record
        
          if(nrow(x) > 1 ){ # if more than one record, need to look more into it...
          
            if(length(unique(x$stand.age)) != length(unique(x[, ifelse(date.to.look.at == "my.date", "date", "start.date")]))) { # if one stand.age is not associated to one date
            # print(X)
            warning(paste("Problem of stand.age and date for i =", i))
            # readline("press[enter]")
          } # if one stand.age is not associated to one date
          
            all.same.study <- length(unique(x$citation.ID)) == 1 # are all record the from the same study ?
            all.same.method <- length(unique(x$method.ID)) == 1  # are all record the from the same method ?
            all.same.notes <-  length(unique(x$notes)) == 1 # have all record the same note ?
            all.same.C.units <- length(unique(x$variable.name)) == 1 # have all record the same carbon units ?
            all.same.depth <- length(unique(x$depth)) == 1 # have all record the same depth ?
          
            all.same.start.date <- ifelse(any.duplicated.start.dates, length(unique(x$start.date)) == 1, TRUE) # have all range record the exact same start date ?
            all.same.end.date <- ifelse(any.duplicated.start.dates, length(unique(x$end.date)) == 1, TRUE) # have all range record the exact same end date ?
          
          # only replicates (all same study & all same method & all same notes & all same carbon units)
            if(all.same.study & all.same.method & all.same.notes & all.same.C.units & all.same.start.date & all.same.end.date & all.same.depth) { # if only replicates
              
              R.group.ID <- R.group.ID +1
              
              x$R.group <- paste(x$R.group, R.group.ID, sep = ",") # paste(i, s, "only replicates") # give a R.group_ID
              x$conflicts <- paste(x$conflicts, "R", sep = "," ) # give a conflict value (beside existing one)
            }  # if only replicates
          
          # Not only replicates (and maybe not at all)...
            if(any(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.start.date, !all.same.end.date, !all.same.depth)) { # if not only replicates
              
              D.group.ID <- D.group.ID +1
            
              ## give D conflict value and D.group ID to all
              x$D.group <- paste(x$D.group, D.group.ID, sep = ",") # paste(i, s, "duplicates")
              x$conflicts <-  paste(x$conflicts, "D", sep = "," ) # give a conflict value (beside existing one)
              
              ## ID sub groups of replicates, if any
              
              all.fields.to.look.at.pasted <- paste(x$variable.name, x$notes, x$method.ID, x$citation.ID, x$depth, ifelse(any.duplicated.start.dates, paste(x$start.date, x$end.date), ""))
              
              unique.all.fields.to.look.at.combinations <- as.data.frame(table(all.fields.to.look.at.pasted), stringsAsFactors = F)
              
              replicates.all.fields.to.look.at.combinations <- unique.all.fields.to.look.at.combinations[unique.all.fields.to.look.at.combinations$Freq > 1, ]$Var1 # look at combinations that are given to more than one record
              
              idx.replicates.amongs.duplicates <- all.fields.to.look.at.pasted %in% replicates.all.fields.to.look.at.combinations # identify those records that are not alone to have the method and citation combinations
              
              if(any(idx.replicates.amongs.duplicates)) { # if there are replicates in the group of duplicates
                
                R.group.ID <- R.group.ID +1
                
                x[idx.replicates.amongs.duplicates, ]$R.group <- paste(x[idx.replicates.amongs.duplicates, ]$R.group, R.group.ID, sep = ",") # paste(i, s, "replicates amongst duplicates") # give a R.group_ID
                
                x[idx.replicates.amongs.duplicates, ]$conflicts <- paste( x[idx.replicates.amongs.duplicates,]$conflicts, "R", sep = "," ) # give a conflict value (beside existing one)
              } # if there are replicates in the group of duplicates
              
              ## Add conflict.type (same for all in group of duplicates, depends on what is not unique)
              
              conflict.types <- unique(c("M", "M", "M", "C", "M", "T", "T") [c(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth,  !all.same.start.date, !all.same.end.date)])
              x$conflict.type <- paste(x$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
              
            } # if not only replicates
            
        }  # if more than one record, need to look more into it
        }  # if date is not NA, care about. otherwise, don't do anything, it will be taken car of in another case
        
        X.split[[s]] <- x
      } # look into each date subset
        
        X <- do.call(rbind, X.split)
        
      } # if any duplicated dates or start.date or stand.gae (if missing all dates),  (excluding NA)
    
    ## if any range record
    if(any.range){ # if dates and ranges together
      
      ### first look at if some ranges are in conflict
      idx.range.date <- which(!is.na(X$my.start.date))
      
      while(length(idx.range.date > 1)) { # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
        
        idx.x <- idx.range.date[1]
        idx.y <- idx.range.date[-1]
        
        x <- X[idx.x, ]
        y <- X[idx.y, ]
        
        overlap <- !(x$my.start.date +1 > y$my.end.date  | y$my.start.date +1 > x$my.end.date)
        
        if(any(overlap)) { # if there is any overlap
          
          overlap.idx <- idx.range.date[-1][overlap]
          
          overlap.already.found <- any(table(as.numeric(unlist(sapply(as.character(X[c(idx.x, overlap.idx), "D.group"]), strsplit, ",")))) > 1)
          
          if(!overlap.already.found) { # if the overlap was not already found in previous rounds
            
            ## give D conflict value and D.group ID
            
            D.group.ID <- D.group.ID +1
            
            this.case.idx <- c(idx.x, overlap.idx)
            
            X[this.case.idx, "D.group"] <- paste( X[this.case.idx, "D.group"], D.group.ID, sep = "," )
            X[this.case.idx, "conflicts"] <- paste( X[this.case.idx, "conflicts"], "D", sep = "," )
            
            ## give the conflict.type values T (besides others)
            
            all.same.study <- length(unique(X[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
            all.same.method <- length(unique(X[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
            all.same.notes <-  length(unique(X[this.case.idx, ]$notes)) == 1 # have all record the same note ?
            all.same.C.units <- length(unique(X[this.case.idx,]$variable.name)) == 1 # have all record the same carbon units ?
            all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
            
            
            conflict.types <- unique(c("M", "M", "M", "C", "M", "T") [c(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, all.same.depth, TRUE)])
            X[this.case.idx, ]$conflict.type <- paste(X[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
            
            
          } # if the overlap was not already found in previous rounds
          
        } # if there is any overlap
        
        
        idx.range.date <- idx.range.date[-1] # remove range of focus so that we don't look at it again
        
      }  # look at one range at time and find overlaps, then ignore it. Do this until no other range to look at
      
      ### Second, for each range, look at if there is a 1-to-1 or 1-to-many
      idx.range.date <- which(!is.na(X$my.start.date))
      
      for(r in 1:length(idx.range.date)) { # loop through each range
        
        idx.y <- idx.range.date[-r]
        
        if(length(idx.range.date) == 1) x <- X
        if(length(idx.range.date) > 1) x <- X[-idx.y, ]
        
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
          
        if(length(idx.range.date.subset) != 1) stop("Error: problem in coding, length(idx.range.date.subset) should be 1")
        
        if(no.conflict)  x$conflicts <- paste(x$conflicts, "I", sep = "," )
        
        if(one.to.one.conflict) { # if 1-to-1 conflict
          
          ## give D conflict value and D.group ID to all non-NA
          
          D.group.ID <- D.group.ID +1
          
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          x$D.group[this.case.idx] <- paste( x$D.group[this.case.idx], D.group.ID, sep = ",") # paste(i, s, "duplicates")
          x$conflicts[this.case.idx] <-  paste(x$conflicts[this.case.idx], "D", sep = "," ) # give a conflict value (beside existing one)
          ## give the conflict.type values T (besides others)
          
          all.same.study <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx,]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "T") [c(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
          
        } # if 1-to-1 conflict
        if(one.to.many.conflict) { # if 1-to-many conflict
          
          S.group.ID <- S.group.ID +1
          
          ## give s code to the dates that are within range
          x$conflicts[idx.non.range.dates[dates.within.range]] <- paste(x$conflicts[idx.non.range.dates[dates.within.range]], "s", sep = "," ) # give a small s for those dates that are within the range (beside existing one)
          
          ## give S code to the range record
          x$conflicts[idx.range.date.subset] <- paste(x$conflicts[idx.range.date.subset], "S", sep = "," ) # give a capital s for the record that is the range
          x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] <- paste(x$S.group[c(idx.range.date.subset,idx.non.range.dates[dates.within.range])] , S.group.ID, sep = ",") # paste(i, "one-to-many-conflict") # give a S.group_ID
          
          ## give the conflict.type values T (besides others)
          this.case.idx <- c(idx.range.date.subset, idx.non.range.dates[dates.within.range]) # look at range record and non-range record
          
          all.same.study <- length(unique(x[this.case.idx, ]$citation.ID)) == 1 # are all record the from the same study ?
          all.same.method <- length(unique(x[this.case.idx, ]$method.ID)) == 1  # are all record the from the same method ?
          all.same.notes <-  length(unique(x[this.case.idx, ]$notes)) == 1 # have all record the same note ?
          all.same.C.units <- length(unique(x[this.case.idx, ]$variable.name)) == 1 # have all record the same carbon units ?
          all.same.depth <- length(unique(X[this.case.idx,]$depth)) == 1 # have all record the same depth ?
          
          conflict.types <- unique(c("M", "M", "M", "C", "M", "T") [c(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, TRUE)])
          x[this.case.idx, ]$conflict.type <- paste(x[this.case.idx, ]$conflict.types, paste(conflict.types, collapse = ",") , sep = "," )
        } # if 1-to-many conflict
        
        ### put back in X
        
        if(length(idx.range.date) == 1) X <- x
        if(length(idx.range.date) > 1) X[-idx.y, ] <- x
       
      }
      
      } # if dates and ranges together
    
    
    ## missing dates amongst other records with dates
    if((!all.types.dates.NA & any.all.types.dates.NA) | (all.types.dates.NA & any.stand.age.NA)) { # if any missing dates, give s category
      
      if(!all.types.dates.NA & any.all.types.dates.NA) idx.NA <- which(paste0(X$my.date, X$my.start.date) %in% "NANA")
      if(all.types.dates.NA & any.stand.age.NA) idx.NA <- which(my_is.na(X$stand.age))
      
      ## give S code to the dates that are NA
      X$conflicts[idx.NA] <- paste(X$conflicts[idx.NA], "S", sep = "," )
      
      ## give s code to the range record
      X$conflicts[-idx.NA] <- paste(X$conflicts[-idx.NA], "s", sep = "," )
      
      ## give S.group ID
      
      S.group.ID <- S.group.ID +1
      
      X$S.group <- paste(X$S.group, S.group.ID, sep = ",") # give a S.group_ID (same as others if any within this subset of data)
      
      ## give the conflict.type values T (besides others)
      
      ## give the conflict.type values T (besides others)
      
      all.same.study <- length(unique(X$citation.ID)) == 1 # are all record the from the same study ?
      all.same.method <- length(unique(X$method.ID)) == 1  # are all record the from the same method ?
      all.same.notes <-  length(unique(X$notes)) == 1 # have all record the same note ?
      all.same.C.units <- length(unique(X$variable.name)) == 1 # have all record the same carbon units ?
      all.same.depth <- length(unique(X$depth)) == 1 # have all record the same depth ?
      
      conflict.types <- unique(c("M", "M", "M", "C", "M", "T") [c(!all.same.study, !all.same.method, !all.same.notes, !all.same.C.units, !all.same.depth, TRUE)])
      
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
  
  
  
 
  
  # save output into final object####
  MEASUREMENTS.final[[i]] <- X
}

MEASUREMENTS.final.split <- MEASUREMENTS.final
MEASUREMENTS.final <- do.call(rbind, MEASUREMENTS.final)

# remove columns we don't want to keep ####
names(MEASUREMENTS.final)[!names(MEASUREMENTS.final) %in% sets.of.columns.to.keep.at.the.end]

MEASUREMENTS.final <- MEASUREMENTS.final[, sets.of.columns.to.keep.at.the.end]

# SAVE ####
# write.csv(MEASUREMENTS.final, "data/ForC_measurements.csv", row.names = F)
