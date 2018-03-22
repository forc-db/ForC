######################################################
# Purpose: Select records of ForC among duplicates.Following these rulues:
#           - Replicate measurements (‘R’ code) should be averaged.
#           - For duplicate records reported by different studies (D, DC, or P codes), priority should be given to the most recently published estimate (i.e., highest number in dup.num field).
#           - For duplicates that differ only in methodology (M code), code should select records with the highest value. 
# Inputs: - MEASUREMENTS table
#         
# outputs: 1. png file, saved in figures folder as [Forest_type]_C_cylce_diagram.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-13)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(readr)


# Load data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv")

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999")  # various ways "NA" is encoded in ForC

# ~~~~~~~ may need to remove this ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
MEASUREMENTS$date <- addNA(MEASUREMENTS$date)
MEASUREMENTS$stand.age <- addNA(MEASUREMENTS$stand.age)
MEASUREMENTS$start.date <- addNA(MEASUREMENTS$start.date)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

# Deal with duplicates records ####
## dup.code:
##    - D or DC: Duplicate estimates of the same variable but with different values, where DC refers to instances where one value is given in carbon and the other in dry matter
##    - R: Replicate sampling of the same variables within the same study
##    - M: Multiple estimates of the same variables within the same study but using different methods
##    - P: Effectively duplicates another record, but with differences in names of plot or site (e.g., single site given two names, one plot is subsample of another).
##    - 0: no duplicates
## Multiple codes may apply.
## dup.num: Assigns a number to every entry in each set of duplicates/replicates in chronological order according to the citation.year. If two sources were published in the same year, the primary source (as opposed to a synthesis) comes first.

unique(MEASUREMENTS[, c("dup.code", "dup.num")])

## R codes
### 1- Keep records with R only in dup.code
### 2- Working one sites.sitename/plot.name at a time, average by stand age


R <- droplevels(MEASUREMENTS[MEASUREMENTS$dup.code %in% "R",])



R.split <- split(R, list(R$sites.sitename, R$plot.name, R$variable.name, R$date, R$stand.age, R$start.date), drop = TRUE)
table(do.call(rbind, lapply(R.split, dim)))

R.unsplit <- do.call(rbind, R.split)

if(!nrow(do.call(rbind, R.split)) == nrow(R)) stop("Error: problem in the way the data is split. need to remove one factor but then we don't have the right set of replicates... need to find a solution ") # if false, problem.... need to remove date but then we don't have the right set of replicates...

R[!R$measurement.ID %in% R.unsplit$measurement.ID,] # should be empty


new.R.records <- NULL

for(i in 1:length(R.split)){
  
  x <- R.split[[i]]
  
  if(nrow(x) <= 1){
    warning("Error: only one row for this duplicate record (so not duplicatesd ?)", immediate. = T)
    print(x)
    readline("Press [eneter] to continue")
  }
  
  
  columns.to.change <- c(colnames(x)[!apply(x, 2, function(x) all(x %in% x[1]))])
  
  new.x <- x[1,]
  
  for(c in columns.to.change){
    
    if(c %in% c("measurement.ID", "dup.num", "measurement.ID.v1")) new.x[, c] <- paste(x[, c], collapse = ".")
    
    if(c %in%  "mean"){ if(any(is.na(x$n))) stop("Problem with weighted mean") else new.x[, c] <- weighted.mean(x[, c], x$n, na.rm = T)
    
        if(length(unique(x$n)) > 1){
          cat("old")
          print(x[, c("measurement.ID", "sites.sitename", "plot.name", "stand.age", "mean", "n", "dup.code", "dup.num")])
          
          cat("new")
          print(new.x[, c("measurement.ID", "sites.sitename", "plot.name", "stand.age", "mean", "n", "dup.code", "dup.num")])
          
          readline("n was used for weighting, Click [enter]")
        }
    }
    
    if(c %in% "covariate_1"){ if(length(na.omit(unique(x$coV_1.value))) == 1)  new.x[, c] <- na.omit(unique(x$coV_1.value)) else stop("Problem with covariate_1, may need to chan dup.code to M")
    cat("old")
    print(x)
    readline("covariate_1 was fixed, Click [enter]")
    cat(new)
    print(new.x)
    }
    
    if(c %in% c("stat.name", "stat", "notes", "loaded.by", "source.notes")) new.x[, c] <- "NA.R"
    
    if(!c %in% c("measurement.ID", "mean", "n", "dup.num", "measurement.ID.v1", "stat.name", "stat")){
      print(x)
      warning(paste("Error: fix that particular case for", c), immediate. = T)
      readline("Click [enter] if you've seen it and are waiting to fix it")
  }
  

  }
  
  if(nrow(new.x) != 1) stop("Error: nrow(new.x)  is not equal to 1")
  
  new.R.records <- rbind(new.R.records, new.x)
  print(paste(i, nrow(new.R.records)))
}


### ~~~~~~~~~~~~~~~~~ WAIT FOR KRISTA FOR FINAL DECISION ON "stat.name", "stat", "n", "notes", "depth", "loaded.by", "covariate_1", "coV_1.value", "source.notes", "min.dbh", "method.ID" ~~~~~~ #####

## R codes
### 1- Keep records with R only in dup.code
### 2- Working one sites.sitename/plot.name at a time, average by stand age
