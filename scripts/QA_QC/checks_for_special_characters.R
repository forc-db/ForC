######################################################
# Purpose: Identify special characters and convert them to their closest equivalent
# Developped by: Valentine Herrmann - HerrmannV@si.edu (adapted from Ian McGregor - mcgregori@si.edu)
#  R version 3.5.1 (2018-07-02)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(tools)

# Load data ####

## ForC data ####
MEASUREMENTS <- read.csv("data/ForC_measurements.csv", stringsAsFactors = F, )
SITES        <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
HISTORY      <- read.csv("data/ForC_history.csv", stringsAsFactors = F)
METHODOLOGY  <- read.csv("data/ForC_methodology.csv", stringsAsFactors = F)
ALLOMETRY    <- read.csv("data/ForC_allometry.csv", stringsAsFactors = F)
CITATIONS    <- read.csv("data/ForC_citations.csv", stringsAsFactors = F, encoding = "UTF-8")

# Special characters conversion table ####
sp_ch_conv_tbl <- read.table("scripts/QA_QC/special_characters_convert_table.txt", h = T, encoding = "UTF-8", sep = ",", stringsAsFactors = F)



## remove special characters ####
chr.cols.to.check <- c("sites.sitename", "plot.name", "scientific.name", "veg.notes", 
                       "notes", "citation.ID", "loaded.from", "source.notes", "loaded.by", 
                       "ForC.investigator", "required.citations", "network", "alt.names", 
                       "super.site", "city", "state", "country", "geography.notes", 
                       "climate.notes", "soil.texture", "sand", "silt", "clay", "soil.classification", 
                       "soil.notes", "hydrology.notes", "site.notes", "measurement.refs", 
                       "site.ref", "ref.notes", "lacks.info.from.ori.pub", "hist.notes", 
                       "method.citation", "citation.doi", "citation.author", "citation.year", 
                       "citation.title", "citation.citation", "citation.language", "citation.url"#, "citation.abstract"
                       )

double.chck.char.changes <- NULL
non.ascii.detected <- NULL

for(Table_name in c("MEASUREMENTS", "SITES", "HISTORY", "METHODOLOGY", "ALLOMETRY", "CITATIONS")) {
  
  print(Table_name)
  Table <- get(Table_name)
  rownames(Table) <- 1:nrow(Table)
  
  for(chr.col in chr.cols.to.check) {
    print(chr.col)
    
    if(chr.col %in% names(Table)) {
      
      X <- Table[, chr.col]
      for(i in 1:nrow(sp_ch_conv_tbl)) {
        
        # print(i)
        X <-  gsub(sp_ch_conv_tbl$symbol[i], sp_ch_conv_tbl$equiv[i], X)
        
      } # for(i in 1:nrow(sp_ch_conv_tbl))
      
      double.chck.char.changes <- rbind(double.chck.char.changes, data.frame(with_special_character = unlist(Table[, chr.col]), fixed= X, stringsAsFactors = F, row.names = paste(Table_name, chr.col, rownames(Table), sep = "_")))
      
      non.ascii.detected.here <- showNonASCII(X[!is.na(X)])
      
      if(length(non.ascii.detected.here) > 0) non.ascii.detected <- rbind(non.ascii.detected, data.frame(non.ascii.detected = non.ascii.detected.here, row.names = paste(Table_name, chr.col, 1:(length(non.ascii.detected.here)), sep = "_"), stringsAsFactors = F))
      
      
      Table[, chr.col] <- X
      
    } # if(chr.col %in% names(Table))
    
    
  } # for(chr.col in chr.cols.to.check)
  
  assign(Table_name, Table)
  
} # for(Table_name in c("MEASUREMENTS", "SITES", "HISTORY", "METHODOLOGY", "ALLOMETRY", "CITATIONS"))

non.ascii.detected # this should be empty if we detected all the non ASCII characters

if(length(non.ascii.detected) > 0) stop("Not all non ascii characters were detected and translated to their closest equivalent. Look at non.ascii.detected and add the non ascii characters in scripts/QA_QC/special_characters_convert_table.txt along with their equivalents")

(double.chck.char.changes <- double.chck.char.changes[!double.chck.char.changes$with_special_character %in% double.chck.char.changes$fixed,])
View(double.chck.char.changes)


# save to fix special characters
# 
# write.csv(MEASUREMENTS, "data/ForC_measurements.csv", row.names = F,)
# write.csv(SITES, "data/ForC_sites.csv", row.names = F)
# write.csv(HISTORY, "data/ForC_history.csv", row.names = F)
# write.csv(METHODOLOGY, "data/ForC_methodology.csv", row.names = F)
# write.csv(ALLOMETRY, "data/ForC_allometry.csv", row.names = F)
# write.csv(CITATIONS, "data/ForC_citations.csv", row.names = F, fileEncoding =  "UTF-8")

# (A <- sort(unique(double.chck.char.changes$after)))
# i = 72
# r = 5
# grepl(sp_ch_conv_tbl$symbol[i], A[r])
# double.chck.char.changes[which( double.chck.char.changes$after %in% A[r]),]
