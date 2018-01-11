######################################################
# Purpose: Automatically change variable names and places where they are refered to in data tables, metadata tables and scripts
# Inputs:  - all data tables, all metadata tables and all .R or .m files in all directories of scripts
#          - old names and new names as specified in "metadata/archive/variable_names_polishing.tx (to enter by hand, one by one, be careful at comments written in file)
# outputs: polished files
# NOTES: Same changes need to be made in diagram of entity relationship and paper
# Developped by: Valentine Herrmann ( HerrmannV@si.edu) in Januaray 2018
# R version 3.4.2
######################################################

rm(list = ls())

# what column names are we chaning and to what ?
old.variable.name <- "\\bNEE_annual\\b"
new.variable.name <- "NEE_C"



# Get tables path and name
tables_filenames  <- list.files("data", pattern = "\\.csv$", full.names = T)

# Get metadata path and name
metadata_filenames <- list.files("metadata", pattern = "\\.csv$", full.names = T)


# Get all scripts path and names

all.scripts <- c(list.files("scripts/Figures/", pattern = "\\.R$|\\.m$", full.names = T), list.files("scripts/Generate PLOTS from HISTORY/", pattern = "\\.R$|\\.m$", full.names = T), list.files("scripts/Group_sites_into_areas/", pattern = "\\.R$|\\.m$", full.names = T), list.files("scripts/QA_QC/", pattern = "\\.R$|\\.m$", full.names = T), list.files("scripts/z_archive/", pattern = "\\.R$|\\.m$", full.names = T))


# MAKE CHANGES AND SAVE 

  for( f in c(tables_filenames, metadata_filenames,all.scripts) ){
    
    x <- readLines(f)
    y <- gsub(old.variable.name, new.variable.name, x)
    cat(y, file=f, sep="\n")
    
  }

print(old.variable.name)
