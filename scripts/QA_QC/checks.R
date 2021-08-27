######################################################
# Purpose: Perform quality control/sanity checks on ForC data following criteria described in publication
# Inputs:  - ForC MEASUREMENTS table and metadata
#          - ForC PLOTS table and metadata
#          - ForC SITES table and metadata
#          - ForC HISTORY table and metadata
#          - ForC PFT table and metadata
#          - ForC HISTTYPE table and metadata
#          - ForC VARIABLES table and metadata
#          - ForC METHODOLOGY table and metadata
#          - ForC ALLOMETRY table and metadata
# outputs: flagged issues
# Developped by: BBL August 2017 and continued by Valentine Herrmann ( HerrmannV@si.edu) in December 2017
# R version 4.0.3 (2020-10-10)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libraries ####

library(dplyr)  # version 0.5.0
library(readr)  # version 1.1.0


# ===== Read data ====

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999")  # various ways "NA" is encoded in ForC


MEASUREMENTS <- read_csv("data/ForC_measurements.csv", guess_max = 1e6)
PLOTS        <- read_csv("data/ForC_plots.csv")
SITES        <- read_csv("data/ForC_sites.csv", na = na_codes)
HISTORY      <- read_csv("data/ForC_history.csv")
PFT          <- read_csv("data/ForC_pft.csv", na = na_codes)
HISTTYPE     <- read_csv("data/ForC_histtype.csv")
VARIABLES    <- read_csv("data/ForC_variables.csv", na = na_codes)
METHODOLOGY  <- read_csv("data/ForC_methodology.csv", col_types = "ccccc", na = na_codes)
ALLOMETRY    <- read_csv("data/ForC_allometry.csv", na = na_codes)
CITATIONS    <- read_csv("data/ForC_citations.csv", na = na_codes)


MEASUREMENTS_meta  <- read.csv("metadata/measurements_metadata.csv", stringsAsFactors = F)
PLOTS_meta         <- read.csv("metadata/plots_metadata.csv", stringsAsFactors = F)
SITES_meta         <- read.csv("metadata/sites_metadata.csv", stringsAsFactors = F)
HISTORY_meta       <- read.csv("metadata/history_metadata.csv", stringsAsFactors = F)
PFT_meta           <- read.csv("metadata/pft_metadata.csv", stringsAsFactors = F)
HISTTYPE_meta      <- read.csv("metadata/histtype_metadata.csv", stringsAsFactors = F)
VARIABLES_meta     <- read.csv("metadata/variables_metadata.csv", stringsAsFactors = F)
METHODOLOGY_meta   <- read.csv("metadata/methodology_metadata.csv", stringsAsFactors = F)
ALLOMETRY_meta     <- read.csv("metadata/allometry_metadata.csv", stringsAsFactors = F)


# ===== prepare messages ==== ####
warn <- NULL
err <- NULL

# ===== MEASUREMENTS checks ==== ####

# unique measurement ID:

if(sum(duplicated(MEASUREMENTS$measurement.ID)) > 0) err <- c(err, paste("there are", sum(duplicated(MEASUREMENTS$measurement.ID)), "measurement.ID that are repeated"))
cat(paste("there are", sum(duplicated(MEASUREMENTS$measurement.ID)), "measurement.ID that are repeated"))
# View(MEASUREMENTS[MEASUREMENTS$measurement.ID %in% MEASUREMENTS$measurement.ID[duplicated(MEASUREMENTS$measurement.ID)],])


# For each site-plot combination in MEASUREMENTS, there is a corresponding site-plot record in HISTORY
MEASUREMENTS %>%
  anti_join(HISTORY, by = c("sites.sitename", "plot.name")) %>%
  distinct(measurement.ID, sites.sitename, plot.name) ->
  m_no_h

look_name <- "m_no_h"
look <- get(look_name)
say <- paste("There are", nrow(look), "measurements with no corresponding history record\n")
cat(say)
filename <- "QA_QC/error_reports/meas_with_no_history_record.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# For each site in MEASUREMENTS, there is a corresponding site record in SITES
MEASUREMENTS %>%
  anti_join(SITES, by = c("sites.sitename")) %>%
  distinct(measurement.ID, sites.sitename) ->
  m_no_s


look_name <- "m_no_s"
look <- get(look_name)
say <- paste("There are", nrow(look), "measurements with no corresponding SITES record\n")
cat(say)
filename <- "QA_QC/error_reports/meas_with_no_SITES_record.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# All measurement PFTs should be defined
MEASUREMENTS %>%
  filter(!dominant.veg %in% na_codes) %>% 
  filter(!is.na(dominant.veg)) %>%
  anti_join(PFT, by = c("dominant.veg" = "pftcode")) %>%
  distinct(measurement.ID, dominant.veg) ->
  m_no_pft



look_name <- "m_no_pft"
look <- get(look_name)
say <- paste("There are", nrow(look), "measurements  with undefined PFTs\n")
cat(say)
filename <- "QA_QC/error_reports/meas_with_undefined_PFT.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# All variables in MEASUREMENTS should be ID-ed in VARIABLES

m_no_v <- unique(MEASUREMENTS[,"variable.name"][!MEASUREMENTS$variable.name%in% VARIABLES$variable.name,])



look_name <- "m_no_v"
look <- get(look_name)
say <- paste("There are", nrow(look), "variable.name in MEASUREMENT that are not defined in VARIABLES\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_variables.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}






# For each covariate_# in MEASUREMENTS, there is a definition in VARIABLES

m_no_c1 <- unique(MEASUREMENTS[,"covariate_1"][!MEASUREMENTS$covariate_1 %in% VARIABLES$variable.name & !MEASUREMENTS$covariate_1 %in% c("NAC", "NI", "NRA") & !is.na(MEASUREMENTS$covariate_1),])

look_name <- "m_no_c1"
look <- get(look_name)
say <- paste("There are", nrow(look), "covariate_1 in measurements that are not defined in VARIABLES\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_covariate_1.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}

m_no_c2 <- unique(MEASUREMENTS[,"covariate_2"][!MEASUREMENTS$covariate_1 %in% VARIABLES$variable.name & !MEASUREMENTS$covariate_2 %in% c("NAC", "NI", "NRA") & !is.na(MEASUREMENTS$covariate_2),])

look_name <- "m_no_c1"
look <- get(look_name)
say <- paste("There are", nrow(look), "covariate_2 in measurements that are not defined in VARIABLES\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_covariate_2.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}





# For each allometry_1 and allometry_2 in MEASUREMENTS, there is an allometric equation in ALLOMETRIE

m_no_a1 <- unique(MEASUREMENTS[,"allometry_1"][!MEASUREMENTS$allometry_1 %in% VARIABLES$variable.name & !MEASUREMENTS$allometry_1 %in% c("NAC", "NI", "NRA") & !is.na(MEASUREMENTS$allometry_1),])

look_name <- "m_no_a1"
look <- get(look_name)
say <- paste("There are", nrow(look), "allometry_1 in measurements that are not defined in VARIABLES\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_allometry_1.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}

m_no_a2 <- unique(MEASUREMENTS[,"allometry_2"][!MEASUREMENTS$allometry_2 %in% VARIABLES$variable.name & !MEASUREMENTS$allometry_2 %in% c("NAC", "NI", "NRA") & !is.na(MEASUREMENTS$allometry_2),])

look_name <- "m_no_a2"
look <- get(look_name)
say <- paste("There are", nrow(look), "allometry_2 in measurements that are not defined in VARIABLES\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_allometry_2.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# For each citation_Id and allometry_2 in MEASUREMENTS, there is citation in CITATIONS
if(!all(unique(na.omit(MEASUREMENTS$citation.ID)) [!unique(na.omit(MEASUREMENTS$citation.ID)) %in% CITATIONS$citation.ID] %in% na_codes)) warning("There are citation.ID in measurements that are not defined in CITATIONS") #unique(na.omit(MEASUREMENTS$citation.ID)) [!unique(na.omit(MEASUREMENTS$citation.ID)) %in% CITATIONS$citation.ID]


m_no_citation <- unique(MEASUREMENTS[,"citation.ID"][!MEASUREMENTS$citation.ID %in% CITATIONS$citation.ID & !MEASUREMENTS$citation.ID %in% c("NAC", "NI", "NRA") & !is.na(MEASUREMENTS$citation.ID),])

look_name <- "m_no_citation"
look <- get(look_name)
say <- paste("There are", nrow(look), "citation.ID in measurements that are not defined in CITATIONS\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_citation.ID.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# There should be no records in MEASUREMENTS that lack corresponding records in METHODOLOGY
MEASUREMENTS %>% 
  filter(!method.ID %in% na_codes & !is.na(method.ID)) %>% 
  anti_join(METHODOLOGY, by = c("method.ID")) %>% 
  distinct(measurement.ID, method.ID) ->
  m_no_m
cat("There are", nrow(m_no_m), "measurement records with no corresponding methodology record\n")
if(nrow(m_no_m)) message("See `m_no_m`")


look_name <- "m_no_m"
look <- get(look_name)
say <- paste("There are", nrow(m_no_m), "measurement records with no corresponding methodology record\n")
cat(say)
filename <- "QA_QC/error_reports/undefined_methodology.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# ===== SITES checks ==== ####

# There are no sites in SITES that lack records in PLOTS
SITES %>% 
  anti_join(PLOTS, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_p
cat("There are", nrow(s_no_p), "sites with no corresponding plot record\n")
if(nrow(s_no_p)) message("See `s_no_p`")


look_name <- "s_no_p"
look <- get(look_name)
say <- paste("There are", nrow(look), "sites with no corresponding plot record\n")
cat(say)
filename <- "QA_QC/error_reports/sites_with_no_plots.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# There are no sites in SITES that lack records in History
SITES %>% 
  anti_join(HISTORY, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_h
cat("There are", nrow(s_no_h), "sites with no corresponding history record\n")
if(nrow(s_no_h)) message("See `s_no_h`")


look_name <- "s_no_h"
look <- get(look_name)
say <- paste("There are", nrow(look), "sites with no corresponding history record\n")
filename <- "QA_QC/error_reports/sites_with_no_history.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# There are no sites in SITES that lack records in MEASUREMENTS
SITES %>% 
  anti_join(MEASUREMENTS, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_m
cat("There are", nrow(s_no_m), "sites with no corresponding measurements record\n")
if(nrow(s_no_m)) message("See `s_no_m`")


look_name <- "s_no_m"
look <- get(look_name)
say <- paste("There are", nrow(look), "sites with no corresponding measurements record\n")

filename <- "QA_QC/error_reports/sites_with_no_measurements.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# Sites should only be defined once
if(any(duplicated(SITES$sites.sitename))) {
  message("There are duplicated sites.sitename in the SITES table!")  
}

dup_sites <- SITES[SITES$sites.sitename %in% SITES$sites.sitename[duplicated(SITES$sites.sitename)],]

look_name <- "dup_sites"
look <- get(look_name)
say <- paste("There are duplicated sites.sitename in the SITES table\n")

filename <- "QA_QC/error_reports/duplicated_sites.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# View(SITES[SITES$sites.sitename %in% SITES$sites.sitename[duplicated(SITES$sites.sitename)], ])

if(any(duplicated(SITES$site.ID))) {
  message("There are duplicated site.ID names in the SITES table!")  
}

dup_sitesID <- SITES[SITES$site.ID %in% SITES$site.ID[duplicated(SITES$site.ID)],]

look_name <- "dup_sites"
look <- get(look_name)
say <- paste("There are duplicated site.ID in the SITES table\n")

filename <- "QA_QC/error_reports/duplicated_siteIDs.csv"
if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# ===== HISTORY checks ==== ####

# There are no sites in History that lack records in MEASUREMENTS
HISTORY %>% 
  anti_join(MEASUREMENTS, by = c("sites.sitename")) %>% 
  distinct(history.ID, sites.sitename) ->
  h_no_m

look_name <- "h_no_m"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with no corresponding measurements record\n")
filename <- "QA_QC/error_reports/History_with_no_meas.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# There are no sites in History that lack records in SITES
HISTORY %>% 
  anti_join(SITES, by = c("sites.sitename")) %>% 
  distinct(history.ID, sites.sitename) ->
  h_no_s
cat("There are", nrow(h_no_s), "history records with no corresponding sites record\n")
if(nrow(h_no_s)) message("See `h_no_s`")




look_name <- "h_no_s"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with no corresponding sites record\n")
filename <- "QA_QC/error_reports/History_with_no_sites.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# All disturbance categories and types in HISTORY should be defined

# We need to do some work to ensure that the `hist.cat` and `hist.type` fields are correctly matched
# See https://github.com/forc-db/ForC/issues/23#issuecomment-324903771
pattern1 <- "[(]?_prior[)]?$"  # remove optional "_prior" and "(_prior)"
HISTORY$hist.cat2 <- gsub(pattern1, "", HISTORY$hist.cat)
HISTTYPE$hist.cat2 <- gsub(pattern1, "", HISTTYPE$hist.cat)
pattern2 <- "Fertilization_[A-Za-z]*$"  # remove _N, _Mg, etc.
HISTORY$hist.type2 <- gsub(pattern2, "Fertilization", HISTORY$hist.type)
HISTTYPE$hist.type2 <- gsub(pattern2, "Fertilization", HISTTYPE$hist.type)
HISTORY %>%
  anti_join(HISTTYPE, by = c("hist.cat2", "hist.type2")) %>%
  distinct(history.ID, hist.cat, hist.type) %>% filter(hist.type != "NAC") ->
  h_no_dist
cat("There are", nrow(h_no_dist), "history records with undefined disturbance category/type combinations\n")
if(nrow(h_no_dist)) message("See `h_no_dist`") # it is okay if all "NAC"




look_name <- "h_no_dist"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with undefined disturbance category/type combinations\n")
filename <- "QA_QC/error_reports/undefined_hist_cat_hist_type_combinations.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# There are no records in HISTORY that lack corresponding records in PLOTS
# (these can be identified based on whether the site-plot combination and the history.ID 
# show up in PLOTS. see metadata to understand how history.ID works in PLOTS)
HISTORY %>% 
  anti_join(PLOTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(history.ID, sites.sitename, plot.name) ->
  h_no_p
cat("There are", nrow(h_no_p), "history records with no corresponding plot record\n")
if(nrow(h_no_p)) message("See `h_no_p`")

look_name <- "h_no_p"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with  no corresponding plot record\n")
filename <- "QA_QC/error_reports/History_with_no_plot_record.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# ===== PLOTS checks ==== ####

# No records in PLOTS that lack corresponding records in MEASUREMENTS
PLOTS %>%
  anti_join(MEASUREMENTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plot.ID, sites.sitename, plot.name) ->
  p_no_m
cat("There are", nrow(p_no_m), "plots with no corresponding measurement record\n")
if(nrow(p_no_m)) message("See `p_no_m`")




look_name <- "p_no_m"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with  no corresponding measurement record\n")
filename <- "QA_QC/error_reports/History_with_no_measurement_record.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}

# For each site-plot combination in PLOTS, there should be at least one corresponding record in HISTORY
PLOTS %>% 
  anti_join(HISTORY, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plot.ID, sites.sitename, plot.name) ->
  p_no_h
cat("There are", nrow(p_no_h), "plots with no corresponding history record\n")
if(nrow(p_no_h)) message("See `p_no_h`")



look_name <- "p_no_h"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with  no corresponding history record\n")
filename <- "QA_QC/error_reports/History_with_no_history_record.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# For each site in PLOTS, there should be a corresponding record in SITES
PLOTS %>%
  anti_join(SITES, by = c("sites.sitename")) %>% 
  distinct(plot.ID, sites.sitename) ->
  p_no_s
cat("There are", nrow(p_no_s), "plots with no corresponding site record\n")
if(nrow(p_no_s)) message("See `p_no_s`")



look_name <- "p_no_s"
look <- get(look_name)
say <- paste("There are", nrow(look), "history records with  no corresponding site record\n")
filename <- "QA_QC/error_reports/History_with_no_site_record.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# ===== HISTTYPE checks ==== ####

# Disturbance types should only be defined once
if(any(duplicated(paste(HISTTYPE$hist.cat, HISTTYPE$hist.type)))) {
  message("There are duplicated history category/types in the HISTTYPE table!")  
}

dup_histtype <- HISTTYPE[paste(HISTTYPE$hist.cat, HISTTYPE$hist.type) %in% paste(HISTTYPE$hist.cat, HISTTYPE$hist.type)[duplicated(paste(HISTTYPE$hist.cat, HISTTYPE$hist.type))],]

look_name <- "dup_histtype"
look <- get(look_name)
say <- paste("There are duplicated history category/types in the HISTTYPE table\n")
filename <- "QA_QC/error_reports/duplicated_history_category_types.csv"

if(nrow(look) > 0) {
  err <- c(err, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# All hist.cat exist in HISTORY
if(!all(gsub("\\(_prior\\)", "", HISTTYPE$hist.cat) %in% HISTORY$hist.cat)) warning("There are hist.cat in HISTTYPE that don't exist in HISTORY")


unused_hist.cat <- HISTTYPE[!gsub("\\(_prior\\)", "", HISTTYPE$hist.cat) %in% HISTORY$hist.cat, ]

look_name <- "unused_hist.cat"
look <- get(look_name)
say <- paste("There are", nrow(look), "hist.cat in HISTTYPE that don't exist in HISTORY\n")
filename <- "QA_QC/warning_reports/unused_hist.cat.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# All hist.type exist in HISTORY

if(!all(HISTTYPE$hist.type2 %in% HISTORY$hist.type))  warning("There are hist.type in HISTTYPE that don't exist in HISTORY table. See HISTTYPE$hist.type2[!HISTTYPE$hist.type2 %in% HISTORY$hist.type]
")
HISTTYPE$hist.type2[!HISTTYPE$hist.type2 %in% HISTORY$hist.type] # Leave Precipitation Diversion


unused_hist.type2 <- HISTTYPE[!HISTTYPE$hist.type2 %in% HISTORY$hist.type, ]

look_name <- "unused_hist.type2"
look <- get(look_name)
say <- paste("There are", nrow(look), "hist.type in HISTTYPE that don't exist in HISTORY table\n")
filename <- "QA_QC/warning_reports/unused_hist.type2.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# All hist.type exist in PLOTS
if(!all(HISTTYPE$hist.type %in% unique(c(PLOTS$regrowth.type, PLOTS$distmrs.type, PLOTS$dist1.type, PLOTS$dist2.type)))) warning("There are hist.cat in HISTTYPE that don't exist in PLOTS table. SEE")


unused_hist.cat <- HISTTYPE[!HISTTYPE$hist.type %in% unique(c(PLOTS$regrowth.type, PLOTS$distmrs.type, PLOTS$dist1.type, PLOTS$dist2.type)), ] # keep for furture records


look_name <- "unused_hist.cat"
look <- get(look_name)
say <- paste("There are", nrow(look), "hist.cat in HISTTYPE that don't exist in PLOTS table\n")
filename <- "QA_QC/warning_reports/unused_hist.cat.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



# ===== PFT checks ==== ####
# PFTs should only be defined once
if(any(duplicated(PFT$pftcode))) {
  message("There are duplicated PFT codes in the PFT table!")  
}


dup_PFT <- PFT[PFT$pftcode %in% PFT$pftcode[duplicated(PFT$pftcode)], ] # keep for furture records


look_name <- "dup_PFT"
look <- get(look_name)
say <- paste("There are duplicated PFT codes in the PFT table\n")
filename <- "QA_QC/warning_reports/dup_PFT.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# All pftcode are present in MEASUREMENTS table
if(!all(PFT$pftcode %in% MEASUREMENTS$dominant.veg)) warning("There are pftcode in PFT that don't exist in MEASUREMENTS table. See PFT$pftcode[!PFT$pftcode %in% MEASUREMENTS$dominant.veg]
")
PFT$pftcode[!PFT$pftcode %in% MEASUREMENTS$dominant.veg] # Leave "2VW"   "2GW"   "2FORB" "2LTR"  "2RF"   "2RB"



unused_PFT <- PFT[!PFT$pftcode %in% MEASUREMENTS$dominant.veg, ] # Leave "2VW"   "2GW"   "2FORB" "2LTR"  "2RF"   "2RB"



look_name <- "unused_PFT"
look <- get(look_name)
say <- paste("There are", nrow(look), "pftcode in PFT that don't exist in MEASUREMENTS table\n")
filename <- "QA_QC/warning_reports/unused_PFT.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# ===== METHODOLOGY checks ==== ####


# There should be no records in METHODOLOGY that lack corresponding records in MEASUREMENTS
if(!all(METHODOLOGY$method.ID %in% MEASUREMENTS$method.ID)) warning("There are method.ID in METHODOLOGY that don't exist in MEASUREMENTS table. See METHODOLOGY$method.ID[!METHODOLOGY$method.ID %in% MEASUREMENTS$method.ID]")


unused_method <- METHODOLOGY[!METHODOLOGY$method.ID %in% MEASUREMENTS$method.ID, ]

look_name <- "unused_method"
look <- get(look_name)
say <- paste("There are", nrow(look), "method.ID in METHODOLOGY that don't exist in MEASUREMENTS table\n")
filename <- "QA_QC/warning_reports/unused_method.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# All variables in METHODOLOGY exist in VARIABLES --> not checking as names may change


# ===== VARIABLES checks ==== ####

# All variables in VARIABLES should exist in MEASUREMENTS in either variable.name or covariate_#
if(any(!VARIABLES$variable.name %in% c(MEASUREMENTS$variable.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2))) {
  message("There variables not used in MEASUREMENTS table")
  message("Check 
          unique(VARIABLES$variable.name)[!unique(VARIABLES$variable.name)%in% c(MEASUREMENTS$variable.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2)]")
}

unique(VARIABLES$variable.name)[!unique(VARIABLES$variable.name)%in% c(MEASUREMENTS$variable.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2)] # Leave "NPP_4", "NPP_5", "NPP_understory", "NPP_woody", "ANPP_litterfall_2_C", "ANPP_litterfall_3_C"

unused_variable <- unique(VARIABLES[!VARIABLES$variable.name%in% c(MEASUREMENTS$variable.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2),])

look_name <- "unused_variable"
look <- get(look_name)
say <- paste("here variables not used in MEASUREMENTS table\n")
filename <- "QA_QC/warning_reports/unused_variable.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# All variables in VARIABLES should exist in METHODOLOGY --> not checking as names may change


# ===== ALLOMETRIES checks ==== ####

# All allometric.equation in ALLOMETRY exist in MEASUREMENTS allometry_1 and allometry_2
if(any(!ALLOMETRY$allometric.equation %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2))) {
  message("There allometric.equation not used in MEASUREMENTS table.")
  message("Check 
          ALLOMETRY$allometric.equation[!ALLOMETRY$allometric.equation %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2)]")
}

unique(ALLOMETRY$allometric.equation)[!unique(ALLOMETRY$allometric.equation) %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2)] # Keep all


sort(unique(c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2)))


unused_allo <- ALLOMETRY[!ALLOMETRY$allometric.equation %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2), ] # Keep all


look_name <- "unused_allo"
look <- get(look_name)
say <- paste("There allometric.equation not used in MEASUREMENTS table\n")
filename <- "QA_QC/warning_reports/unused_allometry.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}





# ===== MEASUREMENT MEAN VALUE check against range in VARIABLES ==== ####

# All records in MEASUREMENTS:mean should fall within the range reported for that variable in the VARIABLES table. (It is possible that valid new records will fall outside the range, but script should generate a warning.)

mean.not.within.range <- data.frame()
covariate_1.not.within.range <- data.frame()
covariate_2.not.within.range <- data.frame()
Value.for.variables.without.range <-  data.frame()

for(i in 1:nrow(VARIABLES)){
  
  v <- VARIABLES$variable.name[i]
  print(v)
  
  min.v <- as.numeric(VARIABLES$min[i])
  max.v <- as.numeric(VARIABLES$max[i])
  n.records.v <- as.numeric(VARIABLES$n.records[i])
  
  
  if(n.records.v == 0 & nrow(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]) > 0){
    Value.for.variables.without.range <- rbind(Value.for.variables.without.range, data.frame(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, c("measurement.ID", "sites.sitename", "variable.name", "mean")], min_range = min.v, max_range = max.v))
  }
  
  
  
  if(all(!is.na(c(min.v, max.v)))){
    
    if(!VARIABLES$variable.type[i] %in% "covariates"){
      
      x <- MEASUREMENTS[MEASUREMENTS$variable.name %in% v, c("measurement.ID", "mean")]
      x$mean <- as.numeric(ifelse(x$mean %in% na_codes, NA, x$mean))
      
      value.too.small <- any(na.omit(x$mean) < min.v)
      value.too.big <- any(na.omit(x$mean) > max.v)
      
      flagged.ID <- na.omit(c(x$measurement.ID[x$mean < min.v], x$measurement.ID[x$mean > max.v]))
      
      if(any(value.too.small, value.too.big)) mean.not.within.range <- rbind(mean.not.within.range, data.frame(MEASUREMENTS[MEASUREMENTS$variable.name %in% v & MEASUREMENTS$measurement.ID %in% flagged.ID, c("measurement.ID", "sites.sitename", "variable.name", "mean")], min_range = min.v, max_range = max.v))
      
      
    }
    
    if(VARIABLES$variable.type[i] %in% "covariates"){
      
      x_1 <- MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v, ]$coV_1.value
      x_1 <- as.numeric(ifelse(x_1 %in% na_codes, NA, x_1))
      
      x_2 <- MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v, ]$coV_2.value
      x_2 <- as.numeric(ifelse(x_2 %in% na_codes, NA, x_2))
      
      value_1.too.small <- any(na.omit(x_1) < min.v)
      value_1.too.big <- any(na.omit(x_1) > max.v)
      
      value_2.too.small <- any(na.omit(x_2) < min.v)
      value_2.too.big <- any(na.omit(x_2) > max.v)
      
      flagged.value <- na.omit(c(x_1[x_1 < min.v], x_1[x_1 > max.v], x_2[x_2 < min.v], x_2[x_2 > max.v]))
      
      if(any(value_1.too.small, value_1.too.big)) covariate_1.not.within.range <- rbind(covariate_1.not.within.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v & MEASUREMENTS$coV_1.value %in% flagged.value, c("measurement.ID", "sites.sitename", "covariate_1", "coV_1.value")]))
      
      if(any(value_2.too.small, value_2.too.big))  covariate_2.not.within.range <- rbind(covariate_2.not.within.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v & MEASUREMENTS$coV_2.value %in% flagged.value, c("measurement.ID", "sites.sitename", "covariate_2", "coV_2.value")]))
      
    }
  }
}


warning(paste("There is", nrow(mean.not.within.range), "measurements falling out of variable range"))
if(nrow(mean.not.within.range) > 0) cat("See mean.not.within.range")


look_name <- "mean.not.within.range"
look <- get(look_name)
say <- paste("There is", nrow(look), "measurements falling out of variable range")
filename <- "QA_QC/warning_reports/mean.not.within.range.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}




# View(mean.not.within.range)
# View(MEASUREMENTS[MEASUREMENTS$measurement.ID %in% mean.not.within.range$measurement.ID, ])

look_name <- "covariate_1.not.within.range"
look <- get(look_name)
say <- paste("There is", nrow(look), "covariate_1 value(s) falling out of variable range")
filename <- "QA_QC/warning_reports/covariate_1.not.within.range.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}



look_name <- "covariate_2.not.within.range"
look <- get(look_name)
say <- paste("There is", nrow(look), "covariate_2 value(s) falling out of variable range")
filename <- "QA_QC/warning_reports/covariate_2.not.within.range.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


look_name <- "Value.for.variables.without.range"
look <- get(look_name)
say <- paste("There is", nrow(look), "value(s) from a variable that has no defined range (this is probably the first record for that variable)")
filename <- "QA_QC/warning_reports/Value.for.variables.without.range.csv"

if(nrow(look) > 0) {
  warn <- c(warn, say)
  cat(say)
  message("See ", look_name)
  write.csv(look, file = filename, row.names = F)
} else {
  if(file.exists(filename)) file.remove(filename)
}


# plot to trouble shoot ####
require("R2HTML")

target <- HTMLInitFile(outdir = paste0(getwd(), "/scripts/QA_QC"), filename = "checking_range_of_records")
HTML("<h1>These are plots to review wether records outside of range are plausible or if they are mistakes.</h1>",file=target)

for( v in sort(unique(mean.not.within.range$variable.name))) {
  old_minimum <- as.numeric(VARIABLES[VARIABLES$variable.name %in% v,]$min)
  old_maximum <- as.numeric(VARIABLES[VARIABLES$variable.name %in% v,]$max)
  curr_minimum <- min(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$mean, na.rm = T)
  curr_maximum <- max(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$mean, na.rm = T)
  
  plot(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$mean, ylim = c(min(old_minimum, curr_minimum), max(old_maximum, curr_maximum)), main = v, col = ifelse(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$measurement.ID %in% mean.not.within.range[mean.not.within.range$variable.name %in% v, ]$measurement.ID, "red", "black"), pch = 16, ylab = v)
  abline(h =c(old_minimum, old_maximum))
  text(x = 1:nrow(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]), y = MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$mean, labels = ifelse(MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$measurement.ID %in% mean.not.within.range[mean.not.within.range$variable.name %in% v, ]$measurement.ID, MEASUREMENTS[MEASUREMENTS$variable.name %in% v, ]$measurement.ID, NA), pos = 2)
  
  HTMLplot(file=target, append = T, GraphDirectory = "scripts/QA_QC")
  
  Sys.sleep(1)
}


# file.remove(list.files("scripts/QA_QC", full.names = T)[grepl("GRAPH",list.files("scripts/QA_QC"), ignore.case = F)])

# ===== All tables numerical variables, check against range in corresponding matadata table ==== ####

# All records in all tables should fall within the range reported in the corresponding metadata table or be missing

variable.not.within.range <- NULL

for(Table in c("MEASUREMENTS", "PLOTS", "SITES", "HISTORY", "PFT", "HISTTYPE", "VARIABLES", "METHODOLOGY","ALLOMETRY")){
  
  DF <- as.data.frame(get(Table))
  DF_meta <- as.data.frame(get(paste0(Table, "_meta")))
  
  
  for(i in 1:nrow(DF_meta)){
    
    f <- DF_meta$Field[i]
    print(f)
    
    DF_meta.min <- as.numeric(DF_meta$min[i])
    DF_meta.max <- as.numeric(DF_meta$max[i])
    
    
    if(all(!is.na(c(DF_meta.min, DF_meta.max)))) {
      
      x <- DF[, f]
      x <- round(na.omit(as.numeric(ifelse(x %in% na_codes, NA, x))))
      
      value.too.small <- any(x < DF_meta.min)
      value.too.big <- any(x > DF_meta.max)
      
      flagged.value <- c(x[x < DF_meta.min], x[x > DF_meta.max])
      
      if(any(value.too.small, value.too.big)) variable.not.within.range[[Table]][[f]] <- rbind(variable.not.within.range[[Table]][[f]],  as.data.frame(DF[DF[, f] %in% flagged.value, c(1,2,3, which(names(DF) %in% f))]))
    }
  }
}


cat(paste("There is", length(variable.not.within.range), "table(s) with at least one variable value that is out of variable range."))
if(length(variable.not.within.range) > 0) cat("See variable.not.within.range")



## make a png with errors and warnings ####

### errors

filename <- file.path("QA_QC/error_reports/errors.png")
if(length(err) > 0){
  err_message <- paste("ERRORS TO FIX:\n",  paste(err, collapse = "\n"), "\n\n[CLICK HERE TO SEE]")
  
  
  png(filename, width = 6, height = 0.5 + (0.3*length(err)), units = "in", res = 300)
  par(mar = c(0,0,0,0))
  plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
  text(0,0.1, err_message, col = "red", cex = 0.6, adj = c(.5,0.5))
  dev.off()
  
  } else {
    if(exists(filename))  file.remove(filename)
  }



### warnings
if(length(warn) > 0){
  
  filename <- file.path("QA_QC/warning_reports/all_warnings.png")
  filename1 <- file.path("QA_QC/warning_reports/warnings.png")
  
  
  warn_message <- paste("WARNINGS:\n\n",  paste(warn, collapse = "\n"))
  warn_message1 <- paste("There are WARNINGS\n\n[CLICK HERE TO SEE]")
  
  
  png(filename, width = 6, height = 0.5 + (0.3*length(warn)), units = "in", res = 300)
  par(mar = c(0,0,0,0))
  plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
  text(0,0.1, warn_message, col = "red", cex = 0.6, adj = c(.5,0.5))
  dev.off()
  
  png(filename1, width = 6, height = 0.5, units = "in", res = 300)
  par(mar = c(0,0,0,0))
  plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
  text(0,0.1, warn_message1, col = "red", cex = 0.6, adj = c(.5,0.5))
  dev.off()
  
} else {
  if(exists(filename))  file.remove(filename)
}


