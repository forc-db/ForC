# Perform quality control/sanity checks on ForC data
# Following criteria laid out in https://github.com/forc-db/ForC/issues/23
# BBL August 2017

# Set working directory as ForC main folder
setwd(".")

library(dplyr)  # version 0.5.0
library(readr)  # version 1.1.0

# ===== Read data ====

na_codes <- c("NA", "NAC", "NRA", "NI")   # various ways "NA" is encoded in ForC

MEASUREMENTS <- read_csv("data/ForC_measurements.csv", guess_max = 1e6)
PLOTS        <- read_csv("data/ForC_plots.csv")
SITES        <- read_csv("data/ForC_sites.csv", na = na_codes)
HISTORY      <- read_csv("data/ForC_history.csv")
PFT          <- read_csv("data/ForC_pft.csv")
HIST         <- read_csv("data/ForC_histtype.csv")
VARIABLES    <- read_csv("data/ForC_variables.csv", na = na_codes)
METHODOLOGY  <- read_csv("data/ForC_methodology.csv", col_types = "cccc")

# ===== MEASUREMENTS and PLOTS checks ====

# For each site-plot combination in MEASUREMENTS, there is a corresponding site-plot record in PLOTS
MEASUREMENTS %>%
  anti_join(PLOTS, by = c("sites.sitename", "plot.name")) %>%
  distinct(measurementID, sites.sitename, plot.name) ->
  m_no_p
cat("There are", nrow(m_no_p), "measurements with no corresponding plot record\n")
if(nrow(m_no_p)) message("See `m_no_p`")

# No records in PLOTS that lack corresponding records in MEASUREMENTS
PLOTS %>%
  anti_join(MEASUREMENTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plotID, sites.sitename, plot.name) ->
  p_no_m
cat("There are", nrow(p_no_m), "plots with no corresponding measurement record\n")
if(nrow(p_no_m)) message("See `p_no_m`")

# For each site in PLOTS, there should be a corresponding record in SITES
PLOTS %>%
  anti_join(SITES, by = c("sites.sitename")) %>% 
  distinct(plotID, sites.sitename) ->
  p_no_s
cat("There are", nrow(p_no_s), "plots with no corresponding site record\n")
if(nrow(p_no_s)) message("See `p_no_s`")

# ===== SITES checks ====

# There are no sites in SITES that lack records in PLOTS
SITES %>% 
  anti_join(PLOTS, by = c("sites.sitename")) %>% 
  distinct(siteID, sites.sitename) ->
  s_no_p
cat("There are", nrow(s_no_p), "sites with no corresponding plot record\n")
if(nrow(s_no_p)) message("See `s_no_p`")

# ===== HISTORY checks ====

# For each site-plot combination in PLOTS, there should be at least one corresponding record in HISTORY
PLOTS %>% 
  anti_join(HISTORY, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plotID, sites.sitename, plot.name) ->
  p_no_h
cat("There are", nrow(p_no_h), "plots with no corresponding history record\n")
if(nrow(p_no_h)) message("See `p_no_h`")

# There are no records in HISTORY that lack corresponding records in PLOTS
# (these can be identified based on whether the site-plot combination and the historyID 
# show up in PLOTS. see metadata to understand how historyID works in PLOTS)
HISTORY %>% 
  anti_join(PLOTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(historyID, sites.sitename, plot.name) ->
  h_no_p
cat("There are", nrow(h_no_p), "history records with no corresponding plot record\n")
if(nrow(h_no_p)) message("See `h_no_p`")

# ===== METHODOLOGY checks ====

# There should be no records in MEASUREMENTS that lack corresponding records in METHODOLOGY
MEASUREMENTS %>% 
  filter(!method_id %in% na_codes) %>% 
  anti_join(METHODOLOGY, by = c("method_id")) %>% 
  distinct(measurementID, method_id) ->
  m_no_m
cat("There are", nrow(m_no_m), "measurement records with no corresponding methodology record\n")
if(nrow(m_no_m)) message("See `m_no_m`")

# ===== PFT and DIST checks ====

# PFTs should only be defined once
if(any(duplicated(PFT$pftcode))) {
  message("There are duplicated PFT codes in the PFT table!")  
}

# All measurement PFTs should be defined
MEASUREMENTS %>%
  filter(!dominantveg %in% na_codes) %>% 
  filter(!is.na(dominantveg)) %>%
  anti_join(PFT, by = c("dominantveg" = "pftcode")) %>%
  distinct(measurementID, dominantveg) ->
  m_no_pft
cat("There are", nrow(m_no_pft), "measurement records with undefined PFTs\n")
if(nrow(m_no_pft)) message("See `m_no_pft`")

# Disturbance types should only be defined once
if(any(duplicated(paste(HIST$histcat, HIST$histtype)))) {
  message("There are duplicated history category/types in the HIST table!")  
}

# All disturbance categories and types in HISTORY should be defined

# We need to do some work to ensure that the `histcat` and `histtype` fields are correctly matched
# See https://github.com/forc-db/ForC/issues/23#issuecomment-324903771
pattern1 <- "[(]?_prior[)]?$"  # remove optional "_prior" and "(_prior)"
HISTORY$histcat2 <- gsub(pattern1, "", HISTORY$histcat)
HIST$histcat2 <- gsub(pattern1, "", HIST$histcat)
pattern2 <- "Fertilization_[A-Za-z]*$"  # remove _N, _Mg, etc.
HISTORY$histtype2 <- gsub(pattern2, "Fertilization", HISTORY$histtype)
HIST$histtype2 <- gsub(pattern2, "Fertilization", HIST$histtype)
HISTORY %>%
  anti_join(HIST, by = c("histcat2", "histtype2")) %>%
  distinct(historyID, histcat, histtype) ->
  h_no_dist
cat("There are", nrow(h_no_dist), "history records with undefined disturbance category/type combinations\n")
if(nrow(h_no_dist)) message("See `h_no_dist`")

# ===== MEASUREMENT mean check ====

# All records in MEASUREMENTS:mean should fall within the range reported for that variable in 
# the VARIABLES table. (It is possible that valid new records will fall outside the range, but
# script should generate a warning.)
MEASUREMENTS %>%
  filter(variables.name=="Deadwood") %>%
  select(measurementID, sites.sitename, variables.name, mean) %>%
  left_join(select(VARIABLES, variables.name, min, max), by = "variables.name") %>%
  mutate(out_of_bounds = mean < min | mean > max) ->
  meas_check

if(any(is.na(meas_check$out_of_bounds))) {
  message("No measurement range found for one or more measurements")
  message("See `meas_check`")
}
if(any(meas_check$out_of_bounds, na.rm = TRUE)) {
  message("Measurements fall outside the range of valies that currently exist for this variable")
  message("Check value, and if valid update allowed range in VARIABLES")
  message("See `filter(meas_check, out_of_bounds)`")
}

# Variables should only be defined once
if(any(duplicated(VARIABLES$variables.name))) {
  message("There are duplicated variable names in the VARIABLES table!")  
}

