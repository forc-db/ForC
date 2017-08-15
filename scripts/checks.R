# Perform quality control/sanity checks on ForC data
# Following criteria laid out in https://github.com/forc-db/ForC/issues/23
# BBL August 2017

library(dplyr)  # version 0.5.0
library(readr)  # version 1.1.0

# ===== Read data ====

na_codes <- c("NA", "NAC", "NRA", "NI")

coltypes <- cols(
  .default = col_character(),
  measurementID = col_integer(),
  dateloc = col_double(),
  start_date = col_double(),
  start_dateloc = col_double(),
  end_date = col_double(),
  end_dateloc = col_double(),
  mean = col_double(),
  stat = col_double(),
  area_sampled = col_double(),
  min_dbh = col_double(),
  dupnum = col_integer(),
  citations.year = col_integer(),
  measurementID.v1 = col_integer(),
  TEMPORARY.FIELD..internal.purposes...match.in.2ForC..0.indicates.no.match. = col_integer()
)

measurements <- read_csv("data/ForC_measurements.csv", col_types = coltypes, na = na_codes)

plots <- read_csv("data/ForC_plots.csv")


# ===== Perform QC and sanity checks ====

# For each site-plot combination in MEASUREMENTS, there is a corresponding site-plot record in PLOTS
m_no_p <- anti_join(measurements, plots, by = c("sites.sitename", "plot.name"))
cat("There are", nrow(m_no_p), "measurements with no corresponding plot record\n")
if(nrow(m_no_p)) message("See `m_no_p`")

# No records in PLOTS that lack corresponding records in MEASUREMENTS
p_no_m <- anti_join(plots, measurements, by = c("sites.sitename", "plot.name"))
cat("There are", nrow(p_no_m), "plots with no corresponding measurement record\n")
if(nrow(p_no_m)) message("See `p_no_m`")

