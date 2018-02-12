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
# R version 3.4.2
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
METHODOLOGY  <- read_csv("data/ForC_methodology.csv", col_types = "cccc", na = na_codes)
ALLOMETRY    <- read_csv("data/ForC_allometry.csv", na = na_codes)


MEASUREMENTS_meta  <- read.csv("metadata/measurements_metadata.csv", stringsAsFactors = F)
PLOTS_meta         <- read.csv("metadata/plots_metadata.csv", stringsAsFactors = F)
SITES_meta         <- read.csv("metadata/sites_metadata.csv", stringsAsFactors = F)
HISTORY_meta       <- read.csv("metadata/history_metadata.csv", stringsAsFactors = F)
PFT_meta           <- read.csv("metadata/pft_metadata.csv", stringsAsFactors = F)
HISTTYPE_meta      <- read.csv("metadata/histtype_metadata.csv", stringsAsFactors = F)
VARIABLES_meta     <- read.csv("metadata/variables_metadata.csv", stringsAsFactors = F)
METHODOLOGY_meta   <- read.csv("metadata/methodology_metadata.csv", stringsAsFactors = F)
ALLOMETRY_meta     <- read.csv("metadata/allometry_metadata.csv", stringsAsFactors = F)


# ===== MEASUREMENTS checks ==== ####

# For each site-plot combination in MEASUREMENTS, there is a corresponding site-plot record in PLOTS
MEASUREMENTS %>%
  anti_join(PLOTS, by = c("sites.sitename", "plot.name")) %>%
  distinct(measurement.ID, sites.sitename, plot.name) ->
  m_no_p
cat("There are", nrow(m_no_p), "measurements with no corresponding plot record\n")
if(nrow(m_no_p)) message("See `m_no_p`")


# For each site in MEASUREMENTS, there is a corresponding site record in SITES
MEASUREMENTS %>%
  anti_join(SITES, by = c("sites.sitename")) %>%
  distinct(measurement.ID, sites.sitename) ->
  m_no_s
cat("There are", nrow(m_no_s), "measurements with no corresponding site in SITES record\n")
if(nrow(m_no_s)) message("See `m_no_s`")


# All measurement PFTs should be defined
MEASUREMENTS %>%
  filter(!dominant.veg %in% na_codes) %>% 
  filter(!is.na(dominant.veg)) %>%
  anti_join(PFT, by = c("dominant.veg" = "pftcode")) %>%
  distinct(measurement.ID, dominant.veg) ->
  m_no_pft
cat("There are", nrow(m_no_pft), "measurement records with undefined PFTs\n")
if(nrow(m_no_pft)) message("See `m_no_pft`")

# All variables in MEASUREMENTS should be ID-ed in VARIABLES
if(any(!unique(MEASUREMENTS$variables.name) %in% VARIABLES$variable.name)) {
  message("There variables not defined")
  message("Check unique(MEASUREMENTS$variables.name)[!unique(MEASUREMENTS$variables.name)%in% VARIABLES$variable.name]")
}




# For each covariate_# in MEASUREMENTS, there is a definition in VARIABLES
if(!all(na.omit(MEASUREMENTS$covariate_1)[!na.omit(MEASUREMENTS$covariate_1) %in% VARIABLES$variable.name] %in% na_codes)) stop("There are covariate_1 in measurements that are not defined in VARIABLES")

if(!all(na.omit(MEASUREMENTS$covariate_2)[!na.omit(MEASUREMENTS$covariate_2) %in% VARIABLES$variable.name] %in% na_codes))stop("There are covariate_2 in measurements that are not defined in VARIABLES")


# For each allometry_1 and allometry_2 in MEASUREMENTS, there is an allometric equation in ALLOMETRIE
if(!all(unique(na.omit(MEASUREMENTS$allometry_1)) [!unique(na.omit(MEASUREMENTS$allometry_1)) %in% ALLOMETRY$allometric.equation] %in% na_codes)) stop("There are coV_1.value in measurements that are not defined in ALLOMETRY")

if(!all(na.omit(MEASUREMENTS$allometry_2) [!na.omit(MEASUREMENTS$allometry_2) %in% ALLOMETRY$allometric.equation] %in% na_codes)) stop("There are coV_2.value in measurements that are not defined in ALLOMETRY")


# There should be no records in MEASUREMENTS that lack corresponding records in METHODOLOGY
MEASUREMENTS %>% 
  filter(!method.ID %in% na_codes) %>% 
  anti_join(METHODOLOGY, by = c("method.ID")) %>% 
  distinct(measurement.ID, method.ID) ->
  m_no_m
cat("There are", nrow(m_no_m), "measurement records with no corresponding methodology record\n")
if(nrow(m_no_m)) message("See `m_no_m`")



# ===== SITES checks ==== ####

# There are no sites in SITES that lack records in PLOTS
SITES %>% 
  anti_join(PLOTS, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_p
cat("There are", nrow(s_no_p), "sites with no corresponding plot record\n")
if(nrow(s_no_p)) message("See `s_no_p`")

# There are no sites in SITES that lack records in History
SITES %>% 
  anti_join(HISTORY, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_h
cat("There are", nrow(s_no_h), "sites with no corresponding hystory record\n")
if(nrow(s_no_h)) message("See `s_no_h`")

# There are no sites in SITES that lack records in MEASUREMENTS
SITES %>% 
  anti_join(MEASUREMENTS, by = c("sites.sitename")) %>% 
  distinct(site.ID, sites.sitename) ->
  s_no_m
cat("There are", nrow(s_no_m), "sites with no corresponding measurements record\n")
if(nrow(s_no_m)) message("See `s_no_m`")

# Sites should only be defined once
if(any(duplicated(SITES$sites.sitename))) {
  message("There are duplicated variable names in the VARIABLES table!")  
}
if(any(duplicated(SITES$site.ID))) {
  message("There are duplicated variable names in the VARIABLES table!")  
}

# ===== HISTORY checks ==== ####

# There are no sites in History that lack records in MEASUREMENTS
HISTORY %>% 
  anti_join(MEASUREMENTS, by = c("sites.sitename")) %>% 
  distinct(history.ID, sites.sitename) ->
  h_no_m
cat("There are", nrow(h_no_m), "history records with no corresponding measurements record\n")
if(nrow(h_no_m)) message("See `h_no_m`")


# There are no sites in History that lack records in SITES
HISTORY %>% 
  anti_join(SITES, by = c("sites.sitename")) %>% 
  distinct(history.ID, sites.sitename) ->
  h_no_s
cat("There are", nrow(h_no_s), "history records with no corresponding sites record\n")
if(nrow(h_no_s)) message("See `h_no_s`")





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
  distinct(history.ID, hist.cat, hist.type) ->
  h_no_dist
cat("There are", nrow(h_no_dist), "history records with undefined disturbance category/type combinations\n")
if(nrow(h_no_dist)) message("See `h_no_dist`")


# There are no records in HISTORY that lack corresponding records in PLOTS
# (these can be identified based on whether the site-plot combination and the history.ID 
# show up in PLOTS. see metadata to understand how history.ID works in PLOTS)
HISTORY %>% 
  anti_join(PLOTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(history.ID, sites.sitename, plot.name) ->
  h_no_p
cat("There are", nrow(h_no_p), "history records with no corresponding plot record\n")
if(nrow(h_no_p)) message("See `h_no_p`")


# ===== PLOTS checks ==== ####

# No records in PLOTS that lack corresponding records in MEASUREMENTS
PLOTS %>%
  anti_join(MEASUREMENTS, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plot.ID, sites.sitename, plot.name) ->
  p_no_m
cat("There are", nrow(p_no_m), "plots with no corresponding measurement record\n")
if(nrow(p_no_m)) message("See `p_no_m`")


# For each site-plot combination in PLOTS, there should be at least one corresponding record in HISTORY
PLOTS %>% 
  anti_join(HISTORY, by = c("sites.sitename", "plot.name")) %>% 
  distinct(plot.ID, sites.sitename, plot.name) ->
  p_no_h
cat("There are", nrow(p_no_h), "plots with no corresponding history record\n")
if(nrow(p_no_h)) message("See `p_no_h`")


# For each site in PLOTS, there should be a corresponding record in SITES
PLOTS %>%
  anti_join(SITES, by = c("sites.sitename")) %>% 
  distinct(plot.ID, sites.sitename) ->
  p_no_s
cat("There are", nrow(p_no_s), "plots with no corresponding site record\n")
if(nrow(p_no_s)) message("See `p_no_s`")


# ===== HISTTYPE checks ==== ####

# Disturbance types should only be defined once
if(any(duplicated(paste(HISTTYPE$hist.cat, HISTTYPE$hist.type)))) {
  message("There are duplicated history category/types in the HISTTYPE table!")  
}

# All hist.cat exist in HISTORY
if(!all(gsub("\\(_prior\\)", "", HISTTYPE$hist.cat) %in% HISTORY$hist.cat)) stop("There are hist.cat in HISTTYPE that don't exist in History")


# All hist.type exist in HISTORY

if(!all(HISTTYPE$hist.type2 %in% HISTORY$hist.type))  stop("There are hist.type in HISTTYPE that don't exist in History. See HISTTYPE$hist.type2[!HISTTYPE$hist.type2 %in% HISTORY$hist.type]
")
HISTTYPE$hist.type2[!HISTTYPE$hist.type2 %in% HISTORY$hist.type] # Leave Precipitation Diversion

# All hist.type exist in PLOTS
if(!all(HISTTYPE$hist.type %in% unique(c(PLOTS$regrowth.hist.type, PLOTS$dist.mrs.hist.type, PLOTS$dist_1.hist.type, PLOTS$dist_2.hist.type)))) stop("There are hist.cat in HISTTYPE that don't exist in PLOTS. SEE")

  HISTTYPE$hist.type[!HISTTYPE$hist.type %in% unique(c(PLOTS$regrowth.hist.type, PLOTS$dist.mrs.hist.type, PLOTS$dist_1.hist.type, PLOTS$dist_2.hist.type))] # keep for furture records


  
  # ===== PFT checks ==== ####
# PFTs should only be defined once
if(any(duplicated(PFT$pftcode))) {
  message("There are duplicated PFT codes in the PFT table!")  
}
# All pftcode are present in MEASUREMENTS table
if(!all(PFT$pftcode %in% MEASUREMENTS$dominant.veg)) stop("There are pftcode in PFT that don't exist in Measurements. See PFT$pftcode[!PFT$pftcode %in% MEASUREMENTS$dominant.veg]
")
PFT$pftcode[!PFT$pftcode %in% MEASUREMENTS$dominant.veg] # Leave "2VW"   "2GW"   "2FORB" "2LTR"  "2RF"   "2RB"


# ===== METHODOLOGY checks ==== ####


# There should be no records in METHODOLOGY that lack corresponding records in MEASUREMENTS
if(!all(METHODOLOGY$method.ID %in% MEASUREMENTS$method.ID)) stop("There are method.ID in METHODOLOGY that don't exist in Measurements. See METHODOLOGY$method.ID[!METHODOLOGY$method.ID %in% MEASUREMENTS$method.ID]")


# All variables in METHODOLOGY exist in VARIABLES --> not checking as names may change


# ===== VARIABLES checks ==== ####

# All variables in VARIABLES should exist in MEASUREMENTS in either variables.name or covariate_#
if(any(!VARIABLES$variable.name %in% c(MEASUREMENTS$variables.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2))) {
  message("There variables not used in MEASUREMENTS")
  message("Check 
          unique(VARIABLES$variable.name)[!unique(VARIABLES$variable.name)%in% c(MEASUREMENTS$variables.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2)]")
}

unique(VARIABLES$variable.name)[!unique(VARIABLES$variable.name)%in% c(MEASUREMENTS$variables.name, MEASUREMENTS$covariate_1, MEASUREMENTS$covariate_2)] # Leave "NPP_4", "NPP_5", "NPP_understory", "NPP_woody", "ANPP_litterfall_2_C", "ANPP_litterfall_3_C"

# All variables in VARIABLES should exist in METHODOLOGY --> not checking as names may change


# ===== ALLOMETRIES checks ==== ####

# All allometric.equation in ALLOMETRY exist in MEASUREMENTS allometry_1 and allometry_2
if(any(!ALLOMETRY$allometric.equation %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2))) {
  message("There allometric.equation not used in MEASUREMENTS")
  message("Check 
          ALLOMETRY$allometric.equation[!ALLOMETRY$variables.name %in% c(MEASUREMENTS$coV_1.value, MEASUREMENTS$coV_2.value)]")
}

unique(ALLOMETRY$allometric.equation)[!unique(ALLOMETRY$allometric.equation) %in% c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2)] # Keep all


sort(unique(c(MEASUREMENTS$allometry_1, MEASUREMENTS$allometry_2)))







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
  

    if(n.records.v == 0 & nrow(MEASUREMENTS[MEASUREMENTS$variables.name %in% v, ]) > 0){
      Value.for.variables.without.range <- rbind(Value.for.variables.without.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$variables.name %in% v, c("measurement.ID", "sites.sitename", "variables.name", "mean")]))
    }
  
  
  
  if(all(!is.na(c(min.v, max.v)))){
    
    if(!VARIABLES$variable.type[i] %in% "covariates"){
      
      x <- MEASUREMENTS[MEASUREMENTS$variables.name %in% v, ]$mean
      x <- na.omit(as.numeric(ifelse(x %in% na_codes, NA, x)))
      
      value.too.small <- any(x < min.v)
      value.too.big <- any(x > max.v)
      
      flagged.value <- c(x[x < min.v], x[x > max.v])
      
      if(any(value.too.small, value.too.big)) mean.not.within.range <- rbind(mean.not.within.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$variables.name %in% v & MEASUREMENTS$mean %in% flagged.value, c("measurement.ID", "sites.sitename", "variables.name", "mean")]))
      
      
    }
    
    if(VARIABLES$variable.type[i] %in% "covariates"){
      
      x_1 <- MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v, ]$coV_1.value
      x_1 <- na.omit(as.numeric(ifelse(x_1 %in% na_codes, NA, x_1)))
      
      x_2 <- MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v, ]$coV_2.value
      x_2 <- na.omit(as.numeric(ifelse(x_2 %in% na_codes, NA, x_2)))
      
      value_1.too.small <- any(x_1 < min.v)
      value_1.too.big <- any(x_1 > max.v)
      
      value_2.too.small <- any(x_2 < min.v)
      value_2.too.big <- any(x_2 > max.v)
      
      flagged.value <- c(x_1[x_1 < min.v], x_1[x_1 > max.v], x_2[x_2 < min.v], x_2[x_2 > max.v])
      
      if(any(value_1.too.small, value_1.too.big)) covariate_1.not.within.range <- rbind(covariate_1.not.within.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$covariate_1 %in% v & MEASUREMENTS$coV_1.value %in% flagged.value, c("measurement.ID", "sites.sitename", "covariate_1", "coV_1.value")]))
      
      if(any(value_2.too.small, value_2.too.big))  covariate_2.not.within.range <- rbind(covariate_2.not.within.range, as.data.frame(MEASUREMENTS[MEASUREMENTS$covariate_2 %in% v & MEASUREMENTS$coV_2.value %in% flagged.value, c("measurement.ID", "sites.sitename", "covariate_2", "coV_2.value")]))
      
    }
  }
}


cat(paste("There is", nrow(mean.not.within.range), "measurements falling out of variable range"))
if(nrow(mean.not.within.range) > 0) cat("See mean.not.within.range")

cat(paste("There is", nrow(covariate_1.not.within.range), "covariate_1 value(s) falling out of variable range"))
if(nrow(covariate_1.not.within.range) > 0) cat("See covariate_1.not.within.range")

cat(paste("There is", nrow(covariate_2.not.within.range), "covariate_2 value(s) falling out of variable range"))
if(nrow(covariate_2.not.within.range) > 0) cat("See covariate_2.not.within.range")

cat(paste("There is", nrow(Value.for.variables.without.range), "value(s) from a variable that has no defined range (this is probably the first record for that variable)"))
if(nrow(Value.for.variables.without.range) > 0) cat("See Value.for.variables.without.range")


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
    
    
    if(all(!is.na(c(DF_meta.min, DF_meta.max)))){
    
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

