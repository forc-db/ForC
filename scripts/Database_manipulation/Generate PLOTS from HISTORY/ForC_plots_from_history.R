######################################################
# Purpose: Generate ForC_plots from ForC_history
# Developped by: Valentine Herrmann - HerrmannV@si.edu (original script was developped by Kristina Anderson-Teixeira in Matlab)
#  R version 3.5.1 (2018-07-02)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(progress)

# Load data ####
HISTORY <- read.csv("data/ForC_history.csv", stringsAsFactors = F)
PLOTS <-  read.csv("data/ForC_plots.csv", stringsAsFactors = F) # this is just to steel the column names from that table

# work on creating/completing PLOTS ####

## create site-plot ID name, use it to identify unique plots, each of which gets one row in new version
site_plot <- paste(toupper(HISTORY$sites.sitename), toupper(HISTORY$plot.name), sep = "_")
plots_list <- unique(site_plot)
n_plots <- length(plots_list)


## cycle through unique plots, pulling out data to assign to new PLOTS table. ##

PLOTS_updated <- NULL

establishements_to_revisit_by_hand <- NULL


pb <- progress_bar$new(total = n_plots, format = "Generating plots: [:bar] :percent in :elapsed")

for (n in 1:n_plots) {
  pb$tick()
  
  # index records for the plot
  index <- which(site_plot %in% plots_list[n]) # index for all records for the plot
  # histID <- n + HISTORY$event.sequence[index]/100
  
  # get fields that are the same for all records in the original version
  index1 <- min(index); #index for just the first record for the plot 
  PLOTID <- floor(HISTORY$history.ID[index1])
  TROP_EXTRATROP <- HISTORY$tropical.extratropical[index1]
  SITE <- HISTORY$sites.sitename[index1]
  PLOT <- HISTORY$plot.name[index1]
  PLOTAREA <- HISTORY$plot.area[index1]
  
  # Indexes for HISTTYPE = no.info or no.disturbance. These will be used multiple times
  i_ni <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'No.info')
  i_ND <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'No.disturbance')
  
  if (length(i_ni) > 1) readline(paste(c(plots_list[n], "length(i_ni) > 1")))
  if (length(i_ND) > 1) i_ND <- i_ND[which.min(HISTORY$event.sequence[i_ND])] #readline(paste(c(plots_list[n], "length(i_ni) > 1")))
  
  
  # establishment of oldest trees
  i1 <-  which(site_plot %in% plots_list[n] & HISTORY$hist.type %in% 'Establishment of oldest trees')
  i2 <-  which(site_plot %in% plots_list[n] & HISTORY$hist.type %in% 'Establishment of key species')
  i_est <- c(i1, i2)
  
  if (length(i_est)==1) {
    EST_RN <- HISTORY$history.ID[i_est]
    EST_DATE <- HISTORY$date[i_est]
    EST_DATE_LOC <- HISTORY$date.loc[i_est]
    EST_DATE_INFERRED <- HISTORY$est.regrowth.assumed.same.year[i_est]
    
    if (length(i2) == 0) {
       EST_TYPE <- 'trees'
    } else {
       EST_TYPE<- 'key_taxa'
    }
   
    EST_NOTES <- HISTORY$hist.notes[i_est]
  }
  if (length(i_est) == 0) {
      if(length(i_ni) == 0) {
        EST_RN <- 0
        EST_DATE <- 'NAC'
        EST_DATE_LOC <- NA
        EST_DATE_INFERRED <- NA
        EST_TYPE <- NA
        EST_NOTES <- NA
      } else {
        EST_RN <- 0
        EST_DATE <- HISTORY$date[i_ni]
        EST_DATE_LOC <- NA
        EST_DATE_INFERRED <- NA
        EST_TYPE <- NA
        EST_NOTES <- HISTORY$hist.notes[i_ni]
      }
    }
  if(length(i_est) > 1) {
    warning(paste('REVISIT BY HAND (ESTABLISHMENT): ', plots_list[n])) # revisit by hand
    establishements_to_revisit_by_hand <- rbind(establishements_to_revisit_by_hand, HISTORY[index, ])
  }


  
  
  # regrowth (most recent)
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth' & HISTORY$hist.type %in% 'Initiation of post-disturbance cohort (natural)')
  i3 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth' & HISTORY$hist.type %in% 'Initiation of post-disturbance cohort (planted or natural)')
  i5 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth' & HISTORY$hist.type %in% 'Planted')
  i6 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth' &  HISTORY$hist.type %in% 'Seeded_trees')
  i7 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth' &  HISTORY$hist.type %in%  'Planted_and_natural_regeneration')
  i_reg <- c(i1, i3, i5, i6, i7)
  
  if(length(i_reg) == 1) {
    REGROWTH_RN <- HISTORY$history.ID[i_reg]
    REGROWTH_DATE <- HISTORY$date[i_reg]
    REGROWTH_DATE_LOC <- HISTORY$date.loc[i_reg]
    EST_DATE_INFERRED <- HISTORY$est.regrowth.assumed.same.year[i_reg]
  
    if(length(i1) == 1) {
      REGROWTH_TYPE <- 'Initiation of post-disturbance cohort (natural)'
      REGROWTH_PLANTING_DENSITY <- NA
    } else {
      if(length(i3) == 1) {
        REGROWTH_TYPE <- 'Initiation of post-disturbance cohort (planted or natural)'
        REGROWTH_PLANTING_DENSITY <- NA
      } else {
        if(length(i5) == 1) {
          REGROWTH_TYPE <- 'Planted'
          REGROWTH_PLANTING_DENSITY <- HISTORY$level[i5]
        } else {
          if(length(i6) == 1) {
            REGROWTH_TYPE <- 'Seeded_trees'
            REGROWTH_PLANTING_DENSITY <- HISTORY$level[i6]
          } else {
            if(length(i7) == 1) {
              REGROWTH_TYPE <- 'Planted_and_natural_regeneration'
              REGROWTH_PLANTING_DENSITY <- HISTORY$level[i7]
            }
          }
        }
      }
    }
    
    REGROWTH_NOTES <- HISTORY$hist.notes[i_reg]
    seqREGROWTH <- HISTORY$event.sequence[i_reg]
  }
  if(length(i_reg) == 0) {
    if(length(i_ni) == 1)  {# cases with No.Info records. For these,we can know the missing value code
      REGROWTH_DATE <- HISTORY$date[i_ni]
      REGROWTH_TYPE <- HISTORY$hist.type[i_ni]
      REGROWTH_NOTES <- HISTORY$hist.notes[i_ni]
    } else { # cases with no confirmed lack of recent regrowth but no regrowth info and no No.Info record to tell us why the info is missing. For these, we must assume 'NAC'.
      REGROWTH_DATE <- 'NAC'
      REGROWTH_TYPE <- 'NAC'
      REGROWTH_NOTES <- NA
      
    }
    
    REGROWTH_RN <- 0
    REGROWTH_DATE_LOC <- NA
    REGROWTH_DATE_INFERRED <- NA
    REGROWTH_PLANTING_DENSITY <- NA
    seqREGROWTH <- 0
    
  }
  if(length(i_reg) > 1)  print(paste('REVISIT BY HAND (REGROWTH): ', plots_list[n])) # revisit by hand
  
  
  #disturbance prior to regrowth (if any)
  
  i1 <-  which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Disturbance' & HISTORY$event.sequence  < seqREGROWTH) # seqREGROWTH[n])
  i_mrsd <- i1
  
  if(length(i_mrsd) == 1) {
    DIST_MRS_RN <- HISTORY$history.ID[i_mrsd]
    DIST_MRS_DATE <- HISTORY$date[i_mrsd]
    DIST_MRS_TYPE <- HISTORY$hist.type[i_mrsd]
    DIST_MRS_ADD_RN <- 0
    MORT_MRS <- HISTORY$percent.mortality[i_mrsd]
  }
  
  if(length(i_mrsd) == 0) { # cases with no records of disturbance prior to regrowth
    DIST_MRS_RN <- 0
    DIST_MRS_ADD_RN <- 0
    
    if (length(i_ND)==1) { #cases with confirmed lack of disturbance since a given date.
      DIST_MRS_DATE <- HISTORY$date[i_ND]
      DIST_MRS_TYPE <- HISTORY$hist.type[i_ND]
      MORT_MRS <- NA
    }
    if (length(i_ni)==1) {  #cases with No.Info records. For these,we can know the missing value code
      DIST_MRS_DATE <- HISTORY$date[i_ni]
      DIST_MRS_TYPE <- HISTORY$hist.type[i_ni]
      MORT_MRS <- HISTORY$percent.mortality[i_ni]
    }
    if(length(i_ND)==0 & length(i_ni)==0) { #cases with no confirmed lack of recent regrowth but no regrowth info and no No.Info record to tell us why the info is missing. For these, we must assume 'NAC'.
      DIST_MRS_DATE <- 'NAC'
      DIST_MRS_TYPE <- 'NAC'
      MORT_MRS <- 'NAC'
      
    }
    
      
  }
  
  if(length(i_mrsd) > 1 ){ # cases with multiple records of disturbance prior to regrowth
    i_mrsd2 <- max(i_mrsd)
    i_mrsd3 <- min(i_mrsd)
    DIST_MRS_RN <- HISTORY$history.ID[i_mrsd2]
    DIST_MRS_DATE <- HISTORY$date[i_mrsd2]
    DIST_MRS_TYPE <- HISTORY$hist.type[i_mrsd2]
    MORT_MRS <- HISTORY$percent.mortality[i_mrsd2]
    if (length(i_mrsd) == 2) {
      DIST_MRS_ADD_RN <- HISTORY$history.ID[i_mrsd3]
      }
    if(length(i_mrsd)>2) {
      DIST_MRS_ADD_RN <- PLOTID # plotN[i_mrsd3]
    }
      
  }
  
  
  # prior disturbance/ regrowth
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Regrowth_prior')
  i2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Disturbance_prior')
  i_prior <- c(i1, i2)
  
  if (length(i_prior) == 1) {
    PRIOR_RN <- HISTORY$history.ID[i_prior]
  }
  
  if (length(i_prior) == 0){
     PRIOR_RN <- 0
  }
 
  if (length(i_prior) > 1){
    PRIOR_RN <- PLOTID # plotN[i_prior[1]]
  }
 

  # disturbance after regrowth or in primary forests or forests with unknown disturbance history

  i1 <-  which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Disturbance' & HISTORY$event.sequence > seqREGROWTH) # seqREGROWTH[n])
  i_dist <- i1
  
  DIST_RN <- c(0, 0, 0)
  DIST_DATE <- c(NA, NA, NA)
  DIST_TYPE <- c(NA, NA, NA)
  MORT <- c(NA, NA, NA)
  
  if(length(i_dist) > 0) {
    for(e in 1:2) {
      if( e <= length(i_dist)) {
        iX <- i_dist[e]
        DIST_RN[e] <- HISTORY$history.ID[iX]
        DIST_DATE[e] <- HISTORY$date[iX]
        DIST_TYPE[e] <- HISTORY$hist.type[iX]
        MORT[e] <- HISTORY$percent.mortality[iX]
      } else {
        DIST_RN[e] <- 0
        DIST_DATE[e] <- NA
        DIST_TYPE[e] <- NA
        MORT[e] <- NA
      }
    }
    
    if (length(i_dist) == 3) DIST_RN[3] <- HISTORY$history.ID[max(i_dist)]
    if (length(i_dist) > 3) DIST_RN[3] <- PLOTID # plotN[i_dist[1]]
    if (length(i_dist) < 3) DIST_RN[3] <- 0 # ?? ask Krista
    
  }

  # managements  
  
  ## set MANAGEMENT field to UM when there's a No.Disturbance record
  if (length(i_ND) != 0) MANAGEMENT <- 'UM'
  if (length(i_ND) == 0) MANAGEMENT <- ''
  
  ## CO2
  iCO2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'CO2_fumigation')
  
  if (length(iCO2) == 1) CO2 <- HISTORY$history.ID[iCO2]
  if (length(iCO2) == 0) CO2 <- 0
  if (length(iCO2) > 1) CO2 <- PLOTID # plotN[iCO2[1]]
  
  
  ## temperature 
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & grepl("Warming", HISTORY$hist.type))
  # i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Warming_soil')
  # i2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Warming_air')
  iTEMP <- i1 # c(i1, i2)
  
  if (length(iTEMP) == 1) TEMPERATURE <- HISTORY$history.ID[iTEMP]
  if (length(iTEMP) == 0) TEMPERATURE <- 0
  if (length(iTEMP) > 1) TEMPERATURE <- PLOTID # plotN[iTEMP[1]]

  
  ## hydrology
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Drained')
  i2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Irrigation')
  i3 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Precipitation diversion')
  iHYDRO <- c(i1, i2,i3) 
  if (length(iHYDRO) == 1) HYDRO <- HISTORY$history.ID[iHYDRO]
  if (length(iHYDRO) == 0) HYDRO <- 0
  if (length(iHYDRO) > 1) HYDRO <- PLOTID # plotN[iHYDRO[1]]
  
  
  ## nutrients
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & grepl('Fertilization', HISTORY$hist.type))
  iNUT <-  i1
  
  if (length(iNUT) == 1)  NUTRIENTS <- HISTORY$history.ID[iNUT]
  if (length(iNUT) == 0)  NUTRIENTS <- 0
  if (length(iNUT) > 1)  NUTRIENTS <- PLOTID # plotN[iNUT[1]]

  
  ## biota
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Herbicide')
  i2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Pesticide')
  i3 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Planted')
  i4 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Seeded_agriculture')
  iBIOTA <- c(i1, i2, i3, i4)
  
  if (length(iBIOTA) == 1)  BIOTA <- HISTORY$history.ID[iBIOTA]
  if (length(iBIOTA) == 0)  BIOTA <- 0
  if (length(iBIOTA) > 1)  BIOTA <- PLOTID # plotN[iBIOTA[1]]

  
  ## other
  i1 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Soil disturbance');
  i2 <- which(site_plot %in% plots_list[n] & HISTORY$hist.cat %in% 'Management' & HISTORY$hist.type %in% 'Other')
  iOTHER <- c(i1, i2)
  
  if (length(iOTHER) == 1) OTHER <- HISTORY$history.ID[iOTHER]
  if (length(iOTHER) == 0) OTHER <- 0
  if (length(iOTHER) > 1) OTHER <- PLOTID # plotN[iOTHER[1]]
  
  
  
  # finish filling in MANAGEMENT status 
  ## MANAGEMENT has already been set to 'UM' (unmanaged) for all sites with no.disturbance.    
  management_sum <- CO2 + TEMPERATURE + HYDRO + NUTRIENTS + BIOTA + OTHER # indicates whether there are any management records (non-zero values).
  MANAGEMENT[management_sum > 0] <- 'M' #set to 'M' (managed) if there are any management events (based on management_sum). 
  MANAGEMENT[is.na(MANAGEMENT)] <- 'NAC' # plots with no management records but no confirmation of no.disturbance.
  
  
  # create matrices grouping fields for output
  OUT_ESTABLISHMENT <- data.frame(EST_RN, EST_DATE, stringsAsFactors = F) # establishment of oldest trees
  OUT_REGROWTH <- data.frame(REGROWTH_RN, REGROWTH_TYPE, REGROWTH_DATE, stringsAsFactors = F) # regrowth
  OUT_DIST_PREV <- data.frame(DIST_MRS_RN, DIST_MRS_TYPE, MORT_MRS, DIST_MRS_DATE, DIST_MRS_ADD_RN, stringsAsFactors = F) # disturbance prior to most recent regrowth
  OUT_DIST <- data.frame(DIST_RN[1], DIST_TYPE[1], MORT[1], DIST_DATE[1], DIST_RN[2], DIST_TYPE[2], MORT[2], DIST_DATE[2], DIST_RN[3], stringsAsFactors = F)  # disturbance after regrowth
  OUT_MAN <- data.frame(CO2, TEMPERATURE, HYDRO, NUTRIENTS, BIOTA, OTHER, stringsAsFactors = F)# management
  
  PLOTS_to_add <- data.frame(PLOTID, SITE, PLOT, PLOTAREA, OUT_ESTABLISHMENT, OUT_REGROWTH, OUT_DIST_PREV, PRIOR_RN, OUT_DIST, OUT_MAN, stringsAsFactors = F)
  # Rename columns
  names(PLOTS_to_add) <- c("plot.ID", "sites.sitename", "plot.name", "plot.area", 
                           "establishment.ID", "year.establishment.oldest.trees", 
                           "regrowth.ID", "regrowth.type", "regrowth.year", 
                           "dist.mrs.ID", "distmrs.type", "mortality", "distmrs.year", "dist.additional.mrs.ID", 
                           "prior.history.ID",
                           "dist1.ID",	"dist1.type",	"dist1.mort",	"dist1.year",
                           "dist2.ID",	"dist2.type",	"dist2.mort",	"dist2.year",
                           "additional.dist.ID",
                           "management.CO2.ID",	"management.temperature.ID",	"management.hydrology.ID",
                           "management.nutrients.ID",	"management.biota.ID",	"management.other.ID")
  
  
  PLOTS_updated <- rbind(PLOTS_updated, PLOTS_to_add, stringsAsFactors = F)
  
  
  
} # for (n in 1:n_plots) 

establishements_to_revisit_by_hand

# SAVE ####
write.csv(PLOTS_updated, "data/ForC_plots.csv", row.names = F) 


