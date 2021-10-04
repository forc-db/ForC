# Flag some suspected duplicated in ForC simplified.
# ## 1/ create age group 0-10-50-100-999
# groups will be:
#   -[0-10]
# -]10-50]
# -]50-100]
# -]100-999]
# -unknown  (NA, NAC, NI...)
# 
# ## 2/ create year
# take date, if any, start date otherwise, unknown if none is known
# 
# ## 3/ group by variable, geographic_area, year and age group
# considering unknown year as its own group, and unknown age group as its own group too
# 
# ## 4/ in each group, flag with these rules
# - if any GROA: flag all others --> DONE
# - if only GROA: keep all --> DONE
# - if no GROA: 
#   - if any ForC and any SRDB:
#   - flag all SRDB
# - apply rules for "if only ForC or only SRDB" below to remaing ForC records
# - if only ForC or only SRDB: 
#   - if variable is GPP or NEP or Reco, keep most recent citation (randomly pick one if ties)
# - if variable is not GPP nor NEP nor Reco, keep oldest citation (randomly pick one if ties)



# clear environment ###
rm(list = ls())

# load libraries ####

# load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)

# 0/ add "suspected.duplicate" field
ForC_simplified$suspected.duplicate <- 0

# 1/ make age group ####
stand.age <- as.numeric(ForC_simplified$stand.age)

ForC_simplified$age.group <- addNA(cut(stand.age, c(-1, 10, 50, 100, 999)))

table(ForC_simplified$age.group)

# 2/ create year ####
date <- as.numeric(ForC_simplified$date)
start.date <- as.numeric(ForC_simplified$start.date)

ForC_simplified$year <- addNA(factor(floor(ifelse(!is.na(date), date, start.date))))

table(ForC_simplified$year )



# 3/ and 4/ group by variable, geographic are, year and age group and flag with the rules ####
VGYA <- apply(ForC_simplified[, c("variable.name", "geographic.area", "year", "age.group")], 1, paste, collapse = " ")

groups_to_flag <- names(which(table(VGYA) > 1)) # looking only at groups with more than one record

for( vgya in groups_to_flag) {
  message("\rflagging group ", which(groups_to_flag %in% vgya),"/", length(groups_to_flag), "...", appendLF = F)
  
  X <- ForC_simplified[VGYA %in% vgya, ]
  
  idx.GROA <- X$ForC.investigator %in% "Dr. Susan Cook-Patton"
  idx.ForC <- !X$ForC.investigator %in% c("Dr. Susan Cook-Patton", "Ben Bond-Lamberty")
  idx.SRDB <- X$ForC.investigator %in% "Ben Bond-Lamberty"
  
  
  any.GROA <- any(idx.GROA)
  only.GROA <- all(idx.GROA)
  no.GROA <- !any(idx.GROA)
  
  any.ForC <- any(idx.ForC)
  only.ForC <- all(idx.ForC)
  
  any.SRDB <- any(idx.SRDB)
  only.SRDB <- all(idx.SRDB)
  
  ## if any GROA: flag all others --> DONE
  if(any.GROA) X$suspected.duplicate[!idx.GROA] <- 1
  
  ## if only GROA: keep all (already 0 by default so no need to do anything here) --> DONE
  
  ##   if no GROA:
  if(no.GROA) {
    ## if any ForC and any SRDB:
    if(any.ForC & any.SRDB) X$suspected.duplicate[!idx.SRDB] <- 1
    
    if(only.ForC | only.SRDB | (any.ForC & any.SRDB)) { # repeating (any.ForC & any.SRDB) because the SRDB above are already flagged
      
      GPP_NEP_or_Reco <- any(X$variable.name %in% c("GPP", "NEP", "R_eco"))
      
      citation.year <- regmatches(X$citation.ID, regexpr("\\d{4}", X$citation.ID))
      
      if(length(citation.year)!=nrow(X)) stop()
      set.seed(1)
      oldest.citation_not_suspected <- sample(which(citation.year %in%  min(citation.year[X$suspected.duplicate==0]) & X$suspected.duplicate==0), 1) # randomly pick the oldest citation
      
      set.seed(1)
      most.recent_not_suspected <- sample(which(citation.year %in%  max(citation.year[X$suspected.duplicate==0]) & X$suspected.duplicate==0), 1) # randomly pick the oldest citation
      
      ## if variable is GPP or NEP or Reco, keep most recent citation (randomly pick one if ties)
      if(GPP_NEP_or_Reco) X$suspected.duplicate[-oldest.citation_not_suspected] <- 1
      
      ## if variable is not GPP nor NEP nor Reco, keep oldest citation (randomly pick one if ties)
      if(!GPP_NEP_or_Reco) X$suspected.duplicate[-most.recent_not_suspected] <- 1
      
    }
    
    
  }
  
  
  # check that we have only one record kept if only SRC< only ForC
  if(no.GROA & (only.ForC | only.SRDB | (any.ForC & any.SRDB))) if(sum(X$suspected.duplicate == 0) >1) stop()
  
  
  # put back into ForC
  ForC_simplified[VGYA %in% vgya, ] <- X
  
}

# unflag when site is confirmed.unique = 1 or potential_duplicate_group  = "0" or duplicate_precedence = 1
SITES <- read.csv("data/ForC_sites.csv", stringsAsFactors = F)
idx_sites <- SITES$confirmed.unique %in% "1" | SITES$potential_duplicate_group %in% "0" | SITES$duplicate_precedence %in% "1"

ForC_simplified$suspected.duplicate[ForC_simplified$sites.sitename %in% SITES$sites.sitename[idx_sites] ] <- 0


# #unflag "Tepley_2017_vtfl" and "Fleming_1998_conm"
# 
# ForC_simplified$suspected.duplicate[ForC_simplified$citation.ID %in% "Tepley_2017_vtfl"] <- 0
# ForC_simplified$suspected.duplicate[ForC_simplified$citation.ID %in% "Fleming_1998_conm"] <- 0
# 
# ForC_simplified$suspected.duplicate[ForC_simplified$citation.ID %in% "McGarvey_2015_csio"]

table(ForC_simplified$suspected.duplicate)
table(ForC_simplified$suspected.duplicate, ForC_simplified$managed==0 & ForC_simplified$disturbed==0 & ForC_simplified$history.no.info==0) # will remove 5911 record from what we have been looking at in ERL review so far (9/29/2020)



# save ####
write.csv(ForC_simplified[, -which(colnames(ForC_simplified) %in% c("age.group", "year"))], file = "ForC_simplified/ForC_simplified.csv", row.names = F)
