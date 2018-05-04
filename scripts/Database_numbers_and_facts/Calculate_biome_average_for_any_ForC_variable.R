######################################################
# Purpose: Calculate biome average for any ForC variable.
# see issue #2 of ERL-review:
# " We combined some of ForC’s specific variables (e.g., multiple variables for net primary productivity including various components) into more broadly defined variables, resulting in ## variables (detailed here: https://github.com/forc-db/ForC/blob/master/figures/C_cycle_diagrams/ForC_variables_mapping_for_C_cycle_diagrams.csv).: https://github.com/forc-db/ForC/tree/master/ForC_simplified"
# Inputs:
# - ForC_simplified table
# - ForC_variables_mapping_for_C_cycle_diagrams table (in figures/C_cycle_diagrams)
# Outputs:
# - 
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(lubridate)
library(lme4)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
Variables_mapping <- read.csv("figures/C_cycle_diagrams/ForC_variables_mapping_for_C_cycle_diagrams.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


# Prepare biomes ####
## Koeppen region ####

KOEPPEN <- ifelse(grepl("^A", ForC_simplified$Koeppen), "Tropical",
                  ifelse(grepl("(^C)|(^D.a$)|(^D.b$)", ForC_simplified$Koeppen), "Temperate",
                         ifelse(grepl("(^D.c$)|(^D.d$)", ForC_simplified$Koeppen), "Boreal", "Other")))

table(KOEPPEN)
unique(ForC_simplified$Koeppen[KOEPPEN %in% "Other"])

## Broadleaf vs Conifer

broadleaf_codes <- c("2TEB", "2TDB", "2TB")
conifer_codes <- c("2TEN", "2TDN", "2TN")

Leaf_Trait <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
                     ifelse(ForC_simplified$dominant.veg %in% conifer_codes, "conifer", "Other"))
  
table(Leaf_Trait)
unique(ForC_simplified$dominant.veg[Leaf_Trait %in% "Other"])

## Age ####
Age <- as.numeric(ForC_simplified$stand.age)
Age <- ifelse(Age >= 100, "MATURE", "YOUNG")


## combine all ####

Biome <-  paste(KOEPPEN, Leaf_Trait, Age)
table(Biome)

Biomes.of.interest <- c("Tropical broadleaf MATURE",
                        "Tropical broadleaf YOUNG",
                        "Temperate broadleaf MATURE",
                        "Temperate broadleaf YOUNG",
                        "Temperate conifer MATURE",
                        "Temperate conifer YOUNG",
                        "Boreal conifer MATURE",
                        "Boreal conifer YOUNG")



# Prepare Variables_mapping ####

## Ignore NEE_cum_C, GPP_cum_C, and R_eco_cum_C ignored.
Variables_mapping <- Variables_mapping[!Variables_mapping$variable.name %in% c("NEE_cum_C", "GPP_cum_C", "R_eco_cum_C"),]

## Ignore units of C or OM since forC_Simplified is already in units of C only.
Variables_mapping <- Variables_mapping[!grepl("_OM$", Variables_mapping$variable.name),]
Variables_mapping$variable.name <- gsub("_C$", "", Variables_mapping$variable.name)

## replace NA in variable.diagram by variable.name
Variables_mapping$variable.diagram <- ifelse(is.na(Variables_mapping$variable.diagram), Variables_mapping$variable.name, Variables_mapping$variable.diagram)

# Prepare dates of ForC_simplified ####
ForC_simplified$date <- as.Date(date_decimal(as.numeric(ForC_simplified$date)))
ForC_simplified$start.date <- as.Date(date_decimal(as.numeric(ForC_simplified$start.date)))
ForC_simplified$end.date <- as.Date(date_decimal(as.numeric(ForC_simplified$end.date)))

# Prepare output ####
ForC_biome_averages <- NULL

# Make calculations ####

for(b in Biomes.of.interest){
  print(b)
  
  # get the data from the biome + except sites that were managed or disturbed
  B <- ForC_simplified[Biome %in% b & ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0,] # remove managed and disturbed sites
  
  young <- grepl("YOUNG" , b)
 
# Average per plot when multiple measurements. If flux data, do a weighted average according to the length of measurement periods (i.e., end.date - start.date). If several measurement like NPP_1 and NPP_6, take the highest number for that year. 

for (v.diag in unique(Variables_mapping$variable.diagram)) { # for each variable in the C cycle diagram
  print(v.diag)
  
  # get the variables mapping info
  v.map <- Variables_mapping[Variables_mapping$variable.diagram %in% v.diag,]
  v <- v.map$variable.name
  v.type <- unique(v.map$variable.type)
  flux <- v.type %in% "flux"
  
  # subset for the variables of interst
  X <- B[B$variable.name %in% v, ]
  
  # get number of records
  n.records <- nrow(X)
  
  # conversion.factor
  m <- match(X$variable.name, v)
  X$mean <- X$mean * as.numeric(v.map$conversion.factor[m])
  
  # add NA for plots and geographic area as a factor level for both mixed model random effect and average within each groups
  X$plot.name <- addNA(X$plot.name)
  X$geographic.area <- addNA(X$geographic.area)
  
  # consider stand.age as numeric
  X$stand.age <- as.numeric(ifelse(my_is.na(X$stand.age), NA, X$stand.age))

  # if yound forest, compute a mixed effects model with stand.age as a fixed effect and plot as a random effect nested within geographic.area. When the effect of stand.age was significant at p≤0.05, summary statistics were reported as function of age. When there was no significant effect of stand.age, records were averaged as in the preceding two steps for mature stands. For mature stands and young stands with no significant age effects, we computed an unweighted average across geographic.areas.
  
  X.for.model <-  X[!is.na(X$stand.age) & ! X$stand.age %in% 0, ] # removing 0 because we are using log10
  
  if(young & nrow(X.for.model)>10 & length(unique(X.for.model[, c("geographic.area")])) >= 3) { # young + more than 10 n.records + at least 3 different geographic.area
   
    # if(length(unique(X.for.model$geographic.area)) >= 3) { # mixed.model
      mod.null <- lmer(mean ~ 1 + (1|geographic.area), data = X.for.model)
      mod <- lmer(mean ~ log10(stand.age) + (1|geographic.area), data =  X.for.model)
      anova.mod <- anova(mod.null, mod)
      significant <- anova.mod$'Pr(>Chisq)'[2] < 0.05 &  rownames(anova.mod)[2] %in% "mod"
      
    # } # mixed.model
    
    # if(length(unique(X.for.model$geographic.area)) < 3) { # linear model
    #   mod.null <- lm(mean ~ 1, data = X.for.model)
    #   mod <- lm(mean ~ log10(stand.age), data =  X.for.model)
    #   anova.mod <- anova(mod.null, mod)
    #   significant <- anova.mod$'Pr(>F)'[2] < 0.05
    # } # linear model
    
    if(significant){
      summary.mod <- summary(mod)$coefficients
      intercept <- round(summary.mod["(Intercept)", "Estimate" ], 2)
      intercept.se <- round(summary.mod["(Intercept)", "Std. Error" ], 2)
      slope <- round(summary.mod["log10(stand.age)", "Estimate" ], 2)
      slope.se <- round(summary.mod["log10(stand.age)", "Std. Error" ], 2)
      equation <- paste0(format(intercept, nsmall = 2), "\u00b1", format(intercept.se, nsmall = 2), ifelse(sign(slope) == -1, "-log10(age)\u00D7", "+log10(age)\u00D7"), format(abs(slope), nsmall = 2), "\u00b1", format(slope.se, nsmall = 2))
    }
    
    if(!significant) equation = NA
  } # mixed.model
  
  if(!young | !nrow(X.for.model)>10) equation = NA
  
  # if not young or if effect of stand.age was not significant at p≤0.05, average all years per plot, weigthing per measurement perdiod if it is a flux with start and end date + taking higer variable if there is several (like NPP_1 and NPP_2)
  X.split <- split(X, f = list(X$sites.sitename, X$plot.name), drop = T)
  n.plots <- length(X.split) # get number of plots
  
  if(n.plots >= 1) {
  X.final <- NULL
  
  for(i in 1:n.plots) {
    
    x <- X.split[[i]]
    
    # caluculate weigh, keep it only if we are working with flux data and start and end dates are given
    timint <- difftime(x$end.date, x$start.date, units = "days") / 365
    timint <- ifelse(is.na(timint), 1, timint) # take 1 if there is no start and end date
    if(!flux) timint[] <- 1 # take the timint only if we are working in a flux, otherwise just put 1
    
    # get the highest variable name
    v.number <- as.numeric(gsub("(\\w*)([0-9]$)", "\\2", x$variable.name))
    higest.v <- which(v.number %in% max(v.number))[1]
    
    x.out <- x[higest.v,] # take only one row (the highest variable name if there is a number in it)
    x.out$mean <-  weighted.mean(x$mean, timint)# replace the mean by the average of all rows, weighted by time intervalle if flux with start and end date
      
    X.final <- rbind(X.final, x.out)
  }
  
  if(nrow(X.final) != length(X.split)) {stop("Problem when averaging per plot, we don't end up with th right number of observations...")}
  
  X <- X.final
  }
  
  # average per geographic area and weighing by plot.area if it is given for all plots
  X.split <- split(X, f = list(X$geographic.area), drop = T)
  n.areas <- length(X.split) # get number of geographic areas
  
  
  if(n.areas >= 1) {
  X.final <- NULL
  
  for(i in 1:n.areas) {
    
    x <- X.split[[i]]
    
    all.plot.area.given <- all(!my_is.na(x$plot.area))
    
    if(all.plot.area.given) wt.plot.area <- x$plot.area else wt.plot.area <- rep(1, nrow(x))
    
    x.out <- x[1,] # take only one row
    x.out$mean <-  weighted.mean(x$mean, wt.plot.area)# replace the mean by the average of all rows, weighted by plot area if all are given
    
    X.final <- rbind(X.final, x.out)
  }
  
  if(nrow(X.final) != length(X.split)) {stop("Problem when averaging per plot, we don't end up with th right number of observations...")}
  
  X <- X.final
  }
  
  # get statistics
  
  results <- data.frame(Biome = b,
                        variable.type = v.type,
                        variable.diagram = v.diag,
                        mean = round(mean(X$mean), 2),
                        std = round(sd(X$mean), 2),
                        se = round(sd(X$mean) / nrow(X), 2),
                        LCI = round(mean(X$mean) - 1.96 * sd(X$mean) / nrow(X), 2),
                        UCI = round(mean(X$mean) + 1.96 * sd(X$mean) / nrow(X), 2),
                        min = min(X$mean),
                        max = max(X$mean),
                        equation = equation,
                        n.records = n.records,
                        n.plots = n.plots,
                        n.areas = n.areas
                        )
  
  ForC_biome_averages <- rbind(ForC_biome_averages, results)
  
} # for each variable in the C cycle diagram

}

# save ####

write.csv(ForC_biome_averages, file = "numbers_and_facts/ForC_variable_averages_per_Biome.csv", row.names = F)
