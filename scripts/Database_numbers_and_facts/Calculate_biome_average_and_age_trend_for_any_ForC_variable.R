######################################################
# Purpose: Calculate biome average and age trend for any ForC variable.
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

summary_for_ERL <- read.csv("numbers_and_facts/C_variables_summary_for_ERL_review.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}


## map of continents
Continents <- readOGR("supplementary_resources/World Map data/Continents/World_Continents.shp")

## prepare color and abbreviations for biomes ####
levels(ForC_simplified$Biome)
color.biome <- c("Tropical broadleaf" = "tomato", "Temperate broadleaf" = "yellowgreen", "Temperate conifer" = "cyan2", "Boreal conifer" = "cadetblue")
abb.biome <- c("Tropical broadleaf" = "TrB", "Temperate broadleaf" = "TeB", "Temperate conifer" = "TeN", "Boreal conifer" = "BoN")

## prepare map
Continents <- crop(Continents, extent(-180, 180, -43, 73))

# Prepare data ####

## Filter out managed, disturbed and no hisotry info sites
ForC_simplified <- ForC_simplified[ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0 & ForC_simplified$history.no.info %in%0, ]

## Make stand.age a numeric variable
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)


## Prepare dates
ForC_simplified$date <- as.Date(date_decimal(as.numeric(ForC_simplified$date)))
ForC_simplified$start.date <- as.Date(date_decimal(as.numeric(ForC_simplified$start.date)))
ForC_simplified$end.date <- as.Date(date_decimal(as.numeric(ForC_simplified$end.date)))


## make geographic area and plot.name factors
ForC_simplified$geographic.area <- addNA(ForC_simplified$geographic.area)
ForC_simplified$plot.name <- addNA(ForC_simplified$plot.name)

## Prepare biomes ####
### Koeppen region

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
Age <- ifelse(ForC_simplified$stand.age >= 100, "MATURE", "YOUNG")
ForC_simplified$Age <- Age


## combine all

Biome <-  paste(KOEPPEN, Leaf_Trait) #, Age)
Biome_age <- paste(KOEPPEN, Leaf_Trait, Age)

table(Biome)
table(Biome_age)

ForC_simplified$Biome <- factor(ifelse(grepl("Other", Biome), "Other", Biome)) 
ForC_simplified$Biome_age <- Biome_age
table(ForC_simplified$Biome)

## Remove Biome Other ###
ForC_simplified <- droplevels(ForC_simplified[!ForC_simplified$Biome %in% "Other", ])
table(ForC_simplified$Biome)
table(ForC_simplified$Biome_age)

## keep only biomes of interest ###

Biomes.of.interest <- c("Tropical broadleaf MATURE",
                        "Tropical broadleaf YOUNG",
                        "Temperate broadleaf MATURE",
                        "Temperate broadleaf YOUNG",
                        "Temperate conifer MATURE",
                        "Temperate conifer YOUNG",
                        "Boreal conifer MATURE",
                        "Boreal conifer YOUNG")

ForC_simplified <- droplevels(ForC_simplified[ForC_simplified$Biome_age %in% Biomes.of.interest, ])

# order Biomes correctly
ForC_simplified$Biome <- factor(ForC_simplified$Biome, levels = c("Tropical broadleaf", "Temperate broadleaf", "Temperate conifer", "Boreal conifer"))

# Prepare Variables_mapping ####

## Ignore NEE_cum_C, GPP_cum_C, and R_eco_cum_C ignored.
Variables_mapping <- Variables_mapping[!Variables_mapping$variable.name %in% c("NEE_cum_C", "GPP_cum_C", "R_eco_cum_C"),]



## Ignore units of C or OM since forC_Simplified is already in units of C only.
Variables_mapping <- Variables_mapping[!grepl("_OM$", Variables_mapping$variable.name),]
Variables_mapping$variable.name <- gsub("_C$", "", Variables_mapping$variable.name)

##Ignore variables we dont have enough data for
Variables_mapping <- Variables_mapping[Variables_mapping$variable.name %in% ForC_simplified$variable.name,]

## replace NA in variable.diagram by variable.name
Variables_mapping$variable.diagram <- ifelse(is.na(Variables_mapping$variable.diagram), Variables_mapping$variable.name, Variables_mapping$variable.diagram)



# prepare ERL review summary ####
summary_for_ERL$variable.diagram <- gsub("\\$|\\{|\\}| ", "", summary_for_ERL$Variable)
summary_for_ERL$variable.diagram <- gsub("-", "_", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("DW", "deadwood", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("M_woody", "woody.mortality", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("^B_", "biomass_", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("biomass_ag_wood", "biomass_ag_woody", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("OL", "organic.layer", summary_for_ERL$variable.diagram)
summary_for_ERL$variable.diagram <- gsub("_tot", "", summary_for_ERL$variable.diagram)
setdiff(summary_for_ERL$variable.diagram, Variables_mapping$variable.diagram) # shoule be only "R_het_ag" "R_het" , we have no data for those

# Prepare output ####
ForC_biome_averages <- NULL
v_not_enough_data_for_mature <- NULL
Summary_table <- NULL
age_trend_model_summaries <- NULL

# Make calculations for young and mature plots ####

# Average per plot when multiple measurements. If flux data, do a weighted average according to the length of measurement periods (i.e., end.date - start.date). If several measurement like NPP_1 and NPP_6, take the highest number for that year. 

for (v.diag in intersect(summary_for_ERL$variable.diagram, Variables_mapping$variable.diagram)) { # for each variable in the C cycle diagram
  print(v.diag)
  
  # get the variables mapping info
  v.map <- Variables_mapping[Variables_mapping$variable.diagram %in% v.diag,]
  v <- v.map$variable.name
  v.type <- unique(v.map$variable.type)
  flux <- v.type %in% "flux"
  data.filter <- unique(v.map$data.filter)
  
  # subset for the variables of interst
  df <- ForC_simplified[ForC_simplified$variable.name %in% v  & !is.na(ForC_simplified$stand.age) & !ForC_simplified$stand.age %in% 0, ]  # removing age 0 because we are taking the log for youngs
  
  # exclude min dbh >10 when we know
  if(data.filter %in% "Exclude min.dbh >10.  (Do not exclude min.dbh NAC.)") df <- df[as.numeric(df$min.dbh) <10 |is.na(as.numeric(df$min.dbh)),]

  # conversion.factor
  m <- match(df$variable.name, v)
  df$mean <- df$mean * as.numeric(v.map$conversion.factor[m])

  
  
  # if young forest, compute a mixed effects model with stand.age and Biome as a fixed effect and plot as a random effect nested within geographic.area. When the effect of stand.age was significant at p≤0.05, summary statistics were reported as function of age. When there was no significant effect of stand.age, records were averaged as in the preceding two steps for mature stands. For mature stands and young stands with no significant age effects, we computed an unweighted average across geographic.areas.
  
  df.mature <- droplevels(df[df$Age %in% "MATURE", ])
  df.young <- droplevels(df[df$Age %in% "YOUNG",]) 
  
  enough.data.for.model.young <- nrow(df.young) >= 30

  
  ### model young ####
  enough.data.for.mixed.model <- nrow(unique(df.young[, c("geographic.area", "plot.name")])) < nrow(df.young) & nrow(unique(df.young[, c("geographic.area", "plot.name")])) >=3
  
  at.least.10.different.ages.in.each.Biome <- all(tapply(df.young$stand.age, df.young$Biome, function(x) length(x)>=10))
  
  more.than.one.biome <- length(unique(df.young$Biome)) > 1
  
  if(enough.data.for.model.young) {
    
    
    right.skewed.response <- skewness(df.young$mean) > 2 & all(df.young$mean > 0)
    if(enough.data.for.mixed.model) {
      if(more.than.one.biome) mod.young <- lmer(mean ~ -1 + log10(stand.age) + Biome + (1|geographic.area/plot.name), data = df.young) # when there is enough data for mixed model
      if(!more.than.one.biome) mod.young <- lmer(mean ~ -1 + log10(stand.age) + (1|geographic.area/plot.name), data = df.young) # when there is enough data for mixed model
      age.significant <- AIC(mod.young) < AIC(update(mod.young, ~ . -log10(stand.age) ))
    } else {
      if(more.than.one.biome) {
        mod.young <- lm(mean ~ -1 + log10(stand.age) + Biome , data = df.young)# when there is not enough data for mixed model
      } else {
        mod.young <- lm(mean ~ -1 + log10(stand.age), data = df.young) # when there is not enough data for mixed model
      }
      age.significant <-  summary(mod.young)$coefficients['log10(stand.age)', 'Pr(>|t|)'] < 0.05
    }
    
    if(age.significant & at.least.10.different.ages.in.each.Biome ) {
      if( AIC(mod.young) > AIC(update(mod.young, ~ .+log10(stand.age):Biome)))  {
        mod.young <- update(mod.young, ~ .+log10(stand.age):Biome)
        interaction.sig = TRUE
      }  else {
        interaction.sig = FALSE
      }
      
    } else {interaction.sig = F}
    
    
    newDat<- expand.grid(stand.age = 10^seq(min(log10(df.young$stand.age))+0.01, max(log10(df.young$stand.age)), length.out = 100), Biome = levels(df.young$Biome))
    
    fit.young  <- predict(mod.young, newDat, re.form =  NA)
  } else {age.significant = FALSE
  right.skewed.response = F}
  
  ##### get the equations for each Biomes ####
  equation <- list()
  for(b in gsub(" MATURE| YOUNG", "", Biomes.of.interest)) {
  if(age.significant & b %in% df.young$Biome){
    summary.mod <- summary(mod.young)$coefficients
  
    idx_intercept <- rownames(summary.mod) %in% paste0("Biome",b)
    idx_slope <- grepl(paste0("\\:Biome", b), rownames(summary.mod))
    
    intercept <- round(summary.mod[idx_intercept, "Estimate" ], 2)
    intercept.se <- round(summary.mod[idx_intercept, "Std. Error" ], 2)
    slope <- ifelse(interaction.sig, round(summary.mod["log10(stand.age)", "Estimate" ] + ifelse(sum(idx_slope) > 0, summary.mod[idx_slope, "Estimate" ], 0), 2), round(round(summary.mod["log10(stand.age)", "Estimate" ], 2)))  
    
    slope.1 <- round(summary.mod["log10(stand.age)", "Estimate" ], 2)  
    slope1.se <- round(summary.mod["log10(stand.age)", "Std. Error" ], 2)
    slope2 <- ifelse(interaction.sig, round( ifelse(sum(idx_slope) > 0, summary.mod[idx_slope, "Estimate" ], 0), 2), "-")
    slope2.se <- ifelse(interaction.sig, round( ifelse(sum(idx_slope) > 0, summary.mod[idx_slope, "Std. Error" ], 0), 2), "-")
    
    equation[[paste(b, "YOUNG")]] <- NA # paste0(format(intercept, nsmall = 2), ifelse(sign(slope) == -1, "-log10(age)\u00D7", "+log10(age)\u00D7"), format(abs(slope), nsmall = 2)) # we removed the equation from the C-cycle diagrams
    equation[[paste(b, "MATURE")]] <- NA
      
      # equation <- paste0(format(intercept, nsmall = 2), "\u00b1", format(intercept.se, nsmall = 2), ifelse(sign(slope) == -1, "-log10(age)\u00D7", "+log10(age)\u00D7"), format(abs(slope), nsmall = 2), "\u00b1", format(slope.se, nsmall = 2))
    }
   
  if(!age.significant | !b %in% df.young$Biome) {
    equation[[paste(b, "YOUNG")]] <- NA
  equation[[paste(b, "MATURE")]] <- NA
  }
  }
  
  ### get model summiers for ERL_review SI table
  age_trend_model_summaries <- rbind(age_trend_model_summaries,
                                     data.frame(Variable = summary_for_ERL$Variable[match(v.diag, summary_for_ERL$variable.diagram)], 
                                                Parameter = rownames(summary(mod.young)$coefficients),
                                                round(summary(mod.young)$coefficients, 2)))
 
  
  ### model mature ####
  
  mod.mature <- try(lmer(mean ~ -1 + Biome + (1|geographic.area/plot.name), data = droplevels(df.mature)), silent = T)
  
  if(!class(mod.mature) %in% "try-error"){
    drop1.result <- drop1(mod.mature, k = log(nrow(df.mature)))
    biome.significant <- drop1.result$AIC[2] > drop1.result$AIC[1]
    
    if(biome.significant) { # do pairwise comparison
      pairwise.comp <- glht(mod.mature, linfct = mcp(Biome = "Tukey"))
      pairwise.comp.letter.grouping <- cld(pairwise.comp) 
      order.mature.biomes <- pairwise.comp.letter.grouping$mcletters$Letters[order(summary(mod.mature)$coefficients[,1], decreasing = T)]
      names(order.mature.biomes) <- abb.biome[names(order.mature.biomes)]
    }
  } else {
    v_not_enough_data_for_mature <- c(v_not_enough_data_for_mature, v.diag)
  }
  
  # fill in table of results, by biome of interest ####
  # if not young or if effect of stand.age was not significant at p≤0.05, average all years per plot, weigthing per measurement perdiod if it is a flux with start and end date + taking higer variable if there is several (like NPP_1 and NPP_2)
  for(b in Biomes.of.interest) {
    X <- df[df$Biome_age %in% b,]
    
    n.records <- nrow(X)
    
    # split by plot
    X.split <- split(X, f = list(X$sites.sitename, X$plot.name), drop = T)
    n.plots <- length(X.split) # get number of plots
    
    if(n.plots >= 1) {
      X.final <- NULL
      
      for(i in 1:n.plots) {
        
        x <- X.split[[i]]
        
        # caluculate weigh, keep it only if we are working with flux data and start and end dates are given
        timint <- difftime(x$end.date, x$start.date, units = "days") / 365
        timint <- ifelse(is.na(timint), 1, timint) # take 1 if there is no start and end date
        timint <- ifelse(timint == 0, 1, timint) # take 1 if there is start and end date are equal (assuming they are 1 year measuremet from Jan 01 to dec 31)
        if(!flux) timint[] <- 1 # take the timint only if we are working in a flux, otherwise just put 1
        
        # get the highest variable name
        v.number <- as.numeric(gsub("(\\w*)([0-9]$)", "\\2", x$variable.name))
        higest.v <- which(v.number %in% max(v.number))[1]
        
        x.out <- x[higest.v,] # take only one row (the highest variable name if there is a number in it)
        x.out$mean <-  weighted.mean(x$mean, timint)# replace the mean by the average of all rows, weighted by time intervalle if flux with start and end date
        
        X.final <- rbind(X.final, x.out)
        
        if(is.na(x.out$mean)) stop("problem: mean is NA") # stop if we get an NA for the mean
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
        
        if(all.plot.area.given) wt.plot.area <- as.numeric(x$plot.area) else wt.plot.area <- rep(1, nrow(x))
        
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
                          equation = equation[[b]],
                          n.records = n.records,
                          n.plots = n.plots,
                          n.areas = n.areas
    )
    
    ForC_biome_averages <- rbind(ForC_biome_averages, results)
    
  }
 

  
  ### fill ERL review summary table ####
  idx_ERL <- summary_for_ERL$variable.diagram %in% v.diag
  summary_for_ERL$records[idx_ERL] <- nrow(df)
  summary_for_ERL$plots[idx_ERL] <- nrow(unique(df[, c("sites.sitename", "plot.name")]))
  summary_for_ERL$geographic.areas[idx_ERL] <- length(unique(df$geographic.area))
 if(biome.significant) { 
  biome.differences <- names(order.mature.biomes)[1]
  for(i in seq_along(order.mature.biomes)[-1]) {
    biome.differences <- paste(biome.differences, ifelse(any(unlist(strsplit(order.mature.biomes[i], "")) %in% unlist(strsplit(order.mature.biomes[i-1], ""))), "$\\ge$", ">"), names(order.mature.biomes)[i])
  }
  summary_for_ERL$biome.differences[idx_ERL] <- biome.differences
  } else {
    summary_for_ERL$biome.differences[idx_ERL] <-"n.s."
  }
  summary_for_ERL$age.trend[idx_ERL] <- paste0(c(ifelse(sign(summary(mod.young)$coefficients[1])>0, "+", "-")[age.significant], "xB"[interaction.sig]), collapse = "; ")
  summary_for_ERL$age.trend[idx_ERL] <- ifelse(  summary_for_ERL$age.trend[idx_ERL] == "", "n.s.",   summary_for_ERL$age.trend[idx_ERL])
  
  
  ### plot ####
  
  
  ### ylim
  ylim = range(df$mean)
  
  png(file = paste0("figures/age_trends/", v.diag, ".png"), height = 3, width = 5, units = "in", res = 150)
  
  ### layout figure
  layout(matrix(c(1,1,2,3), ncol = 2, byrow = T), heights = c(1,2), widths = c(5,1))
  
  ### MAP plot all sites ? (even mature?)
  par(mar = c(0,0,0,0))
  plot(Continents, col = "grey", border = "grey")
  
  if(nrow(df.young) > 0) {
    sites <- df.young[, c("lat", "lon", "Biome")]
    coordinates(sites) <- c("lon", "lat")
    points(sites, col = color.biome[as.character(df.young$Biome)], pch = 4)
  }  
  if(nrow(df.mature) > 0) {
    sites <- df.mature[, c("lat", "lon", "Biome")]
    coordinates(sites) <- c("lon", "lat")
    points(sites, col = color.biome[as.character(df.mature$Biome)], pch = 1)
  }
    
    ### Plot young 
    par(mar = c(5.1,4.1,0,0))
    if(nrow(df.young)>0) {
      plot(mean ~ stand.age, data = df.young, col = color.biome[as.character(df.young$Biome)], xlab = "Age (years - log scaled)", ylab = eval(parse(text = ifelse(v.type %in% "flux",  paste0("expression(", v.diag, "~'(Mg C ha'^{-1}~'yr'^{-1}*')')"), paste0("expression(", v.diag, "~'(Mg C ha'^{-1}*')')")))), log = ifelse(right.skewed.response, "xy", "x"), xlim = c(0.999, 100), ylim = ylim, pch = 4, bty = "L", las = 1)
      
      for(b in levels(df.young$Biome)){
        y <- fit.young[newDat$Biome %in% b]
        x <- newDat[newDat$Biome %in% b, ]$stand.age
        lines(y ~ x, col = color.biome[names(color.biome) %in% b], lty = ifelse(age.significant, 1, 2))
        
      }
    } else {
      plot(1,1, col = "white", xlab = "Age (years - log scaled)", ylab = eval(parse(text = ifelse(v.type %in% "flux",  paste0("expression(", v.diag, "~'(Mg C ha'^{-1}~'yr'^{-1}*')')"), paste0("expression(", v.diag, "~'(Mg C ha'^{-1}*')')")))), log = ifelse(right.skewed.response, "xy", "x"), xlim = c(0.999, 100), ylim = ylim, pch = 4, bty = "L", las = 1)
    }
      
      
      
      mtext(side = 3, line = -1, adj = 0.03, text = paste("n =", nrow(df.young)), cex = 0.5)
    
    ## boxplot mature
    par(mar = c(5.1,0,0,0))
    if(nrow(df.mature) > 0) {
      boxplot(mean ~ Biome, data = df.mature, ylim = ylim, axes = F, xlab = "Mature", col = color.biome[levels(df$Biome)], outcol =color.biome[levels(df$Biome)], log = ifelse(right.skewed.response, "y", ""))
      
      if(biome.significant & !class(mod.mature) %in% "try-error") { # do pairwise comparison
        text(x = c(1:length(unique(df.mature$Biome))), y = max(df.mature$mean) + diff(ylim)/50, pairwise.comp.letter.grouping$mcletters$Letters)
      }
    } else {
      boxplot(c(0,0,0,0), axes = F, border = "white", xlab = "Mature Forest")
    }
    
    mtext(side = 1, line = -1, adj = 0.03, text = paste("n =", nrow(df.mature)), cex = 0.5)
    
    # dev.off() ####
    dev.off()
    
    
    
    
  
} # for each variable in the C cycle diagram

# Figure for ERL-review####
for( fig in c("Flux_age_trends", "Stock_age_trends")) {
  
 png(file = paste0("figures/age_trends/for_ERL_review/", fig, ".png"), height = 9, width = 10, units = "in", res = 300)
  
  ### layout figure
 par(mfrow = c(3, 2), mar = c(0,0,0,0))
  
  
  if (fig %in% "Flux_age_trends") variables.of.interest <- c("GPP", "NPP", "ANPP", "R_soil", "R_eco", "NEP")
  if (fig %in% "Stock_age_trends")  variables.of.interest <- c("biomass_ag", "biomass_foliage", "biomass_root_fine", "deadwood_standing", "deadwood_down", "organic.layer")
 
 plot.n = 0
 
  for(v.diag in variables.of.interest) {
    img <- readPNG(paste0("figures/age_trends/", v.diag, ".png"))
    
    plot.n <- plot.n +1
    
    
    plot(0:100, 0:100, type = "n", axes = F, xlab = "", ylab = "")
    
    rasterImage( img , xleft = 0, xright = 100,
                 ybottom = 0, ytop = 100)
    
    
    mtext(side = 3, line = -1, adj = 0.05, text = paste0(letters[which(variables.of.interest %in% v.diag)], ")"), cex = 0.8)
    
   
  }
 dev.off()
  
  
} # for( fig in c("Flux_age_trends", "Stock_age_trends"))


# copy over "Flux_age_trends", "Stock_age_trends" to the ERL_review repo ####
file.copy(c("figures/age_trends/for_ERL_review/Flux_age_trends.png",
            "figures/age_trends/for_ERL_review/Stock_age_trends.png"),
          paste0(dirname(getwd()), "/ERL-review/manuscript/tables_figures/", c("Flux_age_trends.png", "Stock_age_trends.png")), overwrite = T)




# save ####

write.csv(ForC_biome_averages, file = "numbers_and_facts/ForC_variable_averages_per_Biome.csv", row.names = F)
write.csv(summary_for_ERL, file = "numbers_and_facts/C_variables_summary_for_ERL_review.csv", row.names = F)
names(summary_for_ERL) <- gsub("\\.", " ", names(summary_for_ERL))
write.csv(summary_for_ERL[,-8], file = paste0(dirname(getwd()), "/ERL-review/manuscript/tables_figures/C_variables.csv"), row.names = F)

names(age_trend_model_summaries) <- c("Variable", "Parameter", "Estimate", "SE", "$t_{value}$")
write.csv(age_trend_model_summaries, file = paste0(dirname(getwd()), "/ERL-review/manuscript/tables_figures/SI_age_trend_model_summaries.csv"), row.names = F)
if(!is.null(v_not_enough_data_for_mature)) {
  write.csv(v_not_enough_data_for_mature, "figures/age_trends/for_ERL_review/v_not_enough_data_for_mature.txt", row.names = F, quote = F, col.names = NULL)
  write.csv(v_not_enough_data_for_mature, paste0(dirname(getwd()), "/ERL-review/manuscript/tables_figures/v_not_enough_data_for_mature.txt"), row.names = F, quote = F, col.names = NULL, )
}
