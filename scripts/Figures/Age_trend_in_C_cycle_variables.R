######################################################
# Purpose: Plot age trends in C cycle variables
# Inputs:
# - ForC_simplified table
# - Map of the world (in supplenentary resrouces)
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
library(lme4)
library(multcomp)
library(lsmeans)
library(moments)
library(sp)
library(rgdal)
library(raster)

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
Continents <- readOGR("supplementary_resources/World Map data/Continents/World_Continents.shp")

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####

## Filter out managed, disturbed and no hisotry info sites
ForC_simplified <- ForC_simplified[ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0 & ForC_simplified$history.no.info %in%0, ]

## Make stand.age a numeric variable
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)

## make geographic area and plot.name factors
ForC_simplified$geographic.area <- addNA(ForC_simplified$geographic.area)
ForC_simplified$plot.name <- addNA(ForC_simplified$plot.name)

## Prepare Forest Biomes

### Koeppen region

KOEPPEN <- ifelse(grepl("^A", ForC_simplified$Koeppen), "Tropical",
                  ifelse(grepl("(^C)|(^D.a$)|(^D.b$)", ForC_simplified$Koeppen), "Temperate",
                         ifelse(grepl("(^D.c$)|(^D.d$)", ForC_simplified$Koeppen), "Boreal", "Other")))


### Broadleaf vs Conifer

broadleaf_codes <- c("2TEB", "2TDB", "2TB")
conifer_codes <- c("2TEN", "2TDN", "2TN")

Leaf_Trait <- ifelse(ForC_simplified$dominant.veg %in% broadleaf_codes, "broadleaf",
                     ifelse(ForC_simplified$dominant.veg %in% conifer_codes, "conifer", "Other"))

### combine to get tropical, temperate_broadleaf, temperate_evergreen and boreal

Biome <-  paste(KOEPPEN, Leaf_Trait)
table(Biome)

ForC_simplified$Biome <- factor(ifelse(grepl("Boreal", Biome), "Boreal",
                ifelse(grepl("Tropical", Biome), "Tropical",
                             ifelse(Biome %in% "Temperate broadleaf", Biome,
                                    ifelse(Biome %in% "Temperate conifer", Biome, "Other")))))
table(ForC_simplified$Biome)

# Remove Biome Other
ForC_simplified <- droplevels(ForC_simplified[!ForC_simplified$Biome %in% "Other", ])

# order Biomes correctly
ForC_simplified$Biome <- factor(ForC_simplified$Biome, levels = c("Tropical", "Temperate broadleaf", "Temperate conifer", "Boreal"))
# Remove rows with no Age

ForC_simplified <- droplevels(ForC_simplified[!is.na(ForC_simplified$stand.age), ])

## prepare color for biomes
levels(ForC_simplified$Biome)
color.biome <- c( "red", "green", "blue", "cyan2")

## prepare map
Continents <- crop(Continents, extent(-180, 180, -43, 73))

## prepare variables that need to be grouped
ForC_simplified[ForC_simplified$variable.name %in% c("NPP_1", "NPP_2", "NPP_3",  "NPP_4", "NPP_5"),]$variable.name <- "NPP"
ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_0", "ANPP_1", "ANPP_2"),]$variable.name <- "ANPP"
ForC_simplified[ForC_simplified$variable.name %in% c("ANPP_litterfall_1", "ANPP_litterfall_2", "ANPP_litterfall_0"),]$variable.name <- "ANPP_litterfall"

#### multiply NEP  by -1 anc consider is as NEE
ForC_simplified[ForC_simplified $variable.name %in% c("NEP"),]$mean <- -ForC_simplified[ForC_simplified $variable.name %in% c("NEP"),]$mean 
ForC_simplified[ForC_simplified $variable.name %in% c("NEP"),]$variable.name <- "NEE"

## Prepare list of variables to use (those that have at least 30 records in young forest)
response.variables <- names(which(table(ForC_simplified[ForC_simplified$stand.age < 100 & ForC_simplified$stand.age !=0, ]$variable.name)>= 30))
response.variables <- response.variables[!response.variables %in% c("NPP_understory", "total.ecosystem", "soil")]

# Run analysis + plot####

for(response.v in response.variables) {
  print(response.v)
  
  # right.skewed.response <- response.v %in% right.skewed_response.variables
  
  ### data
  df <- ForC_simplified[ForC_simplified$variable.name %in% response.v, ]
  df.mature <- df[df$stand.age >= 100, ]
  df.young <- df[df$stand.age < 100 & df$stand.age != 0,]  # removing 0 because we are taking the log. Removes 28 recorsd
  
  right.skewed.response <- skewness(df.young$mean) > 2 & all(df.young$mean > 0)
  
  ### ylim
  ylim = range(df$mean)
  
  ### model young
  
  mod.young <- lmer(mean ~ log10(stand.age) + Biome + (1|geographic.area/plot.name), data = droplevels(df.young))
  drop1.result <- drop1(mod.young, k = log(nrow(df.young)))
  age.significant <- drop1.result$AIC[2] > drop1.result$AIC[1]
  
  at.least.10.different.ages.in.each.Biome <- all(tapply(droplevels(df.young)$stand.age, droplevels(df.young)$Biome, function(x) length(x)>=10))
  
  if(age.significant & at.least.10.different.ages.in.each.Biome)   mod.young <- lmer(mean ~ log10(stand.age) * Biome + (1|geographic.area/plot.name), data = droplevels(df.young))

  # mod.without.age <- lmer(mean ~ Biome + (1|geographic.area/plot.name), data = droplevels(df.young))
  # age.significant <- anova(mod.without.age, mod)$"Pr(>Chisq)"[2] < 0.05
  
  newDat <- expand.grid(stand.age = 10^seq(min(log10(df.young$stand.age))+0.01, max(log10(df.young$stand.age)), length.out = 100), Biome = levels(droplevels(df.young$Biome)))

  
  fit <- predict(mod.young, newDat, re.form =  NA)
  
  ### model mature
  
  mod.mature <- try(lmer(mean ~ Biome + (1|geographic.area/plot.name), data = droplevels(df.mature)), silent = T)
  
  if(!class(mod.mature) %in% "try-error"){
    drop1.result <- drop1(mod.mature, k = log(nrow(df.mature)))
    biome.significant <- drop1.result$AIC[2] > drop1.result$AIC[1]
  
    if(biome.significant) { # do pairwise comparison
      pairwise.comp <- glht(mod.mature, linfct = mcp(Biome = "Tukey"))
      pairwise.comp.letter.grouping <- cld(pairwise.comp) 
    }
  }
  
  ### plot
  
  tiff(file = paste0("figures/age_trends/", response.v, ".tiff"), height = 800, width = 1000, units = "px", res = 150)
  
  ### layout figure
  layout(matrix(c(1,1,2,3), ncol = 2, byrow = T), heights = c(1,2), widths = c(5,1))
  
  ### MAP plot all sites ? (even mature?)
  par(mar = c(0,0,0,0))
  plot(Continents, col = "grey", border = "grey")
  
  sites <- df.young[, c("lat", "lon", "Biome")]
  coordinates(sites) <- c("lon", "lat")
  points(sites, col = color.biome[df.young$Biome], pch = 4)
  
  sites <- df.mature[, c("lat", "lon", "Biome")]
  coordinates(sites) <- c("lon", "lat")
  points(sites, col = color.biome[df.mature$Biome], pch = 1)
  
  ### Plot young 
  par(mar = c(5.1,4.1,0,0))
  plot(mean ~ stand.age, data = df.young, col = color.biome[df.young$Biome], xlab = "Age (years - log scaled)", ylab = bquote(.(response.v) ~ " (Mg C " ~ ha^{-1}~")"), log = ifelse(right.skewed.response, "xy", "x"), xlim = c(0.999, 100), ylim = ylim, pch = 4, bty = "L", las = 1)
  
  for(b in levels(df$Biome)){
    y <- fit[newDat$Biome %in% b]
    x <- newDat[newDat$Biome %in% b, ]$stand.age
    lines(y ~ x, col = color.biome[levels(df$Biome) %in% b], lty = ifelse(age.significant, 1, 2))
  
  }
  
  mtext(side = 3, line = -1, adj = 0.03, text = paste("n =", nrow(df.young)), cex = 0.5)
  
  ## boxplot mature
  par(mar = c(5.1,0,0,0))
  boxplot(mean ~ Biome, data = droplevels(df.mature), ylim = ylim, axes = F, xlab = "Mature Forest", col = color.biome[as.factor(levels(df$Biome)) %in% df.mature$Biome], outcol =color.biome[as.factor(levels(df$Biome)) %in% df.mature$Biome], log = ifelse(right.skewed.response, "y", ""))

  if(biome.significant & !class(mod.mature) %in% "try-error") { # do pairwise comparison
    text(x = c(1:length(unique(droplevels(df.mature)$Biome))), y = max(df.mature$mean) + diff(ylim)/50, pairwise.comp.letter.grouping$mcletters$Letters)
  }
  
  mtext(side = 1, line = -1, adj = 0.03, text = paste("n =", nrow(df.mature)), cex = 0.5)
  
  
  dev.off()
}

# Figure 6 and 7 ERL-review####

for( fig in c("Figure6", "Figure7")) {
  

  tiff(file = paste0("figures/age_trends/for_ERL_review/", fig, ".tiff"), height = 1000, width = 1000, units = "px", res = 150)
  
  ### layout figure
  nf <- layout(matrix(c(1,1,4,4,
                        2,3,5,6,
                        7,7,10,10,
                        8,9,11,12,
                        13,13,16,16,
                        14,15,17,18), ncol = 4, byrow = T), heights = rep(c(1,2),3), widths = rep(c(4,1),2))
  
  if (fig %in% "Figure6") variables.of.interest <- c("GPP", "NPP", "ANPP", "R_soil", "R_eco", "NEE")
  if (fig %in% "Figure7")  variables.of.interest <- c("biomass_ag", "biomass_ag_foliage", "biomass_root_fine", "deadwood")
 
  
  for(response.v in variables.of.interest) {
    print(response.v)
    
    # right.skewed.response <- response.v %in% right.skewed_response.variables
    
    ### data
    df <- ForC_simplified[ForC_simplified$variable.name %in% response.v, ]
    df.mature <- df[df$stand.age >= 100, ]
    df.young <- df[df$stand.age < 100 & df$stand.age != 0,]  # removing 0 because we are taking the log. Removes 28 recorsd
    
    right.skewed.response <- skewness(df.young$mean) > 2 & all(df.young$mean > 0)
    
    ### ylim
    ylim = range(df$mean)
    
    ### model young
    
    mod.young <- lmer(mean ~ log10(stand.age) + Biome + (1|geographic.area/plot.name), data = droplevels(df.young))
    drop1.result <- drop1(mod.young, k = log(nrow(df.young)))
    age.significant <- drop1.result$AIC[2] > drop1.result$AIC[1]
    
    at.least.10.different.ages.in.each.Biome <- all(tapply(droplevels(df.young)$stand.age, droplevels(df.young)$Biome, function(x) length(x)>=10))
    
    if(age.significant & at.least.10.different.ages.in.each.Biome)   mod.young <- lmer(mean ~ log10(stand.age) * Biome + (1|geographic.area/plot.name), data = droplevels(df.young))
    
    # mod.without.age <- lmer(mean ~ Biome + (1|geographic.area/plot.name), data = droplevels(df.young))
    # age.significant <- anova(mod.without.age, mod)$"Pr(>Chisq)"[2] < 0.05
    
    newDat <- expand.grid(stand.age = 10^seq(min(log10(df.young$stand.age))+0.01, max(log10(df.young$stand.age)), length.out = 100), Biome = levels(droplevels(df.young$Biome)))
    
    
    fit <- predict(mod.young, newDat, re.form =  NA)
    
    ### model mature
    
    mod.mature <- try(lmer(mean ~ Biome + (1|geographic.area/plot.name), data = droplevels(df.mature)), silent = T)
    
    if(!class(mod.mature) %in% "try-error"){
      drop1.result <- drop1(mod.mature, k = log(nrow(df.mature)))
      biome.significant <- drop1.result$AIC[2] > drop1.result$AIC[1]
      
      if(biome.significant) { # do pairwise comparison
        pairwise.comp <- glht(mod.mature, linfct = mcp(Biome = "Tukey"))
        pairwise.comp.letter.grouping <- cld(pairwise.comp) 
      }
    }
    
    ### plot
    
    ### layout figure
    # layout(matrix(c(1,1,2,3), ncol = 2, byrow = T), heights = c(1,2), widths = c(5,1))
    
    ### MAP plot all sites ? (even mature?)
    par(mar = c(0,0,0,0))
    plot(Continents, col = "grey", border = "grey")
    
    sites <- df.young[, c("lat", "lon", "Biome")]
    coordinates(sites) <- c("lon", "lat")
    points(sites, col = color.biome[df.young$Biome], pch = 4)
    
    sites <- df.mature[, c("lat", "lon", "Biome")]
    coordinates(sites) <- c("lon", "lat")
    points(sites, col = color.biome[df.mature$Biome], pch = 1)
    
    
    mtext(side = 3, line = -1, adj = 0.05, text = paste0(letters[which(variables.of.interest %in% response.v)], ")"), cex = 0.8)
    
    ### Plot young 
    par(mar = c(5.1,4.1,0,0))
    plot(mean ~ stand.age, data = df.young, col = color.biome[df.young$Biome], xlab = "Age (years - log scaled)", ylab = "", log = ifelse(right.skewed.response, "xy", "x"), xlim = c(0.999, 100), ylim = ylim, pch = 4, bty = "L", las = 1, yaxt = "n")
    
    if(!right.skewed.response) axis(2, las = 1)
    if(right.skewed.response) axis(2, at = c(0.1, 1, 10, 100, 1000), labels =  c("0.1", "1", "10", expression(10^{2}), expression(10^{3})), las = 1) # dput(paste0("expression(10^{", seq(-1, 3), "})"))
    
    
    for(b in levels(df$Biome)){
      y <- fit[newDat$Biome %in% b]
      x <- newDat[newDat$Biome %in% b, ]$stand.age
      lines(y ~ x, col = color.biome[levels(df$Biome) %in% b], lty = ifelse(age.significant, 1, 2))
      
    }
    
    mtext(side = 2, bquote(.(response.v) ~ " (Mg C " ~ ha^{-1}~")"), cex = 0.65, line = 2)
    mtext(side = 3, line = -1, adj = 0.03, text = paste("n =", nrow(df.young)), cex = 0.5)
    
    ## boxplot mature
    par(mar = c(5.1,0,0,0))
    boxplot(mean ~ Biome, data = droplevels(df.mature), ylim = ylim, axes = F, xlab = "Mature", col = color.biome[as.factor(levels(df$Biome)) %in% df.mature$Biome], outcol =color.biome[as.factor(levels(df$Biome)) %in% df.mature$Biome], log = ifelse(right.skewed.response, "y", ""))
    
    if(biome.significant & !class(mod.mature) %in% "try-error") { # do pairwise comparison
      text(x = c(1:length(unique(droplevels(df.mature)$Biome))), y = max(df.mature$mean) + diff(ylim)/70, pairwise.comp.letter.grouping$mcletters$Letters)
    }
    
    mtext(side = 1, line = 0, adj = 1, text = paste("n =", nrow(df.mature)), cex = 0.5)
    
    Sys.sleep(time = 1) # this is to avoid a problem when NEE is launch while R_eco has not been plotted yet. The NEE was tkaing R_eco's place
    
  } # for(response.v in variables.of.interest)
  
  dev.off()
  
  
} # for( fig in c("Figure6", "Figure7"))
     