######################################################
# Purpose: Test for C cycle closure (consistency)
# see issue #16 of ERL-review:
# "test the internal consistency of the ensemble C cycles derived from ForC. The following text describes what I'd like to do. From a quick scan of the figures, it appears to be true, but of course needs to be formally verified:
# For variables with records from ≥7 distinct geographic areas, these ensemble C budgets were generally consistent. That is, component fluxes summed to within 1 std of more inclusive fluxes in all but one instance (in temperate conifer forests, aboveground woody biomass + foliage biomass > aboveground biomass + 1std). "
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.4 (2018-03-15)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# Load data ####
ForC_biome_averages <- read.csv("numbers_and_facts/ForC_variable_averages_per_Biome.csv", stringsAsFactors = F)
components_maping <- read.csv("figures/C_cycle_diagrams/ForC_variables_components_mapping.csv", stringsAsFactors = F)

# Get the feasable components for each variable and each Biome####
component_closure <- NULL

for(b in unique(ForC_biome_averages$Biome)) {
  print(b)
  X <- ForC_biome_averages[ForC_biome_averages$Biome %in% b, ]
  
  for( v in intersect(components_maping$variable.name, ForC_biome_averages$variable.diagram)) {
    
    print(v)
    
    # equation
    eq_th <- components_maping[components_maping$variable.name %in% v, ]$theoritical.components.equation
      
    # Theoritical components
    eq_comp_th <- strsplit(eq_th, " |-|\\+", eq_th)[[1]]
    eq_comp_th <- eq_comp_th[!eq_comp_th %in% ""]
    
    # Components with enough data
    eq_comp <- eq_comp_th[X[X$variable.diagram %in% eq_comp_th, ]$n.areas >=7]
    
    if(X[X$variable.diagram %in% v, ]$n.areas >= 7) { # enough geograhic areas for the most comprehensive variable
      

      # equation
      if(all(eq_comp_th %in% eq_comp)) eq = eq_th
      if(!all(eq_comp_th %in% eq_comp)) {
        missing_comp <- eq_comp_th[!eq_comp_th %in% eq_comp]
        eq = gsub(paste(missing_comp, collapse = "|"), "0", eq_th)
      }
      
      # Calcul
      y <- X[X$variable.diagram %in% v, ]
      
      Xs <- as.data.frame(t(X$mean))
      colnames(Xs) <- X$variable.diagram
      
      sum.of.components <- with(Xs, eval(parse(text = eq)))
    
      test <- ifelse(sum.of.components > (y$mean + y$std), "greater",
                     ifelse(sum.of.components < (y$mean - y$std), "smaller", "equal")
                     )
      
      lack.of.closure <- ifelse((!all(eq_comp_th %in% eq_comp) & test %in% "greater") | (all(eq_comp_th %in% eq_comp) & !test %in% "equal"), 1, 0)
      lack.of.closure <- ifelse(is.na(test), NA, lack.of.closure)
      
    } #  if(X[X$variable.diagram %in% v, ]$n.areas >= 7) 
    
    if(!X[X$variable.diagram %in% v, ]$n.areas >= 7) {
      eq_comp <- NULL
      sum.of.components <- NA
      test <- NA
      lack.of.closure <- NA 
    } # if(!X[X$variable.diagram %in% v, ]$n.areas >= 7) 
  
    component_closure <- rbind(
      component_closure,
      data.frame(
        Biome = b,
        variable.diagram = v,
        mean = y$mean,
        std = y$std,
        equation = eq_th,
        # theoritical.components = paste(eq_comp_th, collapse = "; "),
        components.with.sufficient.data = paste(eq_comp, collapse = "; "),
        sum.of.components,
        test,
        lack.of.closure
      )
    )
  } # for( i in 1:nrow(components_maping)) 
  
} # for(b %in% unique(ForC_biome_averages$Biome))


# save ####

write.csv(component_closure, file = "numbers_and_facts/C_cycle_closure.csv", row.names = F)
