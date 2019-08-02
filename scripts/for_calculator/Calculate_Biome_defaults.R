######################################################
# Purpose: For Calculator,  update biome_defaults.csv from ForC (see https://github.com/ebimodeling/ghgvc/issues/121)
# Instructions copied over from krista's email:
# There’s one issue (issue #121) that I’m hoping I could get your help on, and wanted to check how long you think it would take to do this. I don’t want to spend your or my time on this until I’m confident that we’re actually going to have the web app working, but if it would be super fast for you (I think it would be) and if you’re able, it may be worth trying to do before you go to France so that we can keep up momentum with Casey. We basically just need to extract/average data from ForC according to the biome categories that we’ve defined. We’ve already done this with an older version of ForC, so it should just require modifying some existing script(s). I’d like to make it so that the script can update the calculator file automatically whenever we update ForC. That integration will really benefit both ForC and the calculator.
#
# Developped by: Valentine Herrmann - HerrmannV@si.edu in Arpil 2018
#  R version 3.6.0 (2019-04-26)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####

# set parameters ####
varibales_of_interests <- list("biomass_ag" = c(Calculator_name = "OM_ag"), 
                            "biomass_root" = c(Calculator_name = "OM_root"),
                            "deadwood" = c(Calculator_name = "OM_wood"),
                            "NEE" = c(Calculator_name = "F_CO2"),
                            "organic.layer" = c(Calculator_name = "OM_litter")
                            )

Calculator_Biome_to_ForC <- list("T1" = list(Koeppen = c("Af"),
                                            dominant.life.form = c("woody", "Forest"),
                                            dominant.veg = c("2TEB", "2TDB", "2TB")),
                                
                                "T2" = list(Koeppen = c("Am"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TEB", "2TDB", "2TB")),
                                
                                "T3" = list(Koeppen = c("As", "Aw", "Bsh"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TEB", "2TDB", "2TB",
                                                            "2TM", "2TEN")),
                                
                                "T4" = list(Koeppen = c("Cfa", "Cwa", "Cwb"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TEB", "2TDB", "2TB")),
                                
                                "T5" = list(Koeppen = c("Af", "Am", "Cfa", "Cwa", "Cwb"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TM", "2TEN")),
                                
                                "T6" = list(Koeppen = c("Bsk", "Bwk", "Cas"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TEB", "2TDB", "2TB")),
                                
                                "T7" = list(Koeppen = c("Cfb", "Cfc", "Csb"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TM", "2TEN")),
                                
                                "T8" = list(Koeppen = c("Cfb", "Cfc", "Csb"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TEB", "2TDB", "2TB")),
                                
                                "T9" = list(Koeppen = c("Dfa", "Dfb", "Dsa", "Dsb", "Dwa", "Dwb"),
                                           dominant.life.form = c("woody", "Forest"),
                                           dominant.veg = c("2TDB", "2TM")),
                                
                                "T10" = list(Koeppen =  c("Dfa", "Dfb", "Dsa", "Dsb", "Dwa", "Dwb"),
                                            dominant.life.form = c("woody", "Forest"),
                                            dominant.veg = c("2TEN", "2TDN", "2TN")),
                                
                                "T11" = list(Koeppen =  c("Dfc", "Dfd", "Dsc", "Dsd", "Dwc", "Dwd"),
                                            dominant.life.form = c("woody", "Forest"),
                                            dominant.veg = c("2TDB", "2TM")),
                                
                                "T12" = list(Koeppen = c("Dfc", "Dfd", "Dsc", "Dsd", "Dwc", "Dwd"),
                                            dominant.life.form = c("woody", "Forest"),
                                            dominant.veg = c("2TEN")),
                                
                                "T13" = list(Koeppen = c("Dfc", "Dfd", "Dsc", "Dsd", "Dwc", "Dwd"),
                                            dominant.life.form = c("woody", "Forest"),
                                            dominant.veg = c("2TDN")),
                                
                                "S1" = list(Koeppen = c("Af", "Am", "As", "Aw", "Bsh", "Cwa", "Cwb"),
                                           dominant.life.form = c("woody+grass", " Savanna"),
                                           dominant.veg = c("2SHRUB", "2TB", "2TD", "2TDB", "2TDN", "2TE", "2TEB", "2TEN", "2TM", "2TREE")) # there will be a problem here
)

ForC_to_Calculator_Biome <- lapply(Calculator_Biome_to_ForC, expand.grid)
ForC_to_Calculator_Biome <- lapply(ForC_to_Calculator_Biome, function(X) apply(X, 1, paste, collapse = "_"))

ForC_to_Calculator_Biome <- data.frame(unlist(mapply(function(x, y) rep(x, each = y), x = names(ForC_to_Calculator_Biome), y = sapply(ForC_to_Calculator_Biome, length))),
                                       row.names = unlist(ForC_to_Calculator_Biome))

# Load data ####
ForC_simplified <- read.csv("ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}
my_na.omit <- function(x) { return(x[!my_is.na(x)])}

# Prepare data ####

## Filter out managed, disturbed and no hisotry info sites
ForC_simplified <- ForC_simplified[ForC_simplified$managed %in% 0 & ForC_simplified$disturbed %in% 0 & ForC_simplified$history.no.info %in%0, ]

## Make stand.age a numeric variable
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)

## Filter out young plots
ForC_simplified <- ForC_simplified[ForC_simplified$stand.age >=50, ]

## Filter out the variables we want
ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% names(varibales_of_interests), ]

## convert to Biomass C (from C))
ForC_simplified$mean_OM <- ForC_simplified$mean / 0.47


## convert NEE from kg to kmol
ForC_simplified$mean_OM <- ifelse(ForC_simplified$variable.name %in%  "NEE", ForC_simplified$mean * 22.722366761722, ForC_simplified$mean)

## rename variables
ForC_simplified$variable.name_calculator <- unlist(varibales_of_interests[ForC_simplified$variable.name])

## give the calculator Biome

ForC_simplified$calculatior_Biome <- ForC_to_Calculator_Biome[paste(ForC_simplified$Koeppen, ForC_simplified$dominant.life.form, ForC_simplified$dominant.veg, sep = "_"),]


# calculate Biome defaults ####

## first average within a plot

Data_to_use <- unique(ForC_simplified[, c("variable.name_calculator", "sites.sitename", "plot.name", "calculatior_Biome")])
Data_to_use$mean_OM <- NA
         
for(i in 1:nrow(Data_to_use)) {
  
  X <- ForC_simplified[ForC_simplified$variable.name_calculator %in%Data_to_use$variable.name_calculator[i] &
                         ForC_simplified$sites.sitename %in%Data_to_use$sites.sitename[i] &
                         ForC_simplified$plot.name %in%Data_to_use$plot.name[i] &
                         ForC_simplified$calculatior_Biome %in%Data_to_use$calculatior_Biome[i], ]
  
  Data_to_use$mean_OM[i] <- mean(X$mean_OM, na.rm = T)
}

## then group by Tropical, Subtropical and Continental for deadwood and litter
Data_to_use$calculatior_Biome  <- as.character(Data_to_use$calculatior_Biome )
Data_to_use$calculatior_Biome <- ifelse(Data_to_use$calculatior_Biome %in% c("T1", "T2", "T3") & Data_to_use$variable.name_calculator %in% c("OM_wood", "OM_litter"), "Tropical", 
                                        ifelse(Data_to_use$calculatior_Biome %in% paste0("T", 4:8) & Data_to_use$variable.name_calculator %in% c("OM_wood", "OM_litter"), "Subtropical", 
                                               ifelse(Data_to_use$calculatior_Biome %in% paste0("T",9:13) & Data_to_use$variable.name_calculator %in% c("OM_wood", "OM_litter"), "Continental", Data_to_use$calculatior_Biom)))


Data_to_use$calculatior_Biome <- factor(Data_to_use$calculatior_Biome, levels = c(paste0("T", 1:13), "S1", unique(Data_to_use$calculatior_Biome )[!unique(Data_to_use$calculatior_Biome ) %in%  c(paste0("T", 1:13), "S1")]))
## now average per Biome 

biome_defaults <- tapply(Data_to_use$mean_OM, list(Data_to_use$variable.name_calculator, Data_to_use$calculatior_Biome), mean, na.rm = T)
biome_defaults <- biome_defaults[, c(paste0("T", 1:13), "S1", colnames(biome_defaults)[!colnames(biome_defaults) %in%  c(paste0("T", 1:13), "S1")])]

biome_defaults_n <- tapply(Data_to_use$mean_OM, list(Data_to_use$variable.name_calculator, Data_to_use$calculatior_Biome), length)
biome_defaults_n <- biome_defaults_n[, colnames(biome_defaults)]

biome_defaults_sd <- tapply(Data_to_use$mean_OM, list(Data_to_use$variable.name_calculator, Data_to_use$calculatior_Biome), sd)
biome_defaults_sd <- biome_defaults_sd[,colnames(biome_defaults)]

# save ####
write.csv(biome_defaults, file = "for_calculator/biome_defaults.csv")
write.csv(biome_defaults_n, file = "for_calculator/biome_defaults_sample_size.csv")
write.csv(biome_defaults_sd, file = "for_calculator/biome_defaults_sd.csv")
write.csv(Data_to_use, file = "for_calculator/data_used_to_calculate_biome_defaults.csv", row.names = F)


