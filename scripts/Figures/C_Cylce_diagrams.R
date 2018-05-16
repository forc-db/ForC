######################################################
# Purpose: Create a C cycle diagram depending on forest type.
# Inputs: - Teixeira's forest type painting
#         - Summary of forests role in global C cycle.csv
#         - my.arrows function (defined in script)
# outputs: 1. png file, saved in figures folder as [Forest_type]_C_cylce_diagram.png
# Developped by: Valentine Herrmann - HerrmannV@si.edu
# R version 3.4.2 (2018-03-13)
######################################################


# Clean environment ####
rm(list = ls())

# Set working directory as ForC main folder ####
setwd(".")

# Load libaries ####
library(png)
library(shape)

# Load data ####
ForC_biome_averages <- read.csv("numbers_and_facts/ForC_variable_averages_per_Biome.csv", stringsAsFactors = F)
# ForC_biome_averages$Biome <- gsub("=", "\u2265",ForC_biome_averages$Biome)
# Encoding(ForC_biome_averages$Biome) <- "UTF-8"

# Load my.arrows function####
source("scripts/Figures/my.arrows.R")


# Load picture table ####
img.Tropical_broadleaf_MATURE <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/tropical_broadleaf_mature.png")
img.Tropical_broadleaf_YOUNG <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/tropical_broadleaf_young.png")
img.Temperate_broadleaf_MATURE <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/temperate_broadleaf_mature.png")
img.Temperate_broadleaf_YOUNG <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/temperate_broadleaf_young.png")
img.Temperate_conifer_MATURE <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/temperate_conifer_mature.png")
img.Temperate_conifer_YOUNG <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/Temperate_conifer_young.png")
img.Boreal_conifer_MATURE <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/boreal_mature.png")
img.Boreal_conifer_YOUNG <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/boreal_young.png")


# Prepare list of variables and their relevant attributes ####

variable.names <- unique(ForC_biome_averages$variable.diagram)
variables <- list(NEE = list(variable.type = "flux", #####
                             variable.name  = expression(bold("NEE")),
                             coordinates = list(x0 = 3, y0 = 11, x1 = 3, y1 = 10),
                             y.adjust = 0,
                             x.adjust = 0,
                             move.text.box = "right",
                             move.text.box.distance = 0.9,
                             need.semi.transparent.box = FALSE),
                  GPP = list(variable.type = "flux",
                             variable.name  = expression(bold("GPP")),
                             coordinates = list(x0 = 0.5, y0 = 10.5, x1 = 0.5, y1 = 9),
                             y.adjust = 0,
                             x.adjust = 0,
                             move.text.box = "left",
                             move.text.box.distance = 0.9,
                             need.semi.transparent.box = FALSE),
                  R_auto_ag = list(variable.type = "flux_no_estimate",
                                   variable.name  = expression(bold("R"[auto_ag])),
                                   coordinates = list(x0 = 1.5, y0 = 9.5, x1 = 4, y1 = 9.5),
                                   y.adjust = "ifelse(is.na(X$mean), -0.3, 0)",
                                   x.adjust = 0,
                                   move.text.box = "bottom",
                                   move.text.box.distance = 0.5,
                                   need.semi.transparent.box = TRUE),
                  R_auto = list(variable.type = "flux",
                                variable.name  = expression(bold("R"[a])),
                                coordinates = list(x0 = 4.2, y0 = 9.5, x1 = 6.5, y1 = 9.5),
                                y.adjust = 0,
                                x.adjust = 0,
                                move.text.box = "top",
                                move.text.box.distance = 0.8,
                                need.semi.transparent.box = FALSE),
                  R_eco = list(variable.type = "flux",
                               variable.name  = expression(bold("R"[eco])),
                               coordinates = list(x0 = 7.2, y0 = 9, x1 = 7.2, y1 = 10.5),
                               y.adjust = 0,
                               x.adjust = 0,
                               move.text.box = "right",
                               move.text.box.distance = 0.9,
                               need.semi.transparent.box = FALSE),
                  NPP = list(variable.type = "flux",
                             variable.name  = expression(bold("NPP")),
                             coordinates = list(x0 = 0.5, y0 = 8.9, x1 = 0.5, y1 = 7.9),
                             y.adjust = 0,
                             x.adjust = 0,
                             move.text.box = "left",
                             move.text.box.distance = 0.8,
                             need.semi.transparent.box = FALSE),
                  BNPP = list(variable.type = "flux",
                              variable.name  = expression(bold("BNPP")),
                              coordinates = list(x0 = 0.4, y0 = 7.8, x1 = 0.2, y1 = 4.5),
                              y.adjust = 0,
                              x.adjust = -0.8,
                              move.text.box = "null",
                              need.semi.transparent.box = FALSE),
                  BNPP_coarse = list(variable.type = "flux",
                                     variable.name  = expression(bold("BNPP"[coarse])),
                                     coordinates = list(x0 = 0.3, y0 = 4.4, x1 = 1.2, y1 = 1.3),
                                     y.adjust = -0.5,
                                     x.adjust = 0.5,
                                     move.text.box = "null",
                                     need.semi.transparent.box = TRUE),
                  BNPP_fine = list(variable.type = "flux",
                                   variable.name  = expression(bold("BNPP"[fine])),
                                   coordinates = list(x0 = 0.1, y0 = 4.4, x1 = 0, y1 = 1.2),
                                   y.adjust = 0,
                                   x.adjust = -0.8,
                                   move.text.box = "null",
                                   need.semi.transparent.box = TRUE),
                  ANPP = list(variable.type = "flux",
                              variable.name  = expression(bold("ANPP")),
                              coordinates = list(x0 = 0.8, y0 = 7.9, x1 = 1.6, y1 = 7.2),
                              y.adjust = 0.6,
                              x.adjust = 0.6,
                              move.text.box = "null",
                              need.semi.transparent.box = TRUE),
                  ANPP_folivory = list(variable.type = "flux",
                                       variable.name  = expression(bold("ANPP"[folivory])),
                                       coordinates = list(x0 = 1.8, y0 = 7.3, x1 = 3, y1 = 8),
                                       y.adjust = 0.6,
                                       x.adjust = 1.2,
                                       move.text.box = "null",
                                       need.semi.transparent.box = TRUE),
                  R_ag_het = list(variable.type = "flux",
                                  variable.name  = expression(bold("R"[ag-het])),
                                  mean = NA,
                                  n.areas = NA,
                                  coordinates = list(x0 = 4.2, y0 = 8.2, x1 = 6.8, y1 = 8.3),
                                  y.adjust = 0.2,
                                  x.adjust = -0.8,
                                  move.text.box = "null",
                                  need.semi.transparent.box = TRUE),
                  ANPP_foliage = list(variable.type = "flux",
                                      variable.name  = expression(bold("ANPP"[foliage])),
                                      coordinates = list(x0 = 1.4, y0 = 7, x1 = 2, y1 = 3),
                                      y.adjust = -1,
                                      x.adjust = -0.1,
                                      move.text.box = "null",
                                      need.semi.transparent.box = TRUE),
                  ANPP_repro_first_arrow = list(variable.type = "flux_arrow_only",
                                    variable.name = "",
                                    mean = "ForC_biome_averages[ForC_biome_averages$Biome %in% b & ForC_biome_averages$variable.diag %in% 'ANPP_repro' ,]$mean",
                                    n.areas = "ForC_biome_averages[ForC_biome_averages$Biome %in% b & ForC_biome_averages$variable.diag %in% 'ANPP_repro' ,]$n.areas",
                                    equation = NA,
                                    coordinates = list(x0 = 1.9, y0 = 7.3, x1 = 3.8, y1 = 7.1),
                                    y.adjust = 0,
                                    x.adjust = 0,
                                    move.text.box = "null",
                                    need.semi.transparent.box = TRUE),
                  ANPP_repro = list(variable.type = "flux_no_estimate",
                                    variable.name  = expression(bold("ANPP"[repro])),
                                    coordinates = list(x0 = 3.8, y0 = 7.1, x1 = 2.5, y1 = 3),
                                    y.adjust = 2,
                                    x.adjust = 1.2,
                                    move.text.box = "null",
                                    need.semi.transparent.box = TRUE),
                  ANPP_woody = list(variable.type = "flux",
                                    variable.name  = expression(bold("ANPP"[woody])),
                                    coordinates = list(x0 = 1.7, y0 = 7.1, x1 = 2.6, y1 = 6.2),
                                    y.adjust = 0,
                                    x.adjust = 0,
                                    move.text.box = "null",
                                    need.semi.transparent.box = TRUE),
                  ANPP_stem = list(variable.type = "flux",
                                   variable.name  = expression(bold("ANPP"[stem])),
                                   coordinates = list(x0 = 2.7, y0 = 6.1, x1 = 4.2, y1 = 4.8),
                                   y.adjust = 0.1,
                                   x.adjust = 0.8,
                                   move.text.box = "null",
                                   need.semi.transparent.box = TRUE),
                  woody.mortality = list(variable.type = "flux",
                                         variable.name  = expression(bold("woody mortality")),
                                         coordinates = list(x0 = 4.4, y0 = 4.6, x1 = 6.8, y1 = 1.8),
                                         y.adjust = -0.2,
                                         x.adjust = -1,
                                         move.text.box = "null",
                                         need.semi.transparent.box = TRUE),
                  ANPP_branch = list(variable.type = "flux",
                                     variable.name  = expression(bold("ANPP"[branch])),
                                     coordinates = list(x0 = 2.3, y0 = 6, x1 = 2.3, y1 =3),
                                     y.adjust = +0.4,
                                     x.adjust = 0,
                                     move.text.box = "null",
                                     need.semi.transparent.box = TRUE),
                  ANPP_litterfall = list(variable.type = "flux",
                                         variable.name  = expression(bold("ANPP"[litterfall])),
                                         coordinates = list(x0 = 2.2, y0 = 2.8, x1 = 2.2, y1 = 1),
                                         y.adjust = -0.8,
                                         x.adjust = 0.8,
                                         move.text.box = "null",
                                         need.semi.transparent.box = FALSE),
                  R_soil = list(variable.type = "flux",
                                variable.name  = expression(bold("R"[soil])),
                                coordinates = list(x0 = 5.9, y0 = 0.8, x1 = 5.9, y1 = 1.9),
                                y.adjust = 0,
                                x.adjust = 0,
                                move.text.box = "bottom",
                                move.text.box.distance = 1, 
                                need.semi.transparent.box = FALSE),
                  R_root = list(variable.type = "flux",
                                variable.name  = expression(bold("R"[root])),
                                coordinates = list(x0 = 5.8, y0 = 2.1, x1 = 5.5, y1 = 8.8),
                                y.adjust = 1,
                                x.adjust = 0,
                                move.text.box = "null",
                                need.semi.transparent.box = TRUE),
                  R_soil_het = list(variable.type = "flux",
                                    variable.name  = expression(bold("R"[soil-het])),
                                    coordinates = list(x0 = 6.1, y0 = 2, x1 = 7, y1 = 8),
                                    y.adjust = 0,
                                    x.adjust = 0,
                                    move.text.box = "null",
                                    need.semi.transparent.box = TRUE),
                  R_cwd = list(variable.type = "flux",
                               variable.name  = expression(bold("R"[cwd])),
                               mean = NA,
                               n.areas = NA,
                               coordinates = list(x0 = 7, y0 = 2, x1 = 7.3, y1 = 7.9),
                               y.adjust = -1.5,
                               x.adjust = 0.3,
                               move.text.box = "null",
                               need.semi.transparent.box = TRUE),
                  R_het = list(variable.type = "flux",
                               variable.name  = expression(bold("R"[het])),
                               mean = NA,
                               n.areas = NA,
                               coordinates = list(x0 = 7.1, y0 = 8.2, x1 = 7.1, y1 = 8.8),
                               y.adjust = 0,
                               x.adjust = 0.2,
                               move.text.box = "null",
                               need.semi.transparent.box = TRUE),
                  biomass_root_coarse = list(variable.type = "stock",
                                             variable.name  = expression(bold("coarse root biomass")),
                                             coordinates = list(x0 = 10.15, y0 = 0.85, x1 = 12.35, y1 = 1.65),
                                             y.adjust = 0,
                                             x.adjust = 0,
                                             move.text.box = "null",
                                             need.semi.transparent.box = FALSE),
                  biomass_root_fine = list(variable.type = "stock",
                                           variable.name  = expression(bold("fine root biomass")),
                                           coordinates = list(x0 = 12.45, y0 = 0.85, x1 = 14.75, y1 = 1.65),
                                           y.adjust = 0,
                                           x.adjust = 0,
                                           move.text.box = "null",
                                           need.semi.transparent.box = FALSE),
                  biomass_root = list(variable.type = "stock",
                                      variable.name  = expression(bold("root biomass")),
                                      coordinates = list(x0 = 11.3, y0 = 1.8, x1 = 14.05, y1 = 2.6),
                                      y.adjust = 0,
                                      x.adjust = 0,
                                      move.text.box = "null",
                                      need.semi.transparent.box = FALSE),
                  organic.layer = list(variable.type = "stock",
                                       variable.name  = expression(bold("organic layer")),
                                       coordinates = list(x0 = 7.75, y0 = 0.85, x1 = 10.05, y1 = 1.65),
                                       y.adjust = 0,
                                       x.adjust = 0,
                                       move.text.box = "null",
                                       need.semi.transparent.box = FALSE),
                  deadwood_down = list(variable.type = "stock",
                                       variable.name  = expression(bold("down dead wood")),
                                       coordinates = list(x0 = 7.75, y0 = 1.8, x1 = 10.05, y1 = 2.6),
                                       y.adjust = 0,
                                       x.adjust = 0,
                                       move.text.box = "null",
                                       need.semi.transparent.box = FALSE),
                  deadwood_standing = list(variable.type = "stock",
                                           variable.name  = expression(bold("standing dead wood")),
                                           coordinates = list(x0 = 7.75, y0 = 4, x1 = 10.05, y1 = 4.8),
                                           y.adjust = 0,
                                           x.adjust = 0,
                                           move.text.box = "null",
                                           need.semi.transparent.box = FALSE),
                  deadwood = list(variable.type = "stock",
                                  variable.name  = expression(bold("dead wood")),
                                  n.areas = 5,
                                  coordinates = list(x0 = 8.9, y0 = 2.9, x1 = 11.65, y1 = 3.7),
                                  y.adjust = 0,
                                  x.adjust = 0,
                                  move.text.box = "null",
                                  need.semi.transparent.box = FALSE),
                  biomass = list(variable.type = "stock",
                                 variable.name  = expression(bold("total biomass")),
                                 coordinates = list(x0 = 12.25, y0 = 5, x1 = 15, y1 = 5.8),
                                 y.adjust = 0,
                                 x.adjust = 0,
                                 move.text.box = "null",
                                 need.semi.transparent.box = FALSE),
                  biomass_ag_woody = list(variable.type = "stock",
                                          variable.name  = expression(bold("woody ag biomass")),
                                          coordinates = list(x0 = 9.25, y0 = 6, x1 = 11.55, y1 = 6.8),
                                          y.adjust = 0,
                                          x.adjust = 0,
                                          move.text.box = "null",
                                          need.semi.transparent.box = FALSE),
                  biomass_foliage = list(variable.type = "stock",
                                         variable.name  = expression(bold("foliage biomass")),
                                         coordinates = list(x0 = 9.25, y0 = 8.85, x1 = 11.55, y1 = 9.65),
                                         y.adjust = 0,
                                         x.adjust = 0,
                                         move.text.box = "null",
                                         need.semi.transparent.box = FALSE),
                  biomass_ag = list(variable.type = "stock",
                                    variable.name  = expression(bold("aboveground biomass")),
                                    coordinates = list(x0 = 10.75, y0 = 7.4, x1 = 13.5, y1 = 8.2),
                                    y.adjust = 0,
                                    x.adjust = 0,
                                    move.text.box = "null",
                                    need.semi.transparent.box = FALSE)
)



# Plot the picture ####

b = "Tropical broadleaf YOUNG"
minimum.number.areas.for.solid.line <- 7
for(b in unique(ForC_biome_averages$Biome)){
  print(b)
  
  img <- get(paste0("img.", gsub(" ", "_", b)))
  
  flux.equations.to.right.at.the.bottom <- NULL # equation.nb <- 0
  stock.equations.to.right.at.the.bottom <- NULL
  
  tiff(file = paste0("figures/C_cycle_diagrams/Diagrams/", b, ".tiff"), width =  2625, height = 2250, units = "px", res = 300)
  
  op <- par(mar = c(1,1,1,1), pin = c(15.3 * 0.53, 11 * 0.53)) #  pty = "s") # 
  # par(xaxs='i', yaxs='i')
  plot(c(-0.8, 14.5), c(0, 11), type = "n", axes = F, xlab = "", ylab = "", main = b)
  # rasterImage(img, 14, 0, 6, 11)
  rasterImage(img, 0, 0, 14, 11)
  # plot the C cylce values ####
  # abline(v = - 0.8)
  # abline(v = c(0, 14))
  # abline(h = c(0, 11))
  ## Put segments that link stocks (need to do first so that it is in the back) ####
  
  ### root biomass
  segments(x0 = c(11, 11, 14.35), y0 = c(1.65, 2.2, 2.2), x1 = c(11, 14.35, 14.35) , y1 = c(2.2, 2.2, 1.65), col = "black")
  
  ### total biomass
  segments(x0 = c(12.45, 12.45, 14, 14), y0 = c(2.6, 4, 4, 7.8), x1 = c(12.45, 14, 14, 12.8) , y1 = c(4, 4, 7.8, 7.8), col = "black")
  
  ### dead wood
  segments(x0 = c(9.5, 10.5, 10.5), y0 = c(2.2, 2.2, 4.4), x1 = c(10.5, 10.5, 9.5) , y1 = c(2.2, 4.4, 4.4), col = "black")
  
  ### aboveground biomass
  segments(x0 = c(11.4, 11.9, 11.9), y0 = c(6.4, 6.4, 9.25), x1 = c(11.9, 11.9, 11.4) , y1 = c(6.4, 9.25, 9.25), col = "black")
  
  ## Draw and fill in the stock and flux variables ####
  for(v in names(variables)){
    
    # GET VARIABLE INFO
    
    X <- ForC_biome_averages[ForC_biome_averages$variable.diagram %in% v & ForC_biome_averages$Biome %in% b,]
    X.draw <- variables[[v]]
    
    if(nrow(X) == 0) {
      X <- X.draw
      X$mean <- eval(parse(text = X.draw$mean))
      X$n.areas <- eval(parse(text = X.draw$n.areas))
    }
    
    if(all(is.na(X))) {
      X$variable.type <- X.draw$variable.type
      X[, c( "n.records", "n.plots", "n.area")] <- 0
    }
    
    variable.type <- X$variable.type
    
    if(is.na(X$mean)) variable.type <- paste0(variable.type, "_no_estimate")
    # if(v %in% c("BNPP_coarse", "BNPP_fine")) variable.type <- "flux_text_only"
    
    # For young forest, when the flux is a function of age, replace X$mean by the predicted value for forests at age 50.
    
    if(!is.null(X$equation)) {
      if(!is.na(X$equation)) {
        equation.to.use <- X$equation
        equation.to.use <- gsub("(?<=\u00B1)(.*)(\\+)", "", equation.to.use, perl = T)
        equation.to.use <-  gsub("\u00B1([^\u00B1]*)$", "", equation.to.use, perl = T)
        equation.to.use <- gsub("\u00B1", "+", equation.to.use)
        equation.to.use <- gsub("\u00D7", "\\*", equation.to.use)
        equation.to.use <- gsub("age", "50", equation.to.use)
        
        X$mean <- eval(parse(text = equation.to.use))
      }
    }
    
    # DRAW ARROWS AND SOTCK RECTANGLES
    
    if(variable.type %in% c("flux", "flux_arrow_only")) my.arrows(X.draw$coordinates, arr.width = sqrt(abs(X$mean))/5, lty = ifelse(X$n.areas >= minimum.number.areas.for.solid.line, 1 , 2))
    
    if(variable.type %in% c("flux_no_estimate", "flux_arrow_only_no_estimate")) Arrows(x0 = X.draw$coordinates$x0, y0 = X.draw$coordinates$y0, x1 = X.draw$coordinates$x1, y1 = X.draw$coordinates$y1, col = "black", lty = 2, arr.lwd = 1, lwd = 1, arr.type = "triangle", arr.length = 0.2, arr.width = 0.2)
    
    if(variable.type %in% c("stock", "stock_no_estimate")) rect(xleft = X.draw$coordinates$x0, ybottom = X.draw$coordinates$y0, xright = X.draw$coordinates$x1, ytop = X.draw$coordinates$y1, col = "grey95", border = X.draw$arr.border, lty = ifelse(X$n.areas >= minimum.number.areas.for.solid.line, 1 , 2))
    
    
    # DRAW TEXT
    
    ## GET TEXT BOX COORDINATES + TEXT 
    
    x.center <- (X.draw$coordinates$x0 + X.draw$coordinates$x1)/2 + X.draw$x.adjust
    y.center <- (X.draw$coordinates$y0 + X.draw$coordinates$y1)/2 + eval(parse(text = X.draw$y.adjust))
    
    if(variable.type %in% c("flux_no_estimate", "stock_no_estimate")){
      
      xleft= x.center - 0.5
      xright= x.center + 0.5
      ybottom = y.center - 0.2
      ytop = y.center + 0.2
      
      text.title <-  X.draw$variable.name
      text.main <- NULL
      
      # if(X.draw$need.semi.transparent.box){
      #   rect(xleft= x.center - 0.5, ytop = y.center + 0.2, xright= x.center + 0.5, ybottom = y.center- 0.2, col = rgb(1,1,1,0.5), border = NA)
      # }
      # 
      # text(x = x.center, y = y.center, labels = X.draw$variable.name, cex = 0.5) 
      # 
      
    }
    
    
    if(!variable.type %in% c("flux_no_estimate", "stock_no_estimate", "flux_arrow_only", "flux_arrow_only_no_estimate")) {
      
      need.to.adjust <- FALSE
      need.to.remove.equation <- FALSE
      
      if(!X.draw$move.text.box %in% "null") {
        
        
        arr.width = sqrt(abs(X$mean))/5
        
        xleft= x.center - ifelse(!is.na(X$equation), 0.5, 0.5)
        xright= x.center + ifelse(!is.na(X$equation), 0.5, 0.5)
        ybottom = y.center - 0.4
        ytop = y.center + 0.4
        
        horizontal.arrow <- X.draw$coordinates$y0 ==   X.draw$coordinates$y1
        
        x.width <- xright-xleft
        y.height <- ytop-ybottom
        
        need.to.adjust <- ifelse((horizontal.arrow & (y.height > arr.width)) |  (!horizontal.arrow & (x.width > arr.width)), TRUE, FALSE)
        
        if(need.to.adjust & !is.na(X$equation)){
          need.to.remove.equation <- TRUE
          flux.equations.to.right.at.the.bottom <- c(flux.equations.to.right.at.the.bottom, X$equation) # equation.nb <- equation.nb +1
          
          need.to.adjust <- ifelse((horizontal.arrow & (y.height > arr.width)) |  (!horizontal.arrow & (x.width - 0.2 > arr.width)), TRUE, FALSE)
        }
        
        X.draw$need.semi.transparent.box <- ifelse(need.to.adjust, TRUE,  X.draw$need.semi.transparent.box)
        
      }
      
      if(X.draw$move.text.box %in% "null" & !is.na(X$equation) & !variable.type %in% "stock") {
        
        
        arr.width = sqrt(abs(X$mean))/5
        
        xleft= x.center - ifelse(!is.na(X$equation), 0.5, 0.3)
        xright= x.center + ifelse(!is.na(X$equation), 0.5, 0.3)
        ybottom = y.center - 0.4
        ytop = y.center + 0.4
        
        horizontal.arrow <- X.draw$coordinates$y0 ==   X.draw$coordinates$y1
        
        x.width <- xright-xleft
        y.height <- ytop-ybottom
        
        need.to.remove.equation <- ifelse((horizontal.arrow & (y.height > arr.width)) |  (!horizontal.arrow & (x.width > arr.width)), TRUE, FALSE)
        
        if(need.to.remove.equation) flux.equations.to.right.at.the.bottom <- c(flux.equations.to.right.at.the.bottom, X$equation) #equation.nb <- equation.nb +1 
      }
      
      if(!is.na(X$equation) & variable.type %in% "stock") {
        
        need.to.remove.equation <- TRUE
        stock.equations.to.right.at.the.bottom <- c(stock.equations.to.right.at.the.bottom, X$equation)
      }
      
      
      if(need.to.adjust) {
        
        if (X.draw$move.text.box == "top") y.center <- y.center + X.draw$move.text.box.distance
        if (X.draw$move.text.box == "bottom") y.center <- y.center - X.draw$move.text.box.distance
        if (X.draw$move.text.box == "right") x.center <- x.center + X.draw$move.text.box.distance
        if (X.draw$move.text.box == "left") x.center <- x.center - X.draw$move.text.box.distance
        
      }
      
      xleft= x.center - ifelse(!is.na(X$equation) | !need.to.remove.equation, 0.5, 0.2)
      xright= x.center + ifelse(!is.na(X$equation) | !need.to.remove.equation, 0.5, 0.2)
      ybottom = y.center - 0.4
      ytop = y.center + 0.4
      
        
      text.title <-  X.draw$variable.name
      
      if(is.na(X$equation)){
        text.main <- c(paste(X$mean, X$std, sep = "\u00b1"), paste(X$n.records, X$n.plots, X$n.areas, sep = "/"))
        if(is.na(X$mean)) text.main <- ""
      }
      
      if(!is.na(X$equation)){
        if(need.to.remove.equation) {
          if(grepl("flux", variable.type)) {
            text.main <- c(paste0("eq (", length(flux.equations.to.right.at.the.bottom), ")"), paste(X$n.records, X$n.plots, X$n.areas, sep = "/"))
          mtext.text <-  paste0("eq (", length(flux.equations.to.right.at.the.bottom), "): ", X$equation)
          }
          
          if(grepl("stock", variable.type)) {
            text.main <- c(paste0("eq (", length(stock.equations.to.right.at.the.bottom), ")"), paste(X$n.records, X$n.plots, X$n.areas, sep = "/"))
            mtext.text <-  paste0("eq (", length(stock.equations.to.right.at.the.bottom), "): ", X$equation)
          }
          
        }
        
        if(!need.to.remove.equation) {
          text.main <- c(X$equation, paste(X$n.records, X$n.plots, X$n.areas, sep = "/"))
        }
        
      } 
    }
    
    if(!variable.type %in% c("flux_arrow_only_no_estimate",  "flux_arrow_only")) {
      
      if(X.draw$need.semi.transparent.box){
        rect(xleft, ytop,  xright, ybottom, col = rgb(1,1,1,0.5), border = NA)
      }
      
      text(x = x.center, y = c(y.center, y.center + 0.2, y.center - 0.2)[1:length(c(text.title, text.main))], labels = c(text.main[1], text.title, ifelse(length(text.main) > 1, text.main[2], "")), cex = 0.5)
      
      # if(need.to.remove.equation){
      #   mtext(side = 1, text = mtext.text, line = - (0.5 + equation.nb * 0.5), adj = 0.03, cex = 0.5)
      # }
    }
    
  }
  
  # add equations if any
  if(length(flux.equations.to.right.at.the.bottom > 0)) {
      mtext(side = 1, text = paste0("eq (", 1:length(flux.equations.to.right.at.the.bottom), "): ", flux.equations.to.right.at.the.bottom), line = - (c(length(flux.equations.to.right.at.the.bottom):1)-1) * 0.5, adj = 0.03, cex = 0.5)
  }
  
  if(length(stock.equations.to.right.at.the.bottom > 0)) {
    mtext(side = 1, text = paste0("eq (", 1:length(stock.equations.to.right.at.the.bottom), "): ", stock.equations.to.right.at.the.bottom), line = - (c(length(stock.equations.to.right.at.the.bottom):1)-1) * 0.5, adj = 0.97, cex = 0.5)
  }
  
  
  
  # add legend
  if(grepl("YOUNG", b)) legend.txt <-  c("mean\u00b1std OR intercept\u00b1se + age \u00D7 slope\u00b1se",  paste("n records", "n plots", "n areas", sep = "/"))
  if(!grepl("YOUNG", b)) legend.txt <-  c("mean\u00b1std",  paste("n records", "n plots", "n areas", sep = "/"))
  
  legend(x = 13, y = 10.5, title = expression(bold("Variable name")), legend = legend.txt, cex = 0.5,  xjust = 0.5, yjust = 0.3)
  
  par(op)
  
  dev.off()
  
}



