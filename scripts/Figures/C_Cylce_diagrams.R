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

# Load my.arrows function####
source("scripts/Figures/my.arrows.R")


# Load picture table ####
img <- readPNG(source = "figures/C_cycle_diagrams/Forest paintings/Mature_Forest_TropForC_review.png")


# Load data ####
variables <- read.csv("figures/C_cycle_diagrams/ForC_variables_mapping_for_C_cycle_diagrams.csv")


# Prepare list of variables and their relevant attributes ####

variable.names <- unique(variables$variable.diagram)
variables <- list(NEE = list(variable.type = "flux",
                             variable.name  = expression(bold("NEE")),
                             estimate = -1.9,
                             error = 2.3,
                             n.records = 37, 
                             n.plots = 11,
                             n.areas = 11,
                             coordinates = list(x0 = 3, y0 = 11, x1 = 3, y1 = 10),
                             arr.fill = "grey",
                             text.col = "black",
                             legend.y.just = 0.3,
                             legend.x.just = 0,
                             need.semi.transparent.box = FALSE),
                  GPP = list(variable.type = "flux",
                             variable.name  = expression(bold("GPP")),
                              estimate = 31.6,
                              error = 6.7,
                              n.records = 38, 
                              n.plots = 18,
                              n.areas = 13,
                              coordinates = list(x0 = 0.5, y0 = 10.5, x1 = 0.5, y1 = 9),
                              arr.fill = "grey",
                              text.col = "black",
                              legend.y.just = 0.5,
                              legend.x.just = 0.5,
                              need.semi.transparent.box = FALSE),
                 Rauto_ag = list(variable.type = "flux_no_estimate",
                                 variable.name  = expression(bold("R"[auto_ag])),
                                 estimate = 2, # no estimate
                                 error = "4.1",
                                 n.records = "9", 
                                 n.plots = "8",
                                 n.areas = "5",
                                 coordinates = list(x0 = 1.5, y0 = 9.5, x1 = 4, y1 = 9.5),
                                 arr.fill = "grey",
                                 text.col = "black",
                                 legend.y.just = 0.5,
                                 legend.x.just = 0.5,
                                 need.semi.transparent.box = FALSE),
                 Ra = list(variable.type = "flux",
                           variable.name  = expression(bold("R"[a])),
                           estimate = 21.2,
                           error = 4.1,
                           n.records = 9, 
                           n.plots = 8,
                           n.areas = 5,
                           coordinates = list(x0 = 4.2, y0 = 9.5, x1 = 6, y1 = 9.5),
                           arr.fill = "grey",
                           text.col = "black",
                           legend.y.just = 0.5,
                           legend.x.just = 0.5,
                           need.semi.transparent.box = FALSE),
                 Reco = list(variable.type = "flux",
                             variable.name  = expression(bold("R"[eco])),
                             estimate = 28.3,
                             error = 8.2,
                             n.records = 29, 
                             n.plots = 11,
                             n.areas = 10,
                             coordinates = list(x0 = 7, y0 = 9, x1 = 7, y1 = 10.5),
                             arr.fill = "grey",
                             text.col = "black",
                             legend.y.just = 0.5,
                             legend.x.just = 0.5,
                             need.semi.transparent.box = FALSE),
                 NPP = list(variable.type = "flux",
                            variable.name  = expression(bold("NPP")),
                            estimate = 8.7,
                            error = 3.7,
                            n.records = 50, 
                            n.plots = 32,
                            n.areas = 13,
                            coordinates = list(x0 = 0.5, y0 = 8.9, x1 = 0.5, y1 = 7.9),
                            arr.fill = "grey",
                            text.col = "black",
                            legend.y.just = 0.4,
                            legend.x.just = 0.58,
                            need.semi.transparent.box = FALSE),
                 ANPP = list(variable.type = "flux",
                             variable.name  = expression(bold("ANPP")),
                             estimate = 5.0,
                             error = 2.4,
                             n.records = 57, 
                             n.plots = 31,
                             n.areas = 13,
                             coordinates = list(x0 = 0.8, y0 = 7.9, x1 = 1.6, y1 = 7.2),
                             arr.fill = "grey",
                             text.col = "black",
                             legend.y.just = 0.2,
                             legend.x.just = 0.2,
                             need.semi.transparent.box = TRUE),
                 ANPP_woody = list(variable.type = "flux_no_estimate",
                                   variable.name  = expression(bold("ANPP"[woody])),
                                   estimate = 0.3,
                                   error = "",
                                   n.records = "", 
                                   n.plots = "",
                                   n.areas = "",
                                   coordinates = list(x0 = 1.7, y0 = 7.1, x1 = 2.6, y1 = 6.2),
                                   arr.fill = "grey",
                                   text.col = "black",
                                   legend.y.just = 0.3,
                                   legend.x.just = 0.22,
                                   need.semi.transparent.box = FALSE),
                 ANPP_stem = list(variable.type = "flux",
                                  variable.name  = expression(bold("ANPP"[stem])),
                                  estimate = 2.8,
                                  error = 1.0,
                                  n.records = 141, 
                                  n.plots = 137,
                                  n.areas = 40,
                                  coordinates = list(x0 = 2.7, y0 = 6.1, x1 = 3.6, y1 = 5.2),
                                  arr.fill = "grey",
                                  text.col = "black",
                                  legend.y.just = 0.3,
                                  legend.x.just = 0.3,
                                  need.semi.transparent.box = TRUE),
                 ANPP_foliage = list(variable.type = "flux",
                                     variable.name  = expression(bold("ANPP"[foliage])),
                                     estimate = 3.0,
                                     error = 1.2,
                                     n.records = 43, 
                                     n.plots = 38,
                                     n.areas = 18,
                                     coordinates = list(x0 = 1.4, y0 = 7, x1 = 2, y1 = 4),
                                     arr.fill = "grey",
                                     text.col = "black",
                                     legend.y.just = 0.5,
                                     legend.x.just = 0.8,
                                     need.semi.transparent.box = FALSE),
                 
                 ANPP_branch = list(variable.type = "flux_no_estimate",
                                    variable.name  = expression(bold("ANPP"[branch])),
                                    estimate = 0.1,
                                    error = "",
                                    n.records = "", 
                                    n.plots = "",
                                    n.areas = "",
                                    coordinates = list(x0 = 2.3, y0 = 6, x1 = 2.3, y1 = 4),
                                    arr.fill = "grey",
                                    text.col = "black",
                                    legend.y.just = 0.5,
                                    legend.x.just = 0.5,
                                    need.semi.transparent.box = FALSE),
                 ANPP_folivory = list(variable.type = "flux",
                                      variable.name  = expression(bold("ANPP"[folivory])),
                                      estimate = 0.4,
                                      error = 0.3,
                                      n.records = 10, 
                                      n.plots = 8,
                                      n.areas = 5,
                                      coordinates = list(x0 = 1.8, y0 = 7.3, x1 = 3, y1 = 7.7),
                                      arr.fill = "grey",
                                      text.col = "black",
                                      legend.y.just = 0,
                                      legend.x.just = -1,
                                      need.semi.transparent.box = TRUE),
                 ANPP_folivory_second_arrow = list(variable.type = "flux_simple_arrow",
                                      variable.name  = "",
                                      estimate = "",
                                      error = "",
                                      n.records = "", 
                                      n.plots = "",
                                      n.areas = "",
                                      coordinates = list(x0 = 4, y0 = 8, x1 = 6.5, y1 = 9),
                                      arr.border = "grey60",
                                      text.col = "black",
                                      legend.y.just = 0,
                                      legend.x.just = -1,
                                      need.semi.transparent.box = TRUE),
                 ANNPrepro = list(variable.type = "flux_no_estimate",
                                  variable.name  = expression(bold("ANPP"[repro])),
                                  estimate = 0.1,
                                  error = "",
                                  n.records = "", 
                                  n.plots = "",
                                  n.areas = "",
                                  coordinates = list(x0 = 3.5, y0 = 7.3, x1 = 2.5, y1 = 4),
                                  arr.fill = "grey",
                                  text.col = "black",
                                  legend.y.just = -4.5,
                                  legend.x.just = -0.2,
                                  need.semi.transparent.box = TRUE),
                 ANPP_litterfall = list(variable.type = "flux",
                                        variable.name  = expression(bold("ANPP"[litterfall])),
                                        estimate = 3.6,
                                        error = 1.5,
                                        n.records = 17, 
                                        n.plots = 16,
                                        n.areas = 10,
                                        coordinates = list(x0 = 2.3, y0 = 3.9, x1 = 2.3, y1 = 1),
                                        arr.fill = "grey",
                                        text.col = "black",
                                        legend.y.just = 0.5,
                                        legend.x.just = 0.5,
                                        need.semi.transparent.box = FALSE),
                 BNPP = list(variable.type = "flux",
                             variable.name  = expression(bold("BNPP")),
                             estimate = 3.1,
                             error = 1.9,
                             n.records = 34, 
                             n.plots = 30,
                             n.areas = 12,
                             coordinates = list(x0 = 0.4, y0 = 7.8, x1 = 1.4, y1 = 1),
                             arr.fill = "grey",
                             text.col = "black",
                             legend.y.just = -2,
                             legend.x.just = 1.5,
                             need.semi.transparent.box = FALSE),
                 BNPP_coarse = list(variable.type = "flux_text_only",
                                    variable.name  = expression(bold("BNPP"[coarse])),
                                    estimate = 0.4,
                                    error = 0.2,
                                    n.records = 29, 
                                    n.plots = 27,
                                    n.areas = 10,
                                    coordinates = list(x0 = 0.2, y0 = 4, x1 = 0.2, y1 = 4),
                                    arr.fill = "grey",
                                    text.col = "black",
                                    legend.y.just = 0.5,
                                    legend.x.just = 0.5,
                                    need.semi.transparent.box = FALSE),
                 BNPP_fine = list(variable.type = "flux_text_only",
                                  variable.name  = expression(bold("BNPP"[fine])),
                                  estimate = 2.6,
                                  error = 1.7,
                                  n.records = 34, 
                                  n.plots = 30,
                                  n.areas = 13,
                                  coordinates = list(x0 = 0.3, y0 = 3, x1 = 0.3, y1 = 3),
                                  arr.fill = "grey",
                                  text.col = "black",
                                  legend.y.just = 0.5,
                                  legend.x.just = 0.5,
                                  need.semi.transparent.box = FALSE),
                 Rsoil = list(variable.type = "flux",
                              variable.name  = expression(bold("R"[soil])),
                              estimate = 13.5,
                              error = 3.7,
                              n.records = 24, 
                              n.plots = 19,
                              n.areas = 12,
                              coordinates = list(x0 = 5, y0 = 0.5, x1 = 5, y1 = 2),
                              arr.fill = "grey",
                              text.col = "black",
                              legend.y.just = 0.5,
                              legend.x.just = 0.5,
                              need.semi.transparent.box = FALSE),
                 Rroot = list(variable.type = "flux_no_estimate",
                              variable.name  = expression(bold("R"[root])),
                              estimate = 0.3,
                              error = "",
                              n.records = "", 
                              n.plots = "",
                              n.areas = "",
                              coordinates = list(x0 = 5, y0 = 2.1, x1 = 4.8, y1 = 8.8),
                              arr.fill = "grey",
                              text.col = "black",
                              legend.y.just = +9.5,
                              legend.x.just = 1,
                              need.semi.transparent.box = FALSE),
                 Rroot_second_arrow = list(variable.type = "flux_no_estimate",
                              variable.name  = "",
                              estimate = 0.3,
                              error = "",
                              n.records = "", 
                              n.plots = "",
                              n.areas = "",
                              coordinates = list(x0 = 5, y0 = 2.1, x1 = 7, y1 = 8.8),
                              arr.fill = "grey",
                              text.col = "black",
                              legend.y.just = 0.5,
                              legend.x.just = 0.5,
                              need.semi.transparent.box = FALSE),
                 Woodymortality = list(variable.type = "flux_no_estimate",
                                       variable.name  = expression(bold("woody\nmortality")),
                                       estimate = 0.3,
                                       error = "",
                                       n.records = "", 
                                       n.plots = "",
                                       n.areas = "",
                                       coordinates = list(x0 = 3.8, y0 = 5, x1 = 6, y1 = 2),
                                       arr.border = "black",
                                       text.col = "black",
                                       legend.y.just = 0.2,
                                       legend.x.just = 1.3,
                                       need.semi.transparent.box = FALSE),
                 Rsoil_het = list(variable.type = "flux_simple_arrow",
                                  variable.name  = expression(bold("R"[soil-het])),
                                  estimate = 7.5,
                                  error = 3.4,
                                  n.records = 18, 
                                  n.plots = 16,
                                  n.areas = 9,
                                  coordinates = list(x0 = 6.2, y0 = 2, x1 = 7.2, y1 = 8.8),
                                  arr.border = "grey60",
                                  text.col = "black",
                                  legend.y.just = 1.2,
                                  legend.x.just = 0.2,
                                  need.semi.transparent.box = TRUE),
                 coarse_root_biomass = list(variable.type = "stock_no_estimate",
                                            variable.name  = expression(bold("coarse root biomass")),
                                            estimate = "",
                                            error = "",
                                            n.records = "", 
                                            n.plots = "",
                                            n.areas = "",
                                            coordinates = list(x0 = 8, y0 = 0, x1 = 10.5, y1 = 0.8),
                                            arr.fill = "grey95",
                                            text.col = "black",
                                            legend.y.just = 0.5,
                                            legend.x.just = 0.5,
                                            need.semi.transparent.box = FALSE),
                 fine_root_biomass = list(variable.type = "stock",
                                          variable.name  = expression(bold("fine root biomass")),
                                          estimate = 8.5,
                                          error = 12.2,
                                          n.records = 11, 
                                          n.plots = 8,
                                          n.areas = 8,
                                          coordinates = list(x0 = 8, y0 = 0.85, x1 = 10.5, y1 = 1.65),
                                          arr.fill = "grey95",
                                          text.col = "black",
                                          legend.y.just = 0.5,
                                          legend.x.just = 0.5,
                                          need.semi.transparent.box = FALSE),
                 root_biomass = list(variable.type = "stock",
                                     variable.name  = expression(bold("root biomass")),
                                     estimate = 26.5,
                                     error = 15.6,
                                     n.records = 39, 
                                     n.plots = 22,
                                     n.areas = 12,
                                     coordinates = list(x0 = 10.7, y0 = 0.4, x1 = 12.8, y1 = 1.2),
                                     arr.fill = "grey95",
                                     text.col = "black",
                                     legend.y.just = 0.5,
                                     legend.x.just = 0.5,
                                     need.semi.transparent.box = FALSE),
                 organic_layer = list(variable.type = "stock",
                                      variable.name  = expression(bold("organic layer")),
                                      estimate = 6.8,
                                      error = 8.0,
                                      n.records = 3, 
                                      n.plots = 3,
                                      n.areas = 3,
                                      coordinates = list(x0 = 10, y0 = 2, x1 = 12, y1 = 2.8),
                                      arr.fill = "grey95",
                                      text.col = "black",
                                      legend.y.just = 0.5,
                                      legend.x.just = 0.6,
                                      need.semi.transparent.box = FALSE),
                 down_dead_wood = list(variable.type = "stock_no_estimate",
                                       variable.name  = expression(bold("down dead wood")),
                                       estimate = "",
                                       error = "",
                                       n.records = "", 
                                       n.plots = "",
                                       n.areas = "",
                                       coordinates = list(x0 = 8.5, y0 = 3.2, x1 = 10.5, y1 = 4),
                                       arr.fill = "grey95",
                                       text.col = "black",
                                       legend.y.just = 0.5,
                                       legend.x.just = 0.5,
                                       need.semi.transparent.box = FALSE),
                 standing_dead_wood = list(variable.type = "stock_no_estimate",
                                           variable.name  = expression(bold("standing dead wood")),
                                           estimate = "",
                                           error = "",
                                           n.records = "", 
                                           n.plots = "",
                                           n.areas = "",
                                           coordinates = list(x0 = 8.5, y0 = 4.05, x1 = 10.5, y1 = 4.9),
                                           arr.fill = "grey95",
                                           text.col = "black",
                                           legend.y.just = 0.5,
                                           legend.x.just = 0.5,
                                           need.semi.transparent.box = FALSE),
                 dead_wood = list(variable.type = "stock",
                                  variable.name  = expression(bold("dead wood")),
                                  estimate = 17.6,
                                  error = 12.7,
                                  n.records = 9, 
                                  n.plots = 9,
                                  n.areas = 5,
                                  coordinates = list(x0 = 10.7, y0 = 3.6, x1 = 12.8, y1 = 4.4),
                                  arr.fill = "grey95",
                                  text.col = "black",
                                  legend.y.just = 0.5,
                                  legend.x.just = 0.5,
                                  need.semi.transparent.box = FALSE),
                 total_biomass = list(variable.type = "stock",
                                      variable.name  = expression(bold("total biomass")),
                                      estimate = 166,
                                      error = 14,
                                      n.records = 37, 
                                      n.plots = 13,
                                      n.areas = 7,
                                      coordinates = list(x0 = 12, y0 = 6, x1 = 14, y1 = 6.8),
                                      arr.fill = "grey95",
                                      text.col = "black",
                                      legend.y.just = 0.5,
                                      legend.x.just = 0.5,
                                      need.semi.transparent.box = FALSE),
                 woody_biomass = list(variable.type = "stock",
                                      variable.name  = expression(bold("woody ag biomass")),
                                      estimate = 4.2,
                                      error = 1.3,
                                      n.records = 18, 
                                      n.plots = 14,
                                      n.areas = 5,
                                      coordinates = list(x0 = 8.5, y0 = 6, x1 = 10.5, y1 = 6.8),
                                      arr.fill = "grey95",
                                      text.col = "black",
                                      legend.y.just = 0.5,
                                      legend.x.just = 0.5,
                                      need.semi.transparent.box = FALSE),
                 foliage_biomass = list(variable.type = "stock",
                                        variable.name  = expression(bold("foliage biomass")),
                                        estimate = 42,
                                        error = 1.3,
                                        n.records = 18, 
                                        n.plots = 14,
                                        n.areas = 5,
                                        coordinates = list(x0 = 8.5, y0 = 8.85, x1 = 10.5, y1 = 9.65),
                                        arr.fill = "grey95",
                                        text.col = "black",
                                        legend.y.just = 0.5,
                                        legend.x.just = 0.5,
                                        need.semi.transparent.box = FALSE),
                 aboveground_biommass = list(variable.type = "stock",
                                             variable.name  = expression(bold("aboveground biomass")),
                                             estimate = 133.9,
                                             error = 50.8,
                                             n.records = 203, 
                                             n.plots = 118,
                                             n.areas = 42,
                                             coordinates = list(x0 = 10.7, y0 = 8.4, x1 = 12.8, y1 = 9.2),
                                             arr.fill = "grey95",
                                             text.col = "black",
                                             legend.y.just = 0.5,
                                             legend.x.just = 0.5,
                                             need.semi.transparent.box = FALSE)
     
       )


# Plot the picture ####

op <- par(pin = c(14 * 0.6, 11 * 0.6))

plot(c(0,14), c(0, 11), type = "n", axes = T, xlab = "", ylab = "")
rasterImage(img, 0, 0, 8, 11)


# plot the C cylce values ####

## add segments that link stocks (need to do first so that it is in the back)

### root biomass
segments(x0 = c(10.5, 11, 11), y0 = c(0.2, 0.2, 1.4), x1 = c(11, 11, 10.5) , y1 = c(0.2, 1.4, 1.4), col = "black")

### total biomass
segments(x0 = c(12.8, 13, 13), y0 = c(0.8, 0.8, 8.9), x1 = c(13, 13, 12.8) , y1 = c(0.8, 8.9, 8.9), col = "black")

### dead wood
segments(x0 = c(10.5, 11, 11), y0 = c(3.4, 3.4, 4.6), x1 = c(11, 11, 10.5) , y1 = c(3.4, 4.6, 4.6), col = "black")

### aboveground biomass
segments(x0 = c(10.5, 11, 11), y0 = c(6.2, 6.2, 9.4), x1 = c(11, 11, 10.5) , y1 = c(6.2, 9.4, 9.4), col = "black")


for(v in names(variables)){
  X <- variables[[v]]

  if(X$variable.type %in% c("flux", "flux_no_estimate")) my.arrows(X$coordinates, arr.width = sqrt(abs(X$estimate))/5, arr.border = X$arr.border)
  
  if(X$variable.type == "flux_simple_arrow") Arrows(x0 = X$coordinates$x0, y0 = X$coordinates$y0, x1 = X$coordinates$x1, y1 = X$coordinates$y1, col = X$arr.border, lwd = 3, lty = 5, arr.type = "triangle", arr.width = 0.5)
  
  if(X$variable.type %in% c("stock", "stock_no_estimate")) rect(xleft = X$coordinates$x0, ybottom = X$coordinates$y0, xright = X$coordinates$x1, ytop = X$coordinates$y1, col = "grey", border = X$arr.border)
  
  # if(X$estimate == "")  X.text <- bquote(bold(.(v))) else  X.text <- bquote(atop(.(X$variable.name), .(paste(paste(X$estimate, X$error, sep = "\u00b1"), paste(X$n.records, X$n.plots, X$n.areas, sep = "/") , sep = "\n"))))
  
  if(X$variable.type %in% c("flux_no_estimate", "stock_no_estimate")){
    legend.title <- NULL
    legend.txt <- X$variable.name
  }else{
    legend.title <- X$variable.name
    if(X$estimate == "") legend.txt <-"" else legend.txt <- bquote(atop(.(paste(paste(X$estimate, X$error, sep = "\u00b1"), paste(X$n.records, X$n.plots, X$n.areas, sep = "/") , sep = "\n"))))
  } 
  
  
  x.center <- (X$coordinates$x0 + X$coordinates$x1)/2
  y.center <- (X$coordinates$y0 + X$coordinates$y1)/2
  
  
  if(X$need.semi.transparent.box){
    a <- legend(x = x.center, y = y.center, title = legend.title, legend = legend.txt, cex = 0.5, xjust = X$legend.x.just, yjust = X$legend.y.just, text.col = X$text.col, bty = "n")
    
    # box size reduced by factor 0.75
    a=a$rect
    mid = a$top - 0.5*a$h
    reduction = 0.6
    
    # draw new box
    rect(xleft=a$left, ytop = mid +0.5*reduction*a$h, xright=a$left+a$w, ybottom=mid-0.5*reduction*a$h, col = rgb(1,1,1,0.5), border = NA)
  }
  
  
  # add legend items to new box
  legend(x = x.center, y = y.center, title = legend.title, legend = legend.txt, cex = 0.5, xjust = X$legend.x.just, yjust = X$legend.y.just, text.col = X$text.col, bty = "n")
  
}


rect(xleft = 12, ybottom = 10, xright =14, ytop =11, col = "white")
X.text <-  bquote(atop(bold(legend), .(paste(paste("mean","std", sep = "\u00b1"), paste("n records", "n plots", "n areas", sep = "/") , sep = "\n"))))
legend(x = 13, y = 10.5,  legend = X.text, cex = 0.5, bty = "n",  xjust = 0.5, yjust = 0.3)

par(op)



# 
# op <- par(pin = c(8 * 0.6, 11 * 0.6))
# 
# plot(c(0,8), c(0, 11), type = "n", axes = T, xlab = "", ylab = "")
# 
# 
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 6 , y1 = 6)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 5 , y1 = 8)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 4 , y1 = 9)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 3 , y1 = 8)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 2 , y1 = 6)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 6 , y1 = 6)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# 
# 
# 
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 3 , y1 = 4)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 4 , y1 = 3)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# X$coordinates <- list(x0 = 4, y0 = 6, x1 = 5 , y1 = 4)
# my.arrows(X$coordinates, arr.width = 1, arr.fill = NULL)
# 
# 
# draw.circle(4,6,1)
# 
# par(op)
