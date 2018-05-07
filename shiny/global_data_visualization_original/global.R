


# Sun Apr 22 21:02:32 2018


# ---Shiny ForC --- 

# Shinny application to explore ForC data [Anderson-Teixeira et al 2018](https://doi.org/10.1002/ecy.2229)
# 
# The application consist of a global map featuring the location of different 
# forest plots adn allows the user to explore the different variables included 
# in the data base using an interactive menu.
# 
# References 
# 
# Anderson‐Teixeira, K.J., M.M.H. Wang, J.C. McGarvey, V. Herrmann, A.J. Tepley,
#     B. Bond‐Lamberty & D.S. LeBaue. 2018. ForC: A global database of forest 
#     carbon stocks and fluxes. Ecology, https://doi.org/10.1002/ecy.2229
#   
# Code developed at the Pacala Lab (PU) by [Isa Martinez Cano](mailto:isamcano@gmail.com) 
# based on Shiny´s gallery SuperZip example (see https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example)


# Data preparation ############

library( plyr )
library( reshape2 )
# library( RCurl )

# Connect to ForC to create a local copy of the data or just load it if a local copy is available
pth <- 'data/ForC/'; dir.create( path = 'data/ForC', showWarnings = FALSE, recursive = TRUE )

usingSimplified <- TRUE # use raw ForC or ForC-simplified

dt <- format( Sys.Date(), '%Y%m' )
if ( usingSimplified )
{
  # update dataset on a monthly basis
  fn <- paste( pth, paste( 'ForC_simplified_', dt, '.rds', sep = '' ), sep = '/' )
  if ( file.exists( fn ) )
  {
    forc <- readRDS( file = fn )
  } else
  {
    forc <- read.csv( 'https://raw.github.com/forc-db/ForC/master/ForC_simplified/ForC_simplified.csv' ) 
    saveRDS( object = forc, file = fn )
  }
  
  # metadata and variables table (we need variable units and type)
  fn <- paste( pth, paste( 'ForC_simplified_metadata_', dt, '.rds', sep = '' ), sep = '/' )
  if ( file.exists( fn ) )
  {
    forc.metadata <- readRDS( file = fn )
  } else
  {
    forc.metadata <- read.csv( 'https://raw.github.com/forc-db/ForC/master/ForC_simplified/ForC_simplified_metadata.csv' ) 
    saveRDS( object = forc.metadata, file = fn )
  }
  
  fn <- c( 'ForC_measurements', 'ForC_variables' )
  fno <- paste( pth, fn, '_', dt, '.rds', sep = '' )
  if ( !all( file.exists( fno ) ) )
  {
    url2forc <- 'https://raw.github.com/forc-db/ForC/master/data/'
    for ( k in  which( !file.exists( fno ) ) )
    {
      forc.data <- read.csv( paste( url2forc, fn[ k ], '.csv', sep = '' ) ) 
      saveRDS( object = forc.data, file = fno[ k ] )
    }
  }
  forc.measurements <- readRDS( file = fno[ 1 ] )
  forc.variables <- readRDS( file = fno[ 2 ] )

  # only a subset of variables from the original data set are present, so we take advantage that all data in
  # the forc_simplified is in units of carbon (it is labelled with the suffix '_C' in the original table)
  varNames <- unique( as.character( forc$variable.name ) )
  forc.variables <- subset( forc.variables, as.character( variable.name ) %in% paste( varNames, '_C', sep = '' ) )
  forc$variable.nameOriginal <- forc$variable.name
  forc$variable.name <- paste( forc$variable.nameOriginal, '_C', sep = '' )
} else
{
  fn <- c( 'ForC_sites', 'ForC_measurements', 'ForC_variables' )
  fno <- paste( pth, fn, '_', dt, '.rds', sep = '' )
  if ( !all( file.exists( fno ) ) )
  {
    url2forc <- 'https://raw.github.com/forc-db/ForC/master/data/'
    for ( k in  which( !file.exists( fno ) ) )
    {
      forc.data <- read.csv( paste( url2forc, fn[ k ], '.csv', sep = '' ) ) 
      saveRDS( object = forc.data, file = fno[ k ] )
    }
  }
  forc.sites <- readRDS( file = fno[ 1 ] )
  forc.measurements <- readRDS( file = fno[ 2 ] )
  forc.variables <- readRDS( file = fno[ 3 ] )
  
  # organize data in a single dataframe
  forc <- merge( forc.measurements, forc.sites, by = "sites.sitename" )
  forc <- merge( forc, forc.variables, by = "variable.name" )
}



dd <-  forc[ c( 'lon', 'lat', 'sites.sitename', 'stand.age', 'variable.name',  'mean' ) ]
names( dd ) <-  c( 'lon', 'lat', 'site', 'stand.age', 'variable.name',  'mean' )

# take the mean when several measurements are available at the same location
dd <-  ddply( dd, .( lon, lat, site, variable.name ), summarize, meanvar = mean( mean )) # without stand age 
# dd <-  ddply( dd, .( lon, lat, site, stand.age, variable.name ), summarize, meanvar = mean( mean )) # including stand age


# extract variable names for different variable types
varNames <- as.list( as.character( forc.variables$variable.name ) )
units <- as.character( forc.variables$units )
varType <- as.character( unique( forc.variables$variable.type ) )

# variable list
varLst <- vector( mode = 'list', length = length( varType ) )
names( varLst ) <- varType
for ( k in 1 : length( varType ) )
{
  varLst[[ k ]] <- vector( mode = 'list', length = 2 )
  names( varLst[[ k ]] ) <- c( 'orgmat', 'carbon' )
  idx <- which( forc.variables$variable.type == varType[ k ] )
  if ( varType[ k ] %in% c( 'secondary', 'covariates' ) )
  {
    ptrn <- c( '.', '.' )
  } else
  {
    ptrn <- c( '_OM$', '_C$' )
  }
  if ( usingSimplified )
  {
    ptrnIdx <- 2
  } else
  {
    ptrnIdx <- 1 : 2
  }
  for ( q in ptrnIdx )
  {
    idxy <- grep( pattern = ptrn[ q ], x = as.character( unlist( varNames[ idx ] ) ) )
    varLst[[ k ]][[ q ]] <- varNames[ idx ][ idxy ]
    names( varLst[[ k ]][[ q ]] ) <- paste( as.character( forc.variables$variable.name[ idx ][ idxy ] ), ' [', units[ idx ][ idxy ], ']: ', 
                                            as.character( forc.variables$description[ idx ][ idxy ] ), sep = '' )
  }
}

# Steps to Publish a shiny application ##########

# https://www.shinyapps.io/admin/#/dashboard
# Step 1  Install rsconnect
# install.packages('rsconnect')

# Step 2  Authorize Account
# rsconnect::setAccountInfo(name='isamcano',
#                           token='token',
#                           secret='secret')

# Step 3  Deploy 
# library(rsconnect)
# rsconnect::deployApp('C:\\Users\\icano\\OneDrive\\Manuscritos\\tropical\\LM3-PPA\\lm3ppa\\ForC')



# sessionInfo() ##############

# note the use of leaflet version 1.1.0; there seems to be a bug with version 2.0.0

# R version 3.4.4 (2018-03-15)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] ggplot2_2.2.1      scales_0.5.0       RColorBrewer_1.1-2 leaflet_1.1.0      reshape2_1.4.3     plyr_1.8.4         shiny_1.0.5        devtools_1.13.5   
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_0.12.16      compiler_3.4.4    pillar_1.2.1      later_0.7.1       viridis_0.5.1     tools_3.4.4       digest_0.6.15     jsonlite_1.5      memoise_1.1.0    
# [10] tibble_1.4.2      gtable_0.2.0      viridisLite_0.3.0 rlang_0.2.0       crosstalk_1.0.0   curl_3.2          yaml_2.1.18       gridExtra_2.3     withr_2.1.2      
# [19] httr_1.3.1        stringr_1.3.0     htmlwidgets_1.2   grid_3.4.4        R6_2.2.2          magrittr_1.5      promises_1.0.1    htmltools_0.3.6   mime_0.5         
# [28] xtable_1.8-2      colorspace_1.3-2  httpuv_1.4.1      stringi_1.1.7     lazyeval_0.2.1    munsell_0.4.3  
