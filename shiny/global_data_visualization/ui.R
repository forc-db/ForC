
# Sun Apr 22 21:02:32 2018


# Page controls ###############


library( leaflet )


navbarPage( "ForC", id = "nav",
            tabPanel( "Interactive map",
                      div( class = "outer",
                           tags$head(
                             # Include our custom CSS
                             includeCSS( "styles.css" )
                           ),
                           
                           # leaflet map
                           leafletOutput( outputId = "map", width = "100%", height = "100%" ),
                           
                           
                           # side ponel to allow user to choose among variables
                           # shiny versions prior to 0.11 should use class="modal" instead.
                           if ( usingSimplified )
                           {
                             absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            h3( "Select pattern" ),
                                            
                                            radioButtons( inputId = "varType", label = h4( "Variable type:" ), 
                                                          choices = list( "Flux" = "flux",
                                                                          "Stock" = "stock" ), selected = 'flux' ),
                                            
                                            radioButtons( "measType", h4( "Type of measurement:" ),
                                                          list( "Carbon" = "carbon" ), selected = 'carbon' ),
                                            
                                            uiOutput( 'color' ), 
                                            # selectInput( "color", h4( "Variable" ), choices = varLst ),
                                            # selectInput( "size", "Size", vars, selected = "npp"), # isa
                                            plotOutput("histCentile", height = 200)#,
                                            
                                            # sliderInput("dates",
                                            #             h4( "Stand age:" ),
                                            #             min = 0,  max = 100, value = c( 0, 100 ), step = 1 )
                             )
                           } else
                           {
                             absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 350, height = "auto",
                                            
                                            h3( "Select pattern" ),
                                            
                                            radioButtons( inputId = "varType", label = h4( "Variable type:" ), 
                                                          choices = list( "Flux" = "flux",
                                                                          "Stock" = "stock",
                                                                          "Secondary" = "secondary" ), selected = 'flux' ),
                                            
                                            radioButtons( "measType", h4( "Type of measurement:" ),
                                                          list( "Organic matter" = "orgmat",
                                                                "Carbon" = "carbon" ), selected = 'carbon' ),
                                            
                                            uiOutput( 'color' ), 
                                            # selectInput( "color", h4( "Variable" ), choices = varLst ),
                                            # selectInput( "size", "Size", vars, selected = "npp"), # isa
                                            plotOutput("histCentile", height = 200)#,
                                            
                                            # sliderInput("dates",
                                            #             h4( "Stand age:" ),
                                            #             min = 0,  max = 100, value = c( 0, 100 ), step = 1 )
                             )
                           },
                           
                           
                           
                           # citation
                           tags$div( id = "cite",
                                     'Exploring ', tags$em('ForC: A global database of forest carbon stocks and fluxes'), ' [',
                                     tags$a( href = "https://doi.org/10.1002/ecy.2229", 
                                                     "Anderson-Teixeira et al 2018", 
                                                     target="_blank"), '] '
                           )
                      )
            )
)
