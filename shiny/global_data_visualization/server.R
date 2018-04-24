
# Sun Apr 22 21:02:32 2018


# Aplication behavior #########


library( leaflet )
library( RColorBrewer )
library( scales )
library( reshape2 )
library( plyr )
library( ggplot2 )




function( input, output, session ) {
  
  ## Interactive Map ###########
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles( providers$CartoDB.Positron ) %>%  #OpenMapSurfer.Grayscale
      setView( lng = 0, lat = 0, zoom = 3 ) %>%
      setMaxBounds( lng1 = -240, lat1 = -90, lng2 = 240, lat2 = 90 )
  })
  
  
  # subset by stand age
  # dd <- subset( dd, stand.age >= input$dates[ 1 ] & stand.age <= input$dates[ 2 ] )
  
  
  # Histogram
  output$histCentile <- renderPlot({
    assign( 'x', eval( parse( text = paste( 'subset( dd, variable.name == \'', input$color, '\' )$meanvar', sep = '' ) ) ) )
    if ( length( na.omit( x ) ) > 0 )
    {
      hist( x,
          main = '',
          xlab = input$color,
          col =  '#66CC00', # '#00DD00',
          border = 'white')
    } else
    {
      return()
    }
  })
  
  
  # color varies depending on the variable selected
  observe({
    output$color <- renderUI( selectInput(  inputId = "color", label = h4( "Variable" ), 
                                            selectize = TRUE, choices = varLst[[ input$varType ]][[ input$measType ]], 
                                            selected = 'GPP_C' ) )
  })
  
  
  # point sioze and color vary according to user input
  observe({
    eval( parse( text = paste( 'ddk <- subset( dd, variable.name == \'', input$color, '\' )', sep = '' ) ) )
    
    colorData <- ddk$meanvar
    if ( length( colorData ) > 0 )
    {
      pal <- colorBin( "viridis", colorData, 7, pretty = FALSE, na.color = NA )
      if ( sum( !is.na( colorData ) ) == 1 )
      {
        pal <- colorBin( "viridis", na.omit( colorData ) * c( 0.99, 1.01 ) , 7, pretty = FALSE, na.color = NA )
      }
    }
    
    radius <- ddk$meanvar
    if ( length( radius ) > 0 & length( colorData ) > 0 )
    {
      ddk$radius <- 100000 + radius / max( radius, na.rm = TRUE ) * 300000 # isa # 150000
      ddk$colorData <- colorData
      ddk$palcolorData <- pal( ddk$colorData )
      
      ddk <- na.omit( ddk[ c( 'lat', 'lon', 'radius', 'site', 'colorData', 'palcolorData' ) ] )
      leafletProxy( "map", data = ddk ) %>%
        clearShapes() %>%
        addCircles( ~lon, ~lat, radius = ~radius, layerId = ~site,
                    stroke = FALSE, fillOpacity = 0.4, fillColor = ~palcolorData ) %>%
        addLegend( "bottomleft", pal = pal, values = colorData, title = input$color,
                   layerId = "colorLegend" )
    } else
    {
      leafletProxy( "map", data = NULL ) %>%
        clearShapes()
      return()
    }
  })
  
  
  # click over a point in the map raises a call to showSitecodePopup
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showSitecodePopup( event$id, event$lat, event$lng )
    })
  })
  
  
  # popup info about the target location for the selected variable
  showSitecodePopup <- function( site, lat, lng ) {
    selectedSite <- dd[ dd$site == site & dd$variable.name == input$color, ]
    lonstr <- paste( sprintf( '%0.2f', abs( selectedSite$lon ) ), ifelse( selectedSite$lon < 0, 'W', 'E' ), sep = '' )
    latstr <- paste( sprintf( '%0.2f', abs( selectedSite$lat ) ), ifelse( selectedSite$lat < 0, 'S', 'N' ), sep = '' )
    content <- as.character(
      tagList(
        tags$h5( "Site:", selectedSite$site ),
        tags$body( HTML( sprintf( paste( input$color, ": %0.4f", sep = '' ), selectedSite$meanvar ) ) ), 
        tags$br(),
        tags$body( HTML( paste( lonstr, latstr, sep = ', ' ) ) ), 
        tags$br()
        ) 
      )
    leafletProxy( "map" ) %>% addPopups( lng, lat, content, layerId = site )
  }
  
  
  # go from table to a specific location -- not working right now
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      site <- input$goto$site
      lat <- input$goto$lat
      lng <- input$goto$lng
      showSitecodePopup(site, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
}


