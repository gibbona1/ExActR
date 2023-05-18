library(shiny)
library(leaflet)
library(sf)

shinyApp(
  ui = fluidPage(
    column(5,
           h5("Opening Map"),
           leafletOutput("plot1")
    ),
    column(2,
           h5(""), br(),
           actionButton("extent", "Generate Extent Account")
    ),
    column(5,
           h5("Closing Map"),
           leafletOutput("plot2")
    )
  ),
  server = function(input, output) {
    # Read shapefiles
    sf1 <- st_read("Hazelwood/hazelwood_CLC2000.shp", quiet=TRUE)
    sf2 <- st_read("Hazelwood/hazelwood_CLC2018.shp", quiet=TRUE)
    
    habitat_palette1 <- colorFactor(
        palette = "viridis",
        domain = unique(sf1$CODE_00)
      )
    
    habitat_palette2 <- colorFactor(
      palette = "viridis",
      domain = unique(sf1$CODE_18)
    )
    
    # Render the first plot
    output$plot1 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sf1,
                    fillColor = habitat_palette1(sf1$CODE_00),
                    fillOpacity = 0.7,
                    color = "#b2aeae", #boundary colour, need to use hex color codes.
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addLegend(pal = habitat_palette1,
                  values = sf1$CODE_00, 
                  position = "bottomleft", 
                  title = "Code <br>")
    })
    
    # Render the second plot
    output$plot2 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sf2,
                    fillColor = habitat_palette2(sf2$CODE_18),
                    fillOpacity = 0.7,
                    color = "#b2aeae", #boundary colour, need to use hex color codes.
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addLegend(pal = habitat_palette2,
                  values = sf2$CODE_18, 
                  position = "bottomleft", 
                  title = "Code <br>")
    })
  }
)
