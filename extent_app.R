library(shiny)
library(leaflet)
library(sf)

map_accepts <- c('.shp','.dbf','.sbn','.sbx','.shx',".prj")

shinyApp(
  ui = fluidPage(
    fluidRow(
      column(4, fileInput("sf1", "Upload Opening Map", accept=map_accepts, multiple = TRUE)),
      column(4, align='center', br(), actionButton("extent", "Generate Extent")),
      column(4, fileInput("sf2", "Upload Closing Map", accept=map_accepts, multiple = TRUE))
    ),
    fluidRow(
      column(6,
             h3("Opening Map"),
             leafletOutput("plot1")
      ),
      column(6,
             h3("Closing Map"),
             leafletOutput("plot2")
      )
    )
  ),
  server <- function(input, output) {
    
    setup_read_sf <- function(shpdf){
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)
      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)
      
      tmp_file1 <- paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/")
      map       <- st_read(tmp_file1, quiet=TRUE)
      return(map)
    }
    # Read shapefiles
    sf1 <- reactive({
      #tmp_file1 <- "Hazelwood/hazelwood_CLC2000.shp"
      #if(is.null(input$sf1)){
        #showNotification(HTML(paste0("Using temporary file ", tags$b(tmp_file1), ".")),
        #                 duration = NULL, type = "message")        
        #return(read_sf(tmp_file1, quiet=TRUE))
      #  showNotification(HTML("No file uploaded, leaving blank."),
      #                   duration = NULL, type = "message")
      #  return(NULL)
      #}
      req(input$sf1)
      return(setup_read_sf(input$sf1))
    })
    
    sf2 <- reactive({
      req(input$sf2)
      return(setup_read_sf(input$sf2))
    })
    
    # Render the first plot
    output$plot1 <- renderLeaflet({
      if(is.null(input$sf1)){
        return(leaflet() %>%
                 #addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
                 setView(lng = 0, lat = 0, zoom = 2))
      }
      
      habitat_palette1 <- colorFactor(
        palette = "viridis",
        domain = unique(sf1()$CODE_00)
      )
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sf1(),
                    fillColor = habitat_palette1(sf1()$CODE_00),
                    fillOpacity = 0.7,
                    color = "#b2aeae", #boundary colour, need to use hex color codes.
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addLegend(pal = habitat_palette1,
                  values = sf1()$CODE_00, 
                  position = "bottomleft", 
                  title = "Code <br>")
    })
    
    # Render the second plot
    output$plot2 <- renderLeaflet({
      if(is.null(input$sf2)){
        return(leaflet() %>%
                 #addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
                 setView(lng = 10, lat = 10, zoom = 2))
      }
      
      habitat_palette2 <- colorFactor(
        palette = "viridis",
        domain = unique(sf2()$CODE_18)
      )
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(data = sf2(),
                    fillColor = habitat_palette2(sf2()$CODE_18),
                    fillOpacity = 0.7,
                    color = "#b2aeae", #boundary colour, need to use hex color codes.
                    weight = 0.5, 
                    smoothFactor = 0.2) %>%
        addLegend(pal = habitat_palette2,
                  values = sf2()$CODE_18, 
                  position = "bottomleft", 
                  title = "Code <br>")
    })
  }
)
