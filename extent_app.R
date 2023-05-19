library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

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
    ),
    fluidRow(
      h3(HTML("Extent table (m<sup>2</sup>)")),
      tableOutput("extentTable"),
      h3(HTML("Extent table (% of opening)")),
      tableOutput("extentPercentTable")
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
      map1      <- st_transform(map, "EPSG:4326")
      return(map1)
    }
    
    change_area <- function(sf1, sf2, grp){
      blank_zero <- function(x){
        if(length(x)==0)
          return(0)
        return(x)
      }
      
      int_area <- sf1 %>% filter(CODE_00 == grp) %>%
        st_intersection(sf2 %>% filter(CODE_18 == grp)) %>%
        st_area() %>% as.numeric() %>% blank_zero() %>% sum()
      
      opening_A <- sf1 %>% 
        filter(CODE_00 == grp) %>% st_area() %>% 
        as.numeric() %>% blank_zero() %>% sum()
      closing_A <- sf2 %>% 
        filter(CODE_18 == grp) %>% st_area() %>% 
        as.numeric() %>% blank_zero() %>% sum()
      
      res <- list(
        "opening"    = opening_A/10^4,
        "increase"   = (closing_A - int_area)/10^4,
        "decrease"   = -1*(opening_A - int_area)/10^4,
        "net change" = -1*(opening_A - closing_A)/10^4,
        "closing"    = closing_A/10^4)
      return(res)
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
    
    extentData <- reactive({
      df1 <- sf1()
      df2 <- sf2()
      
      code_grps <- union(df1$CODE_00, df2$CODE_18)
      
      extent_mat <- sapply(code_grps, function(grp) unlist(change_area(df1, df2, grp)))
      
      extent_df  <- as.data.frame(extent_mat)
      
      return(extent_df)
    })
    
    output$extentTable <- renderTable({
      if(is.null(input$sf1) | is.null(input$sf2))
        return(NULL)
      extent_df <- extentData()
      extent_df$Total <- rowSums(extent_df)
      return(extent_df)
    }, rownames = TRUE)
    
    output$extentPercentTable <- renderTable({
      if(is.null(input$sf1) | is.null(input$sf2))
        return(NULL)
      extent_df       <- extentData()
      extent_df[2:4,] <- sapply(extent_df, function(x) x[2:4]/x[1])
      return(extent_df[2:4,])
    }, rownames = TRUE)
  }
)
