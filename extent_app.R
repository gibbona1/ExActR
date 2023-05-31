library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

#need to upload at least .shp, .shx, .dbf, .prj files for each
#so the map knows where to put itself
map_accepts <- c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")

bold_rownames <- function(el) {
  tags$style(paste0("#", el, " td:first-child { font-weight: bold; }"))
}

uifunc <- function() {
  fluidPage(
    fluidRow(
      column(4,
             fileInput("sf1", "Upload Opening Map",
                       accept   = map_accepts,
                       multiple = TRUE),
             tags$style("white-space: pre-wrap;"),
             verbatimTextOutput("sf1_name")
             ),
      column(4, align = "center", br(),
             #actionButton("extent", "Generate Extent")
             ),
      column(4,
             fileInput("sf2", "Upload Closing Map",
                       accept   = map_accepts,
                       multiple = TRUE),
             tags$style("white-space: pre-wrap;"),
             verbatimTextOutput("sf2_name")
             )
    ),
    fluidRow(
      column(6,
             h3("Opening Map"),
             leafletOutput("plot1"),
             uiOutput("map1col")
      ),
      column(6,
             h3("Closing Map"),
             leafletOutput("plot2"),
             uiOutput("map2col")
      )
    ),
    fluidRow(
      h3("Extent table (Ha)"),
      bold_rownames("extentTable"),
      tableOutput("extentTable"),
      h3("Extent table (% of opening)"),
      bold_rownames("extentPercentTable"),      
      tableOutput("extentPercentTable"),
      h3("Ecosystem Type Change Matrix"),
      bold_rownames("extentMatrix"),
      tableOutput("extentMatrix")
    )
  )
}

server <- function(input, output) {

  #function to read the .shp file and project to the desired coordinate system
  setup_read_sf <- function(shpdf) {
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in seq_len(nrow(shpdf))){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)

    tmp_file1 <- paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/")
    map       <- st_read(tmp_file1, quiet=TRUE)
    map1      <- st_transform(map, "EPSG:4326")
    return(map1)
  }

  #to avoid errors, if map intersections return NULLs, just return zero
  blank_zero <- function(x) {
    if(length(x) == 0)
      return(0)
    return(x)
  }

  #this gets the aggregate changes in each group
  #(start and end areas, and amount increased, decreased, changed)
  change_area <- function(sf1, sf2, grp) {
    sf1_sub <- filter(sf1, sf1[[input$map1_sel_col]] == grp)
    sf2_sub <- filter(sf2, sf2[[input$map2_sel_col]] == grp)

    int_area <- sf1_sub %>%
      st_intersection(sf2_sub) %>%
      st_area() %>% as.numeric() %>% blank_zero() %>% sum()

    opening_A <- sf1_sub %>% st_area() %>%
      as.numeric() %>% blank_zero() %>% sum()
    closing_A <- sf2_sub %>% st_area() %>%
      as.numeric() %>% blank_zero() %>% sum()
    
    res <- list(
      "opening"    = opening_A / 10 ^ 4,
      "increase"   = (closing_A - int_area) / 10 ^ 4,
      "decrease"   = -1*(opening_A - int_area) / 10 ^ 4,
      "net change" = -1*(opening_A - closing_A) / 10 ^ 4,
      "closing"    = closing_A / 10 ^ 4)
    return(res)
  }

  #maps are very similar so just pass to a function the data and which column to colour by
  gen_map_leaflet <- function(data, column) {
    pl <- leaflet() %>%
      addTiles() %>%
      addPolygons(data         = data,
                  fillColor    = plotCols()(data[[column]]),
                  fillOpacity  = 0.7,
                  color        = "#b2aeae", #boundary colour, need to use hex color codes.
                  weight       = 0.5,
                  smoothFactor = 0.2) %>%
      addLegend(pal      = plotCols(),
                values   = data[[column]],
                position = "bottomleft",
                title    = "Code <br>")
    return(pl)
  }

  #extract items from a list and suppress some warnings e.g. NAs, geometry issue, for now
  lazy_unlist <- function(x) suppressWarnings(unlist(x))

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

  #UI with dropdown for grouping of the datasets e.g. habitat codes
  output$map1col <- renderUI({
    req(input$sf1)
    selectInput("map1_sel_col", "Select Grouping Column", choices = names(sf1()))
  })

  output$map2col <- renderUI({
    req(input$sf2)
    selectInput("map2_sel_col", "Select Grouping Column", choices = names(sf2()))
  })
  
  output$sf1_name <- renderText({
    req(input$sf1)
    disp_name <- strsplit(input$sf1$name, "\\.")
    return(disp_name[[1]][1])
  })
  
  output$sf2_name <- renderText({
    req(input$sf2)
    disp_name <- strsplit(input$sf2$name, "\\.")
    return(disp_name[[1]][1])
  })

  #if the sf data or selectInput are not ready, wait
  plot1Wait <- reactive({is.null(input$sf1) | is.null(input$map1_sel_col)})

  plot2Wait <- reactive({is.null(input$sf2) | is.null(input$map2_sel_col)})

  #groups to iterate over for extent account
  codeGroups <- reactive({
    cols <- c()
    if(!plot1Wait())
      cols <- union(cols, sf1()[[input$map1_sel_col]])
    if(!plot2Wait())
      cols <- union(cols, sf2()[[input$map2_sel_col]])
    return(cols)
  })

  #common colour palette between the two maps for easier visualisation of groups
  plotCols <- reactive({
    colorFactor(
      palette = "viridis",
      domain  = codeGroups()
    )
  })

  # Render the first plot
  output$plot1 <- renderLeaflet({
    if(plot1Wait())
      return(leaflet() %>% setView(lng = 0, lat = 0, zoom = 2))

    return(gen_map_leaflet(sf1(), input$map1_sel_col))
  })

  # Render the second plot
  output$plot2 <- renderLeaflet({
    if(plot2Wait())
      return(leaflet() %>% setView(lng = 10, lat = 10, zoom = 2))

    return(gen_map_leaflet(sf2(), input$map2_sel_col))
  })

  extentData <- reactive({
    req(input$map1_sel_col, input$map2_sel_col)

    #get the opening, closing, changes for each code, extract to list of vectors
    extent_mat <- sapply(codeGroups(), function(grp) {
      lazy_unlist(change_area(sf1(), sf2(), grp))
      })

    return(as.data.frame(extent_mat))
  })

  output$extentTable <- renderTable({
    if(is.null(input$sf1) | is.null(input$sf2))
      return(NULL)
    extent_df       <- extentData()
    extent_df$Total <- rowSums(extent_df)
    return(extent_df)
  }, rownames = TRUE)

  #the change portions can be represented as a percent of the opening
  output$extentPercentTable <- renderTable({
    if(is.null(input$sf1) | is.null(input$sf2))
      return(NULL)
    extent_df  <- extentData()
    percent_df <- as.data.frame(sapply(extent_df, function(x) x[2:4] / x[1]))
    rownames(percent_df) <- rownames(extent_df)[2:4]
    return(percent_df)
  }, rownames = TRUE)

  #A bit more complicated. This now has a matrix where:
  ##diagonals: amounts unchanged between opening and closing in that group
  ##off-diagonals: amount changed from type in the row to type in the column
  extentMat <- reactive({
    if(plot1Wait() | plot2Wait())
      return(NULL)

    df1 <- sf1()
    df2 <- sf2()

    code_grps <- codeGroups()

    cross_area <- function(grp1, grp2) {
      df1_sub <- filter(df1, df1[[input$map1_sel_col]] == grp1)
      df2_sub <- filter(df2, df2[[input$map2_sel_col]] == grp2)
      df1_sub %>%
        st_intersection(df2_sub) %>%
        st_area() %>%
        as.numeric() %>%
        blank_zero() %>%
        sum()
    }

    cross_mat <- do.call(rbind, lapply(code_grps, function(grp1) {
      sapply(code_grps, function(grp2) lazy_unlist(cross_area(grp1, grp2)))
    }))

    rownames(cross_mat) <- colnames(cross_mat) <- code_grps

    cross_mat <- cross_mat / 10^4

    cross_df  <- as.data.frame(cross_mat)

    cross_df$openings      <- rowSums(cross_df)
    cross_df["closings", ] <- colSums(cross_df)

    return(cross_df)
  })

  output$extentMatrix <- renderTable({
    extentMat()
  }, rownames = TRUE)
}

shinyApp(uifunc(), server)
