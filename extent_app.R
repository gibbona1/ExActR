library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

#need to upload at least .shp, .shx, .dbf, .prj files for each
#so the map knows where to put itself
map_accepts <- c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")

code_df <- read.csv("habitat_codes.csv")

bold_rownames <- function(el) {
  tags$style(paste0("#", el, " td:first-child { font-weight: bold; }"))
}

copy_button <- function(id, format, formatLab){
  return(actionButton(paste("copy", id, format, sep = "_"), 
                      paste("Copy as", formatLab), 
                      onclick = paste0("copytable('", id, "','", format, "')")))
}

copy_button_group <- function(id){
  div(
    copy_button(id, "text",  "Text"),
    copy_button(id, "html",  "HTML"),
    copy_button(id, "latex", "LaTeX")
  )
}

uifunc <- function() {
  fluidPage(
    useShinyjs(),
    titlePanel("Extent Account Creator"),
    tabsetPanel(
      {tabPanel("Extent Account",
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
        ),
      ),
      fluidRow(
        column(12,
        actionButton("gen_extent", "Generate/Refresh Extent", class = "btn-primary"),
        checkboxInput("use_codes", "Use habitat code lookup", value = FALSE),
        align = "center")
      ),
      fluidRow(
        tags$script(src = "copytable.js"),
        h3("Extent table (Ha)"),
        bold_rownames("extentTable"),
        wellPanel(
          tableOutput("extentTable"),
          copy_button_group("extentTable")
        ),
        h3("Extent table (% of opening)"),
        bold_rownames("extentPercentTable"), 
        wellPanel(
          tableOutput("extentPercentTable"),
          copy_button_group("extentPercentTable")
        ),
        h3("Ecosystem Type Change Matrix"),
        bold_rownames("extentMatrix"),
        wellPanel(
          tableOutput("extentMatrix"),
          copy_button_group("extentMatrix")
        ),
        hr(),
        wellPanel(
          style = "background: lightblue;",
          HTML(paste0(tags$b("Note: "), "In the ", 
            tags$em("Ecosystem Type Change Matrix"), ":", br(),
            tags$ul(
              tags$li("The diagonal values are the amounts unchanged for that group."),
              tags$li("Each row is the unchanged areas plus the reduction in area."),
              tags$li("Each column is the unchanged areas plus the additions in area"),
              tags$li("The sum of a row will equal the opening extent."),
              tags$li("The sum of a column will equal the closing extent.")
            )
          ))
        )
      )
      )},
      {tabPanel("Composition Plots",
                uiOutput("extentPlots"))},
      {tabPanel("Habitat Explorer",
                uiOutput("habitatExplorer"))}
    )
  )
}

server <- function(input, output) {

  #function to read the .shp file and project to the desired coordinate system
  setup_read_sf <- function(shpdf) {
    updir <- dirname(shpdf$datapath[1])
    for(i in seq_len(nrow(shpdf))){
      file.rename(shpdf$datapath[i], file.path(updir, shpdf$name[i]))
    }

    tmp_file1 <- file.path(updir, shpdf$name[grep(pattern="*.shp$", shpdf$name)])
    return(st_read(tmp_file1, quiet=TRUE) %>% st_transform("EPSG:4326"))
  }

  #to avoid errors, if map intersections return NULLs, just return zero
  blank_zero <- function(x) {
    if(length(x) == 0)
      return(0)
    return(x)
  }
  
  clean_sum <- function(x) x %>% st_area() %>% as.numeric() %>% blank_zero() %>% sum()

  #this gets the aggregate changes in each group
  #(start and end areas, and amount increased, decreased, changed)
  change_area <- function(grp, sf1, sf2) {
    sf1_sub <- filter(sf1, sf1[[input$map1_sel_col]] == grp)
    sf2_sub <- filter(sf2, sf2[[input$map2_sel_col]] == grp)

    int_area  <- st_intersection(sf1_sub, sf2_sub) %>% clean_sum()
    opening_A <- sf1_sub %>% clean_sum()
    closing_A <- sf2_sub %>% clean_sum()
    
    res <- list(
      "opening"    = opening_A / 10 ^ 4,
      "increase"   = (closing_A - int_area) / 10 ^ 4,
      "decrease"   = -1*(opening_A - int_area) / 10 ^ 4,
      "net change" = -1*(opening_A - closing_A) / 10 ^ 4,
      "closing"    = closing_A / 10 ^ 4)
    return(lazy_unlist(res))
  }

  #maps are very similar so just pass to a function the data and which column to colour by
  gen_map_leaflet <- function(data, column) {
    pl <- leaflet() %>%
      addTiles() %>%
      addPolygons(data         = data,
                  fillColor    = plotCols()(grouping_col(data[[column]])),
                  fillOpacity  = 0.7,
                  color        = "#b2aeae", #boundary colour, need to use hex color codes.
                  weight       = 0.5,
                  smoothFactor = 0.2) %>%
      addLegend(pal      = plotCols(),
                values   = grouping_col(data[[column]]),
                position = "bottomleft",
                title    = "Code <br>")
    return(pl)
  }

  #extract items from a list and suppress some warnings e.g. NAs, geometry issue, for now
  lazy_unlist <- function(x) suppressWarnings(unlist(x))

  # Read shapefiles
  sf1 <- reactive({
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
  
  get_sf_name <- function(x){
    disp_name <- strsplit(x, "\\.")
    return(disp_name[[1]][1])
  }
  
  output$sf1_name <- renderText({
    req(input$sf1)
    return(get_sf_name(input$sf1$name))
  })
  
  output$sf2_name <- renderText({
    req(input$sf2)
    return(get_sf_name(input$sf2$name))
  })

  #if the sf data or selectInput are not ready, wait
  plot1Wait <- reactive({is.null(input$sf1) | is.null(input$map1_sel_col)})

  plot2Wait <- reactive({is.null(input$sf2) | is.null(input$map2_sel_col)})
  
  grouping_col <- function(vec){
    if(input$use_codes){
      check_codedf <- function(x)
        ifelse(x %in% code_df$Code, paste(x, "-", code_df[code_df$Code == x, 2]), x)
      return(sapply(vec, check_codedf))
    } else {
      return(vec)
    }
  }

  #groups to iterate over for extent account
  codeGroups <- reactive({
    cols <- c()
    if(!plot1Wait())
      cols <- union(cols, grouping_col(sf1()[[input$map1_sel_col]]))
    if(!plot2Wait())
      cols <- union(cols, grouping_col(sf2()[[input$map2_sel_col]]))
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
    extent_mat <- suppressWarnings(sapply(codeGroups(), change_area, sf1(), sf2()))

    return(as.data.frame(extent_mat))
  })
  
  changeData <- reactive({
    extent_df <- extentData()
    change_df <- data.frame(id     = colnames(extent_df),
                            open   = as.numeric(extent_df["opening",]),
                            close  = as.numeric(extent_df["closing",]),
                            change = as.numeric(extent_df["net change",]))
    return(change_df)
  })

  output$extentTable <- renderTable({
    req(input$gen_extent)
    if(is.null(input$sf1) | is.null(input$sf2))
      return(NULL)
    extent_df       <- extentData()
    extent_df$Total <- rowSums(extent_df)
    return(extent_df)
  }, rownames = TRUE)

  #the change portions can be represented as a percent of the opening
  output$extentPercentTable <- renderTable({
    req(input$gen_extent)
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
      st_intersection(df1_sub, df2_sub) %>% clean_sum()
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
    req(input$gen_extent)
    extentMat()
  }, rownames = TRUE)
  
  output$extentPlots <- renderUI({
    if(is.null(input$sf1) | is.null(input$sf2))
      return(NULL)
    
    wellPanel(
      plotOutput("plotComp"),
      plotOutput("plotStack"),
      plotOutput("plotMap1"),
      plotOutput("plotMap2")
    )
  })
  
  output$plotComp <- renderPlot({
    ggplot(changeData()) + 
      geom_bar(aes(x = "open", y = open, fill = id), position = "stack", stat="identity") +
      geom_bar(aes(x = "close", y = close, fill = id), position = "stack", stat="identity") +
      ggtitle("Habitat composition") +
      ylab("Area (Ha)") +
      xlab("") + theme_classic()
  })
  
  output$plotStack <- renderPlot({
    ggplot(changeData()) + 
      geom_bar(aes(x = id, y = change, fill = id), stat = "identity") +
      coord_flip() +
      ggtitle("Ecosystem type net changes") +
      ylab("Area change (Ha)") +
      xlab("") + theme_classic()
  })
  
  plot_extent <- function(data, col, name){
    ggplot() +
      geom_sf(data = data, aes(fill=.data[[col]]), color=NA) +
      labs(title = get_sf_name(name),
           fill = "Ecosystem Type") + 
      theme_bw() + 
      coord_sf(crs = "EPSG:4326")
  }
  
  output$plotMap1 <- renderPlot({
    return(plot_extent(sf1(), input$map1_sel_col, input$sf1$name))
  })
  
  output$plotMap2 <- renderPlot({
    return(plot_extent(sf2(), input$map2_sel_col, input$sf2$name))
  })
  
  output$habitatExplorer <- renderUI({
    if(is.null(input$sf1) | is.null(input$sf2))
      return(NULL)
    div(
      h3("Opening data"),
      tableOutput("openingExpTable"),
      h3("Closing data"),
      tableOutput("closingExpTable")
    )
  })
  
  get_explore_table <- function(col){
    df  <- changeData()
    val <- df[, col]
    exp_df <- data.frame(code   = df$id,
                         aream2 = val * 10^4,
                         areaha = val,
                         perc   = val/sum(val))
    colnames(exp_df) <- c("Code", "Area (m<sup>2</sup>)", "Area (Ha)", "% Coverage")
    return(exp_df)
  }
  
  output$openingExpTable <- renderTable({
    return(get_explore_table("open"))
  }, sanitize.text.function = function(x) x)
  
  output$closingExpTable <- renderTable({
    return(get_explore_table("close"))
  }, sanitize.text.function = function(x) x)
}

shinyApp(uifunc(), server)
