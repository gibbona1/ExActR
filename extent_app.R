library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

#need to upload at least .shp, .shx, .dbf, .prj files for each
#so the map knows where to put itself
map_accepts <- c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
#TODO: multiple time points:
## make combined data with index for each time point
## plots: land use histograms (nominal, percent)
##        difference in land use time 1-2, time 2-3, etc (nominal, percent)

crs_data <- suppressWarnings(rgdal::make_EPSG(file))
crs_list <- crs_data$code
names(crs_list) <- paste(paste0("EPSG:", crs_list), crs_data$note, sep = " - ")
default_crs <- 4326

lookup_file <- "habitat_codes.csv"

bold_rownames <- function(el) {
  tags$style(paste0("#", el, " td:first-child { font-weight: bold; }"))
}

copy_button <- function(id, format, formatLab){
  return(actionButton(paste("copy", id, format, sep = "_"), 
                      paste("Copy as", formatLab), 
                      onclick = sprintf("copytable('%s','%s')", id, format)))
}

copy_button_group <- function(id){
  div(
    copy_button(id, "text",  "Text"),
    copy_button(id, "html",  "HTML"),
    copy_button(id, "latex", "LaTeX")
  )
}

plot_copy_group <- function(id){
  wellPanel(
    plotOutput(id),
    actionButton(paste0("copy_", id), "Copy", icon = icon("copy"),
                 onclick = sprintf("copyplot('%s')", id)),
    downloadButton(paste0("download_", id))
  )
}

sfInput <- function(name, lab){
  div(
    fileInput(name, lab, accept = map_accepts, multiple = TRUE),
    tags$style("white-space: pre-wrap;"),
    verbatimTextOutput(paste(name, "name", sep = "_")),
    class = "sfdiv-item"
    )
}

sfMapOutput <- function(name, id){
  column(6,
         h3(paste(name, "Map")),
         leafletOutput(paste0("plot", id)),
         uiOutput(paste0("map", id, "col"))
  )
}

extentObj <- function(id){
  wellPanel(
    bold_rownames(id),
    tableOutput(id),
    uiOutput(paste0("copybttn_", id))
  )
}

ctd   <- function(el, align = "left") tags$td(align = align, el)
table <- tags$table
tr    <- tags$tr
li    <- tags$li
ul    <- tags$ul

uifunc <- function() {
  fluidPage(
    useShinyjs(),
    titlePanel("Extent Account Creator"),
    tabsetPanel(
      {tabPanel("Extent Account",
      fluidRow(
        tags$style(
          HTML("
            .sfdiv-container {
              display: flex;
              flex-direction: row;
              justify-content: space-between;
            }
            
            .sfdiv-item:not(:last-child) {
              text-align: left;
            }
            
            .sfdiv-item:last-child {
              text-align: right;
            }
          ")
        ),
        uiOutput("sf_group"),
      ),
      fluidRow(
        column(6,
          selectizeInput("sel_crs", "Select CRS", choices = NULL, width = "100%"),
        ),
        column(6,
          actionButton("addTimePoint", label = "Add Time Point", icon = icon("plus-circle"), style = 'margin-top:25px'),
          actionButton("delTimePoint", label = "Delete Time Point", icon = icon("minus-circle"), style = 'margin-top:25px'),
          align = "right"
        ),
      ),
      fluidRow(
        uiOutput("sf_map_group")
      ),
      fluidRow(
        table(style = "width: 100%",
         tr(ctd(fileInput("lookupFile", "Upload Lookup table", accept = ".csv")),
            ctd(verbatimTextOutput("lookup_file")),
            ctd(align = "right", 
                checkboxInput("use_codes", "Use code lookup", value = FALSE))),
         )),
      fluidRow(
        column(12,
        actionButton("gen_extent", "Generate/Refresh Extent", class = "btn-primary"),        
        align = "center")
      ),
      fluidRow(
        tags$script(src = "copytable.js"),
        h3("Extent table (Ha)"),
        extentObj("extentTable"),
        h3("Extent table (% of opening)"),
        extentObj("extentPercentTable"),
        h3("Ecosystem Type Change Matrix"),
        extentObj("extentMatrix"),
        hr(),
        wellPanel(
          style = "background: lightblue;",
          HTML(paste0(tags$b("Note: "), "In the ", 
            tags$em("Ecosystem Type Change Matrix"), ":", br(),
            ul(
              li("The diagonal values are the amounts unchanged for that group."),
              li("Each row is the unchanged areas plus the reduction in area."),
              li("Each column is the unchanged areas plus the additions in area."),
              li("The sum of a row will equal the opening extent."),
              li("The sum of a column will equal the closing extent.")
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

server <- function(input, output, session) {
  
  plot_names <- c("plotComp", "plotStack", "plotMap1", "plotMap2")
  plots <- reactiveValues()
  for(plt in plot_names)
    plots[[plt]]  <- NULL
  
  mapIds <- reactiveVal(c(1, 2))
  sfRaws <- reactiveValues()
  sfs    <- reactiveValues()
  
  updateSelectizeInput(session, "sel_crs", choices = crs_list, 
                       selected = default_crs, server = TRUE)

  #function to read the .shp file and project to the desired coordinate system
  setup_read_sf <- function(shpdf) {
    updir <- dirname(shpdf$datapath[1])
    for(i in seq_len(nrow(shpdf))){
      renamed_file <- file.path(updir, shpdf$name[i])
      if(!file.exists(renamed_file))
        file.rename(shpdf$datapath[i], renamed_file)
    }
    tmp_file1 <- file.path(updir, shpdf$name[endsWith(shpdf$name, ".shp")])
    return(st_read(tmp_file1, quiet = TRUE))
  }

  #to avoid errors, if map intersections return NULLs, just return zero
  blank_zero <- function(x) {
    if(length(x) == 0)
      return(0)
    return(as.numeric(x))
  }
  
  clean_sum <- function(x) x %>% st_area() %>% blank_zero() %>% sum()

  #this gets the aggregate changes in each group
  #(start and end areas, and amount increased, decreased, changed)
  change_area <- function(grp, ext_mat){
    opening_A   <- ext_mat[grp, "openings"]
    closing_A   <- ext_mat["closings", grp]
    unchanged_A <- ext_mat[grp, grp]
    c(
      "opening"    = opening_A,
      "increase"   = sum(ext_mat[, grp]) - closing_A - unchanged_A,
      "decrease"   = -1*(sum(ext_mat[grp, ]) - opening_A - unchanged_A),
      "net change" = closing_A - opening_A,
      "closing"    = closing_A)
  }

  #maps are very similar so use a function the data and which column to colour by
  gen_map_leaflet <- function(data, column) {
    leaflet(options = leafletOptions(crs = leafletCRS(code = input$sel_crs))) %>%
      addTiles() %>%
      addPolygons(data         = data %>% st_transform(default_crs),
                  fillColor    = plotCols()(code_lookup(data[[column]])),
                  fillOpacity  = 0.7,
                  color        = "#b2aeae", #boundary colour, use hex color codes.
                  weight       = 0.5,
                  smoothFactor = 0.2) %>%
      addLegend(pal      = plotCols(),
                values   = code_lookup(data[[column]]),
                position = "bottomleft",
                title    = "Code <br>")
  }

  #extract from a list and suppress  warnings e.g. NAs, geometry issue, for now
  lazy_unlist <- function(x) suppressWarnings(unlist(x))
  
  get_sf_name <- function(x){
    if(is.null(x))
      return(" ")
    disp_name <- strsplit(x, "\\.")
    return(disp_name[[1]][1])
  }
  
  #selectInput for what column of sf data to colour in the map and for accounts
  renderMapSel <- function(id){
    output[[paste0("map", id, "col")]] <- renderUI({
      selectInput(sprintf("map%s_sel_col", id), "Select Grouping Column", 
                  choices = names(sfRaws[[id]]))
      })
    return()
  }
  
  renderSfName <- function(id, sf_id){
    output[[paste0("sf", id, "_name")]] <- renderText({get_sf_name(input[[sf_id]]$name)})
    return()
  }
  
  map_oc <- function(idx, inp){
    n <- length(inp)
    x <- ""
    if(inp[idx] == 1)
      x <- "Opening"
    else if(inp[idx] == inp[n])
      x <- "Closing"
    return(x)
  }
  
  observeEvent(input$addTimePoint, {
    n <- length(mapIds())
    mapIds(c(mapIds(), n + 1))
  })
  
  observeEvent(input$delTimePoint, {
    n <- length(mapIds())
    if(n == 2){
      showNotification("must have at least two time points", type = "warning")
      return()
    }
    
    mapIds(mapIds()[-n])
  })
  
  output$sf_group <- renderUI({
    mapTitle <- function(idx, inp) 
      paste("Upload", map_oc(idx, inp), "Map", paste0("(", idx, ")"))
    
    sfdiv <- function(...) div(..., class = "sfdiv-container")
    do.call(sfdiv, 
            purrr::map(
              mapIds(),
              ~ sfInput(paste0("sf", .x), mapTitle(.x, mapIds()))
              )
            )
  })
  
  output$sf_map_group <- renderUI({
    do.call(div, 
            purrr::map(
              mapIds(),
              ~ sfMapOutput(map_oc(.x, mapIds()), .x)
            )
    )
  })

  # Read shapefiles and render other objects
  observe({
    for(id in mapIds()){
      id    <- paste0(id)
      sf_id <- paste0("sf", id)
      if(is.null(input[[sf_id]]))
        next
      sfRaws[[id]] <- setup_read_sf(input[[sf_id]])
      sfs[[id]]    <- sfRaws[[paste0(id)]] %>% st_transform(as.numeric(input$sel_crs))
      #UI with dropdown for grouping of the datasets e.g. habitat codes
      renderMapSel(paste0(id))
      renderSfName(id, sf_id)
    }
  })

  lookupData <- reactive({
    ifelse(is.null(input$lookupFile), lookup_file, input$lookupFile$datapath) %>%
      read.csv
  })
  
  output$lookup_file <- renderText({
    return(ifelse(is.null(input$lookupFile), lookup_file, input$lookupFile$name))
  })

  #if the sf data or selectInput are not ready, wait
  plot_wait <- function(id) 
    return(is.null(input[[paste0("sf", id)]]) | is.null(input[[paste0("map", id, "_sel_col")]]))
  
  code_lookup <- function(vec){
    if(input$use_codes){
      df <- lookupData()
      check_codedf <- function(x)
        ifelse(x %in% df[, 1], paste(x, "-", df[df[, 1] == x, 2]), x)
      return(sapply(vec, check_codedf))
    } else {
      return(vec %>% as.character())
    }
  }

  #groups to iterate over for extent account
  codeGroups <- reactive({
    cols <- c()
    for(i in mapIds()){
      m_col <- input[[paste0("map", i, "_sel_col")]]
      if(!plot_wait(i))
        cols  <- union(cols, code_lookup(sfs[[paste0(i)]][[m_col]]))
    }
    return(cols)
  })

  #common colour palette between the two maps for easier visualisation of groups
  plotCols <- reactive({
    colorFactor(
      palette = "viridis",
      domain  = codeGroups()
    )
  })

  # Render plots
  renderLeafletPlot <- function(id){
    output[[paste0("plot", id)]] <- renderLeaflet({
      if(plot_wait(id))
        return(leaflet() %>% setView(lng = 10*id, lat = 0, zoom = 2))
      else
        return(gen_map_leaflet(sfs[[paste0(id)]], input[[paste0("map", id, "_sel_col")]]))
    })
    return()
  }
  
  observe({
    for(id in mapIds()){
      renderLeafletPlot(id)
    }
  })

  extentData <- reactive({
    do.call(req, lapply(mapIds(), function(i) input[[paste0("map", i, "_sel_col")]]))
    
    #get opening, closing, changes for each code  from extent change matrix
    return(as.data.frame(sapply(codeGroups(), change_area, extentMat())))
  })
  
  changeData <- reactive({
    extent_df <- extentData()
    change_df <- data.frame(id     = colnames(extent_df),
                            open   = as.numeric(extent_df["opening", ]),
                            close  = as.numeric(extent_df["closing", ]),
                            change = as.numeric(extent_df["net change", ]))
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
    if(any(sapply(mapIds(), function(i) is.null(input[[paste0("sf", i)]]))))
      return(NULL)
    extent_df  <- extentData()
    df <- as.data.frame(sapply(extent_df, function(x) x[2:4] / x[1]))
    rownames(df) <- rownames(extent_df)[2:4]
    #replace NAs and Infs with 0
    df <- apply(df, 2, function(x) replace(x, is.na(x) | is.infinite(x), 0))
    return(df)
  }, rownames = TRUE)

  #A bit more complicated. This now has a matrix where:
  ##diagonals: amounts unchanged between opening and closing in that group
  ##off-diagonals: amount changed from type in the row to type in the column
  extentMat <- reactive({
    if(any(sapply(mapIds(), plot_wait)))
      return(NULL)

    df1 <- sfs[["1"]]
    df2 <- sfs[["2"]]

    code_grps <- codeGroups()

    cross_area <- function(grp1, grp2) {
      df1_sub <- filter(df1, (df1[[input$map1_sel_col]] %>% code_lookup) == grp1)
      df2_sub <- filter(df2, (df2[[input$map2_sel_col]] %>% code_lookup) == grp2)
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
    if(any(sapply(mapIds(), function(i) is.null(input[[paste0("sf", i)]]))))
      return(NULL)
    
    div(
      tags$head(
        tags$script(src = "copyplot.js")
      ),
      tagList(lapply(plot_names, plot_copy_group))
    )
  })
  
  observeEvent(input$gen_extent, {
    render_copybttns <- function(id){
      if(!input$gen_extent)
        return(NULL)
      return(copy_button_group(id))
    }
    coppybttnOutput <- function(tab){
      output[[sprintf("copybttn_%s", tab)]] <- renderUI({render_copybttns(tab)})
      return()
    }
    for(tab in c("extentTable", "extentPercentTable", "extentMatrix")){
      coppybttnOutput(tab)
    }
  })
  
  geom_bar_stack <- function(mapping = NULL)
    geom_bar(mapping, position = "stack", stat = "identity")
  
  output$plotStack <- renderPlot({
    p <- plots$plotStack <- changeData() %>% 
      mutate(id = code_lookup(id)) %>%
      ggplot() + 
      geom_bar_stack(aes(x = "open", y = open, fill = id)) +
      geom_bar_stack(aes(x = "close", y = close, fill = id)) +
      ggtitle("Habitat composition") +
      scale_fill_manual(values = plotCols()(code_lookup(changeData()$id))) +
      ylab("Area (Ha)") +
      xlab("") + theme_classic()
    return(p)
  })
  
  output$plotComp <- renderPlot({
    p <- plots$plotComp <-  changeData() %>% 
      mutate(id = code_lookup(id)) %>%
      ggplot() + 
      geom_bar(aes(x = id, y = change, fill = id), stat = "identity") +
      coord_flip() +
      ggtitle("Ecosystem type net changes") +
      scale_fill_manual(values = plotCols()(code_lookup(changeData()$id))) +
      ylab("Area change (Ha)") +
      xlab("") + theme_classic()
    return(p)
  })
  
  plot_extent <- function(data, col, name){
    ggplot(data, aes(fill = code_lookup(.data[[col]]))) +
      geom_sf(color = NA) +
      labs(title = get_sf_name(name),
           fill  = "Ecosystem Type") + 
      theme_bw() + 
      scale_fill_manual(values = plotCols()(code_lookup(data[[col]]))) +
      coord_sf(crs = as.numeric(input$sel_crs))
  }
  
  observe({
    renderMapPlot <- function(id){
      m_id <- paste0("plotMap", id)
      output[[m_id]] <- renderPlot({
        p <- plots[[m_id]] <- plot_extent(sfs[[paste0(id)]], 
                                          input[[paste0("map", id, "_sel_col")]], 
                                          input[[paste0("sf", id)]]$name)
        return(p)
      })
      return()
    }
    for(id in mapIds())
      renderMapPlot(id)
  })
  
  render_download_bttn <- function(id){
    downloadHandler(
      filename = function() paste0(id, '-', Sys.Date(), '.png'),
      content  = function(con) ggsave(con, plots[[id]])
    )
  }
  
  observe({
    downloadPlotOutput <- function(plt){
      output[[paste0("download_", plt)]]  <- render_download_bttn(plt)
      return()
    }
    for(plt in plot_names)
      downloadPlotOutput(plt)
  })
  
  output$habitatExplorer <- renderUI({
    if(any(sapply(mapIds(), function(i) is.null(input[[paste0("sf", i)]]))))
      return(NULL)
    div(
      h3("Opening data"),
      tableOutput("openingExpTable"),
      h3("Closing data"),
      tableOutput("closingExpTable")
    )
  })
  
  get_explore_table <- function(col, df){
    val <- df[, col]
    exp_df <- data.frame(code   = df$id,
                         aream2 = val * 10^4,
                         areaha = val,
                         perc   = val/sum(val))
    colnames(exp_df) <- c("Code", "Area (m<sup>2</sup>)", "Area (Ha)", "% Coverage")
    return(exp_df)
  }
  
  output$openingExpTable <- renderTable({
    return(get_explore_table("open", changeData()))
  }, sanitize.text.function = function(x) x)
  
  output$closingExpTable <- renderTable({
    return(get_explore_table("close", changeData()))
  }, sanitize.text.function = function(x) x)
}

shinyApp(uifunc(), server)
