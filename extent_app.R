library(shiny)
library(shinyjs)
library(shinyBS)
library(colourpicker)
library(shinycssloaders)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

#need to upload at least .shp, .shx, .dbf, .prj files for each
#so the map knows where to put itself
map_accepts <- c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
#TODO: 
## make maps take up close to 50% width
## select CRS width
## maps should have toggle ability for codes - mapview?
## dashboard sidebar, header with info etc
## nicer UI e.g. https://github.com/Appsilon/shiny.semantic
## choose colour palette, theme (bootstrap) button status etc
## unit tests
## informative errors

crs_data <- rgdal::make_EPSG()
crs_list <- crs_data$code
names(crs_list) <- paste(paste0("EPSG:", crs_list), crs_data$note, sep = " - ")
default_crs     <- 4326

lookup_file <- "habitat_codes.csv"

options(shiny.maxRequestSize = 256 * 1024 ^ 2)

bold_rownames <- function(el, include = TRUE) {
  if(!include)
    return(NULL)
  tags$style(paste0("#", el, " td:first-child { font-weight: bold; }"))
}

bold_lastrow <- function(el, include = TRUE) {
  if(!include)
    return(NULL)
  tags$style(paste0("#", el, " tr:last-child { font-weight: bold; }"))
}

copy_button <- function(id, format, formatLab){
  return(actionButton(paste("copy", id, format, sep = "_"), 
                      paste("Copy as", formatLab), 
                      onclick = sprintf("copytable('%s','%s')", id, format)))
}

copy_button_group <- function(id, time, idt = paste(id, time, sep = "_")){
  div(
    copy_button(idt, "text",  "Text"),
    copy_button(idt, "html",  "HTML"),
    copy_button(idt, "latex", "LaTeX"),
    class = "div-copybttns"
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

repl_sp_da <- function(text) {
  text <- gsub(" ", "_", text)
  text <- gsub("-", "_", text)
  return(text)
}

my_spinner <- function(el) withSpinner(el, type = 1, color = "#228B22", color.background = "#FFFFFF")

ui_nm <- function(id, name, include = TRUE) div(h3(name), 
                                checkboxInput(paste0("include_", id),
                                              label = paste("Include", name),
                                              value = include),
                                uiOutput(id))

sfdiv  <- function(...) div(..., class = "sfdiv-container")
sfdivi <- function(...) div(..., class = "sfdiv-item")

#this keeps the overflow same as sfInput for good spacing
input_group_div <- function() div(class = "shiny-input-container")

sfInput <- function(name, lab){
  sfdivi(
    fileInput(name, lab, accept = map_accepts, multiple = TRUE),
    tags$style("white-space: pre-wrap;"),
    verbatimTextOutput(paste(name, "name", sep = "_"))
    )
}

sfMapOutput <- function(id, name){
  sfdivi(
    input_group_div(),
    h3(paste(name, "Map", paste0("(", id, ")"))),
    leafletOutput(paste0("plot", id)),
    uiOutput(paste0("map", id, "col")),
    checkboxInput(paste0("map", id, "_include"), "Include Leaflet Plot", value = TRUE)
  )
}

extentObj <- function(id, time, b_rnms = TRUE, b_lrow = FALSE){
  wellPanel(
    input_group_div(),
    bold_rownames(paste(id, time, sep = "_"), include = b_rnms),
    bold_lastrow(paste(id, time, sep = "_"), include = b_lrow),
    tableOutput(paste(id, time, sep = "_")) %>% my_spinner(),
    uiOutput(paste("copybttn", id, time, sep = "_")),
    class = "sfdiv-item"
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    titlePanel("Extent Account Creator"),
    tabsetPanel(
      {tabPanel("Extent Account",
      fluidRow(
        uiOutput("sf_group"),
      ),
      fluidRow(
        column(6,
          selectizeInput("sel_crs", "Select CRS", choices = NULL, width = "100%"),
        ),
        column(6,
          actionButton("addTimePoint", 
                       label = "Add Time Point", 
                       icon  = icon("plus-circle"), 
                       style = 'margin-top:25px'),
          actionButton("delTimePoint", 
                       label = "Delete Time Point", 
                       icon  = icon("minus-circle"), 
                       style = 'margin-top:25px'),
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
       bsCollapse(
         bsCollapsePanel("Colour mapping",
                         uiOutput("colour_map")
                         )
       )
      ),
      fluidRow(
        column(12,
        actionButton("gen_extent", "Generate/Refresh Extent", class = "btn-primary"),        
        align = "center")
      ),
      fluidRow(
        tags$script(src = "copytable.js"),
        ui_nm("extentTable_group", "Extent table (Ha)"),
        ui_nm("extentPercentTable_group", "Extent table (% of opening)"),
        ui_nm("extentMatrix_group", "Ecosystem Type Change Matrix"),
        ui_nm("extentPair_group", "Change in land cover by group pair", include = FALSE),
        hr(),
        includeHTML("www/notes.html")
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
  plots  <- reactiveValues()
  mapIds <- reactiveVal(1:2)
  sfRaws <- reactiveValues()
  sfs    <- reactiveValues()
  
  plot_names <- reactive({
    p_names <- c("plotComp", "plotStack", paste0("plotMap", mapIds()))
    for(plt in p_names)
      plots[[plt]] <- NULL
    return(p_names)
  })
  
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
  
  #replace NAs and Infs with 0
  clean_zero <- function(x) replace(x, is.na(x) | is.infinite(x), 0)
  
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
  
  sfid <- function(id, ...) paste0("sf", id, ...)
  
  get_sf_name <- function(id, inp = input){
    x <- inp[[sfid(id)]]$name
    if(is.null(x))
      return(" ")
    return(strsplit(x, "\\.")[[1]][1])
  }
  
  #selectInput for what column of sf data to colour in the map and for accounts
  renderMapSel <- function(id){
    output[[paste0("map", id, "col")]] <- renderUI({
      selectInput(sprintf("map%s_sel_col", id), "Select Grouping Column", 
                  choices = names(sfRaws[[id]]))
      })
    return()
  }
  
  renderSfName <- function(id){
    sf_name <- get_sf_name(id)
    output[[sfid(id, "_name")]] <- renderText({sf_name})
    return()
  }
  
  map_oc <- function(idx, inp = mapIds()){
    if(idx == 1)
      return("Opening")
    else if(idx == inp[length(inp)])
      return("Closing")
    else
      return("")
  }
  
  chng_time <- function(id)
    sprintf("(%s - %s)", get_sf_name(as.integer(id)-1), get_sf_name(id))
                                           
  tabtitle <- function(id, nm) return(paste(nm, chng_time(id)))
  
  observeEvent(input$addTimePoint, {
    mapIds(c(mapIds(), length(mapIds()) + 1))
  })
  
  observeEvent(input$delTimePoint, {
    n <- length(mapIds())
    if(n == 2)
      showNotification("must have at least two time points", type = "warning")
    else
      mapIds(mapIds()[-n])
    return()
  })
  
  output$sf_group <- renderUI({
    mapTitle <- function(idx, inp = mapIds()) 
      paste("Upload", map_oc(idx, inp), "Map", paste0("(", idx, ")"))
    
    do.call(sfdiv, 
            purrr::map(mapIds(),
              ~ sfInput(sfid(.x), mapTitle(.x))
              )
            )
  })
  
  output$sf_map_group <- renderUI({
    do.call(sfdiv, 
            purrr::map(mapIds(),
              ~ sfMapOutput(.x, map_oc(.x))
              )
            )
  })
  
  renderExtentObj <- function(tabname, b_rnms = TRUE, b_lrow = FALSE){
    req(input$gen_extent)
    if(any(sapply(mapIds(), sf_null)))
      return(NULL)
    if(!input[[paste0("include_", tabname, "_group")]])
      return(NULL)
    do.call(sfdiv, 
            purrr::map(as.character(mapIds()[-1]),
                       ~ div(h5(tabtitle(.x, tabname)),
                             extentObj(tabname, .x, b_rnms, b_lrow))
            )
    )
  }
  
  output$extentTable_group <- renderUI({renderExtentObj("extentTable")})
  
  output$extentPercentTable_group <- renderUI({renderExtentObj("extentPercentTable")})
    
  output$extentMatrix_group <- renderUI({renderExtentObj("extentMatrix")})
  
  output$extentPair_group <- renderUI({renderExtentObj("extentPair", b_rnms = FALSE, b_lrow = TRUE)})

  # Read shapefiles and render other objects
  observe({
    for(id in as.character(mapIds())){
      sf_id <- sfid(id)
      if(is.null(input[[sf_id]]))
        next
      sfRaws[[id]] <- setup_read_sf(input[[sf_id]])
      sfs[[id]]    <- sfRaws[[id]] %>% 
        st_transform(as.numeric(input$sel_crs))
      
      #UI with dropdown for grouping of the datasets e.g. habitat codes
      renderMapSel(id)
    }
  })
  
  observe({
    for(id in mapIds()){
      renderSfName(id)
      renderLeafletPlot(id)
      renderMapPlot(id)
      renderExpTable(paste0(id))
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
    return(is.null(input[[sfid(id)]]) | is.null(input[[paste0("map", id, "_sel_col")]]))
  
  code_lookup <- function(vec){
    if(input$use_codes){
      df <- lookupData()
      check_codedf <- function(x)
        ifelse(x %in% df[, 1], paste(x, "-", df[df[, 1] == x, 2]), as.character(x))
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
    return(sort(cols))
  })

  output$colour_map <- renderUI({
    code_grp <- codeGroups()
    if(length(code_grp) == 0)
      return(NULL)

    vir_palette <- colorFactor(
      palette = "viridis",
      domain  = code_grp
      )
    
    do.call(div, 
            purrr::map(code_grp, 
                       ~ colourInput(paste("colpicker", repl_sp_da(.x), sep = "_"),
                                     label = .x,
                                     value = vir_palette(.x)
                                     )
                       )
            )
  })
  
  #common colour palette between the two maps for easier visualisation of groups
  plotCols <- reactive({
    code_grp <- codeGroups()
    col_vec <- sapply(code_grp, function(x) input[[paste0("colpicker_", repl_sp_da(x))]])
    
    if(any(sapply(col_vec, is.null)) | any(col_vec == ""))
      palette <- "viridis"
    else
      palette <- col_vec
    return(colorFactor(palette = palette, domain = code_grp))
  })

  # Render plots
  renderLeafletPlot <- function(id){
    output[[paste0("plot", id)]] <- renderLeaflet({
      if(plot_wait(id) | !input[[paste0("map", id, "_include")]])
      return(leaflet() %>% setView(lng = 10*id, lat = 0, zoom = 2))
      else
        return(gen_map_leaflet(sfs[[paste0(id)]], input[[paste0("map", id, "_sel_col")]]))
    })
    return()
  }

  extentData <- reactive({
    do.call(req, lapply(mapIds(), function(i) input[[paste0("map", i, "_sel_col")]]))
    
    #get opening, closing, changes for each code  from extent change matrix
    res_list <- lapply(as.character(mapIds()[-1]), function(i) {
      as.data.frame(sapply(codeGroups(), change_area, extentMat()[[i]]))
    })
    names(res_list) <- as.character(mapIds()[-1])
    return(res_list)
  })
  
  chk1 <- function(id, y, n) {
    if(id == "1")
      return(y)
    else
      return(n)
  }
  
  changeData <- reactive({
    change_df <- data.frame()
    for(id in as.character(mapIds())){
      extent_df <- extentData()[[chk1(id, "2", id)]]
      row_df <- data.frame(time   = id,
                           id     = colnames(extent_df),
                           open   = chk1(id, NA, as.numeric(extent_df["opening", ])),
                           close  = as.numeric(extent_df["closing", ]),
                           change = chk1(id, NA, as.numeric(extent_df["net change", ])))
      change_df <- rbind(change_df, row_df)
    }
    return(change_df)
  })
  
  sf_null <- function(i) is.null(input[[sfid(i)]])
  
  renderExtentTable <- function(id){
    output[[paste("extentTable", id, sep = "_")]] <- renderTable({
      extent_df       <- extentData()[[id]]
      extent_df$Total <- rowSums(extent_df)
      return(extent_df)
    }, rownames = TRUE)
    return()
  }
  
  renderExtentPercentTable <- function(id){
    #the change portions can be represented as a percent of the opening
    output[[paste("extentPercentTable", id, sep = "_")]] <- renderTable({
      extent_df  <- extentData()[[id]]
      df <- as.data.frame(sapply(extent_df, function(x) x[2:4] / x[1]))
      rownames(df) <- rownames(extent_df)[2:4]
      df <- apply(df, 2, clean_zero)
      return(df)
    }, rownames = TRUE)
  }

  #A bit more complicated. This now has a matrix where:
  ##diagonals: amounts unchanged between opening and closing in that group
  ##off-diagonals: amount changed from type in the row to type in the column
  extentMat <- reactive({
    if(any(sapply(mapIds(), plot_wait)))
      return(NULL)
    
    res_l <- list()
    
    for(id in mapIds()[-1]){
      df1 <- sfs[[paste0(id - 1)]] %>% st_make_valid()
      df2 <- sfs[[paste0(id)]] %>% st_make_valid()
  
      code_grps <- codeGroups()
      
      grp_col1 <- input[[sprintf("map%s_sel_col", id-1)]]
      grp_col2 <- input[[sprintf("map%s_sel_col", id)]]
      
      #I think it's faster to intersect everything up front and then lookup
      df_int <- st_intersection(df1, df2)
      
      cross_mat <- do.call(rbind, 
              lapply(code_grps, function(grp1) {
                res <- sapply(code_grps, function(grp2) {
                  df_int %>%
                    filter((df_int[[grp_col1]] == grp1) & (df_int[[grp_col2]] == grp2)) %>%
                    st_make_valid() %>% clean_sum() %>% lazy_unlist()
                })
                return(res)
              })
      )
      
      rownames(cross_mat) <- colnames(cross_mat) <- code_grps
    
      cross_mat <- cross_mat / 10^4
  
      cross_df  <- as.data.frame(cross_mat)
  
      cross_df$openings      <- rowSums(cross_df)
      cross_df["closings", ] <- colSums(cross_df)
      
      res_l[[paste0(id)]] <- cross_df
    }
    return(res_l)
  })

  renderExtentMatrix <- function(id){
    output[[paste("extentMatrix", id, sep = "_")]] <- renderTable({
      extentMat()[[id]]
    }, rownames = TRUE)
  }
  
  renderExtentPairTable <- function(id){
    output[[paste("extentPair", id, sep = "_")]] <- renderTable({
      ext_mat  <- extentMat()[[id]]
      code_grp <- codeGroups()
      
      pair_df <- expand.grid(from = code_grp, to = code_grp)
      pair_df <- pair_df[pair_df$from != pair_df$to,]

      res <- sapply(1:nrow(pair_df), function(i) {
        from <- pair_df[i, "from"]
        to   <- pair_df[i, "to"]
        return(ext_mat[from,to])
      })
      
      pair_df <- pair_df %>% 
        mutate(change = res,
               perc   = change/sum(change)) %>%
        mutate(across(c('change', 'perc'), \(x) round(x, digits = 2)))
      total_df <- data.frame(from = "Total change", to = "", change = sum(pair_df$change), perc = 1.00)
      pair_df <- rbind(pair_df, total_df)
      colnames(pair_df) <- c("Change from", "Change to", "Area (Ha)", "% change")
      
      return(pair_df)
    })
  }
  
  render_copybttns <- function(id, time){
    if(!input$gen_extent)
      return(NULL)
    return(copy_button_group(id, time))
  }
  
  coppybttnOutput <- function(tab, time){
    output[[sprintf("copybttn_%s_%s", tab, time)]] <- renderUI({render_copybttns(tab, time)})
    return()
  }
  
  output$extentPlots <- renderUI({
    req(input$gen_extent)
    if(any(sapply(mapIds(), sf_null)))
      return(NULL)
    
    div(
      tags$head(
        tags$script(src = "copyplot.js")
      ),
      tagList(lapply(plot_names(), plot_copy_group))
    )
  })
  
  observeEvent(input$gen_extent, {
    for(time in as.character(mapIds()[-1])){
      for(tab in c("extentTable", "extentPercentTable", "extentMatrix", "extentPair")){
        coppybttnOutput(tab, time)
      }
      renderExtentTable(time)
      renderExtentPercentTable(time)
      renderExtentMatrix(time)
      renderExtentPairTable(time)
    }
  })
  
  geom_bar_stack <- function(mapping = NULL)
    geom_bar(mapping, position = "stack", stat = "identity")
  
  time_oc <- function(time_vec){
    res <- paste0("(", time_vec, ") - ", sapply(time_vec, get_sf_name))
    return(res)
  }
  
  output$plotStack <- renderPlot({
    df <- changeData() %>% 
      mutate(id   = code_lookup(id),
             time = time_oc(time)) %>%
      mutate(time = factor(time, levels = unique(time)))
    p <- plots$plotStack <- df %>%
      ggplot() + 
      geom_bar_stack(aes(x = time, y = close, fill = id)) +
      ggtitle("Habitat composition") +
      scale_fill_manual(values = plotCols()(code_lookup(df$id))) +
      ylab("Area (Ha)") +
      xlab("") + theme_classic()
    print(p)
  })
  
  output$plotComp <- renderPlot({
    p <- plots$plotComp <-  changeData() %>% 
      filter(time >= 2) %>%
      mutate(id   = code_lookup(id),
             time = sapply(time, chng_time)) %>%
      ggplot() + 
      geom_bar(aes(x = id, y = change, fill = id), stat = "identity") +
      coord_flip() +
      ggtitle("Ecosystem type net changes") +
      scale_fill_manual(values = plotCols()(code_lookup(changeData()$id))) +
      ylab("Area change (Ha)") +
      xlab("") + theme_classic() +
      facet_grid(vars(time))
    print(p)
  })
  
  plot_extent <- function(data, col, name){
    data    <- data %>% arrange(code_lookup(.data[[col]]))
    col_map <- unique(plotCols()(code_lookup(data[[col]])))
    p <- ggplot(data, aes(fill = code_lookup(.data[[col]]))) +
      geom_sf(color = NA) +
      labs(title = name,
           fill  = "Ecosystem Type") + 
      theme_bw() + 
      scale_fill_manual(values = col_map) +
      coord_sf(crs = as.numeric(input$sel_crs))
    print(p)
  }
  
  renderMapPlot <- function(id){
    m_id <- paste0("plotMap", id)
    output[[m_id]] <- renderPlot({
      p <- plots[[m_id]] <- plot_extent(sfs[[paste0(id)]], 
                                        input[[paste0("map", id, "_sel_col")]], 
                                        get_sf_name(id))
      print(p)
    })
    return()
  }
  
  render_download_bttn <- function(id){
    downloadHandler(
      filename = function() paste0(id, '-', Sys.Date(), '.png'),
      content  = function(con) ggsave(con, plots[[id]])
    )
  }
  
  downloadPlotOutput <- function(plt){
    output[[paste0("download_", plt)]]  <- render_download_bttn(plt)
    return()
  }
  
  observe({
    for(plt in plot_names())
      downloadPlotOutput(plt)
  })
  
  output$habitatExplorer <- renderUI({
    if(all(sapply(mapIds(), sf_null)))
      return(NULL)
    
    do.call(div, 
            purrr::map(mapIds(),
                       ~ div(h3(paste0(map_oc(.x, mapIds()), " Data (", .x, ") - ", 
                                       get_sf_name(.x))),
                             tableOutput(paste0("expTable", .x))))
            )
  })
  
  get_explore_table <- function(time, col, df){
    df  <- df[df$time == time, ]
    val <- df[, col]
    exp_df <- data.frame(code   = df$id,
                         aream2 = val * 10^4,
                         areaha = val,
                         perc   = val/sum(val))
    colnames(exp_df) <- c("Code", "Area (m<sup>2</sup>)", "Area (Ha)", "% Coverage")
    return(exp_df)
  }
  
  renderExpTable <- function(id){
    output[[paste0("expTable", id)]] <- renderTable({
      col  <- ifelse(id == "1", "open", "close")
      time <- ifelse(id == "1", "2", id)
      return(get_explore_table(time, col, changeData()))
    }, sanitize.text.function = function(x) x)
    return()
  }
}

shinyApp(uifunc(), server)
