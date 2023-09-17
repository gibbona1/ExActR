pkgload::load_all()

#need to upload at least .shp, .shx, .dbf, .prj files for each
#so the map knows where to put itself
map_accepts <- c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj", ".zip")
#TODO:
## UI
### (common) legend outside of map and fully visible
### nicer UI e.g. https://github.com/Appsilon/shiny.semantic
### choose colour palette, theme (bootstrap) etc

## Server/computation
### maps should have hover to display codes, areas of polygons etc - mapview?
### check if projection or sf_use_s2(FALSE) impacts areas
### informative errors - https://shiny.posit.co/r/articles/improve/validation/
### JSON upload/download colour map

## Robustness and testing
### https://rstudio.github.io/shinytest2/articles/robust.html

logo_info <- list(
  "nature-energy" = list(
    href = "https://www.marei.ie/project/natureenergy/",
    src  = "https://www.marei.ie/wp-content/uploads/2021/06/Nature-Energy-Project-profile.jpg"),
  "for-es" = list(
    href = "https://www.for-es.ie/",
    src  = "https://static.wixstatic.com/media/94066f_291a8945f27e4b6e9f455baa3a6d0428~mv2.png/v1/fill/w_140,h_39,al_c,q_85,usm_0.66_1.00_0.01,enc_auto/FOR-ES_Logo-Colour.png")
  )


get_logo <- function(id)
  tags$li(class = "dropdown",
          tags$a(href=logo_info[[id]]$href,
                 tags$img(src=logo_info[[id]]$src, height='25')),
          style = "margin-top: -5px; margin-bottom: -5px; background: #86e36d;")


crs_data <- rgdal::make_EPSG()
crs_list <- crs_data$code
names(crs_list) <- paste(paste0("EPSG:", crs_list), crs_data$note, sep = " - ")
default_crs     <- 4326

lookup_file <- "ext_data/habitat_codes.csv"

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

sfid <- function(id, ...) paste0("sf", id, ...)

get_msc <- function(id) sprintf("map%s_sel_col", id)

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
    jqui_resizable(plotOutput(id)),
    actionButton(paste0("copy_", id), "Copy", icon = icon("copy"),
                 onclick = sprintf("copyplot('%s')", id)),
    downloadButton(paste0("download_", id),    label = "Download Original"),
    downloadButton(paste0("download_rs_", id), label = "Download Resized", onclick = sprintf("get_img_src('%s')", id))
  )
}

repl_sp_da <- function(text) {
  text <- gsub(" ", "_", text)
  text <- gsub("-", "_", text)
  return(text)
}

colpicker_id <- function(x){
  return(paste("colpicker", repl_sp_da(x), sep = "_"))
}

my_spinner <- function(el) withSpinner(el, type = 1, color = "#228B22", 
                                       color.background = "#FFFFFF")

ui_nm <- function(id, name, include = TRUE) div(h3(name), 
                                checkboxInput(paste0("include_", id),
                                              label = paste("Include", name),
                                              value = include),
                                uiOutput(id))

sfdiv  <- function(...) div(..., class = "sfdiv-container")
sfdivi <- function(...) div(..., class = "sfdiv-item")

get_sf_path <- function(id, inp){
  x <- inp[[sfid(id)]]$name
  if(is.null(x))
    return(" ")
  return(strsplit(x, "\\.")[[1]][1])
}

sfInput <- function(id, name, lab, width = NULL, inp = input){
  sfdivi(id = paste(name, "grp", sep = "_"),
    fileInput(name, lab, accept = map_accepts, multiple = TRUE, width = width),
    tags$style("white-space: pre-wrap;"),
    textInput(paste(name, "name", sep = "_"), NULL, value = get_sf_path(id, inp)),
    leafletOutput(paste0("plot", id)),
    uiOutput(paste0("map", id, "col")),
    checkboxInput(sprintf("map%s_include", id), "Show Map", value = TRUE)
    )
}

extentObj <- function(id, time, b_rnms = TRUE, b_lrow = FALSE) {
  id_time <- paste(id, time, sep = "_")
  wellPanel(
    bold_rownames(id_time, include = b_rnms),
    bold_lastrow(id_time, include = b_lrow),
    tableOutput(id_time) %>% my_spinner(),
    uiOutput(paste("copybttn", id_time, sep = "_")),
    class = "sfdiv-item"
  )
}

ctd   <- function(el, align = "left") tags$td(align = align, el)
table <- tags$table
tr    <- tags$tr
li    <- tags$li
ul    <- tags$ul

uifunc <- function() {
  dashboardPage(
    header  = dashboardHeader(title      = "Extent Account Creator", 
                              titleWidth = "300px",
                              get_logo("nature-energy"),
                              get_logo("for-es")
                              ),
    sidebar = dashboardSidebar(
      tags$head(tags$style(HTML(".sidebar-menu > li {white-space: normal;}"))),
      width = "300px",
      collapsed = TRUE,
      selectizeInput("sel_crs", "Select CRS", choices = NULL, width = "100%"),
      checkboxInput("use_s2", "Use s2 package", value = TRUE),
      checkboxInput("show_legend", "Show leaflet legends", value = TRUE),
      actionButton("addTimePoint", 
                   label = "Add Time Point", 
                   icon  = icon("plus-circle"), 
                   #style = 'margin-top:25px; color: white;',
                   class = "btn-success"),
      actionButton("delTimePoint", 
                   label = "Delete Time Point", 
                   icon  = icon("minus-circle"), 
                   #style = 'margin-top:25px; color: white;',
                   class = "btn-danger"),
      downloadButton("bundleResults", 
                   label = "Download all results", 
                   style = 'margin-left:15px;',
                   class = "btn-warning"),
      tags$script(HTML('
        $(document).ready(function() {
          var actionButton = document.getElementById("addTimePoint");
          var downloadButton = document.getElementById("bundleResults");
          
          // Get the computed style of the action button
          var textColor = window.getComputedStyle(actionButton).color;
          
          // Apply the computed text color to the download button
          downloadButton.style.color = textColor;
        });
      ')),
      fileInput("lookupFile", "Upload Lookup table", accept = ".csv"),
      verbatimTextOutput("lookup_file"),
      checkboxInput("use_codes", "Use code lookup", value = FALSE),
      checkboxInput("use_date", "Use date in plot download filenames", value = FALSE)
    ),
    body    = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tags$head(tags$script('
        var window_width = 0;
        $(document).on("shiny:connected", function(e) {
            window_width = window.innerWidth;
            Shiny.onInputChange("window_width", window_width);
        });
        $(window).resize(function(e) {
            window_width = window.innerWidth;
            Shiny.onInputChange("window_width", window_width);
        });
    ')),
    tabsetPanel(id = "mainTabs",
      {tabPanel("Extent Account",
      fluidRow(
        uiOutput("sf_group"),
      ),
      fluidRow(
       bsCollapse(
         bsCollapsePanel("Colour mapping",
                         uiOutput("colour_map")
                         )
       )
      ),
      fluidRow(
        column(12,
        actionButton("gen_extent", "Generate Extent", class = "btn-primary"),        
        align = "center")
      ),
      fluidRow(
        tags$script(src = "copytable.js"),
        ui_nm("extentTable_group", "Extent table (Ha)"),
        ui_nm("extentPercentTable_group", "Extent table (% of opening)"),
        ui_nm("extentMatrix_group", "Ecosystem Type Change Matrix"),
        ui_nm("extentPair_group", "Change by group pair", include = FALSE),
        hr(),
        tags$script(src = "colourextentdiag.js"),
        checkboxInput("col_diag", "Colour diagonals of Matrix", value = FALSE),
        includeHTML("www/notes.html")
      )
      )},
      {tabPanel("Composition Plots",
                uiOutput("extentPlots"))},
      {tabPanel("Habitat Explorer",
                uiOutput("habitatExplorer"))},
      {tabPanel("Instructions", 
                fluidRow(
                  tags$ol(
                    tags$li("Decide on the number of time points you want to use",
                            "(use the", tags$b("add/delete time point"), "buttons to adjust this)."),
                    tags$li("Upload the 2 or more map files", "(upload", 
                      lapply(c(".cpg", ".shp", ".dbf", ".prj"), tags$code), "files together for each time point)."),
                    tags$li("Select the grouping column (e.g.", tags$em("CLC_CODE_2022"), "for each time point."),
                    tags$li("Toggle secondary options if desired", 
                            "(CRS, s2 package, code lookup)."),
                    tags$li("When ready, click", tags$b("Generate Extent.")),
                    tags$li("Wait for extent table results.",
                            "result plots are in the", tags$b("Composition plots"), "tab.")
                  )
                )
                )}
    )
    ),
    skin = "green"
  )
}

server <- function(input, output, session) {
  plots  <- reactiveValues()
  tables <- reactiveValues()
  mapIds <- reactiveVal(1:2)
  sfRaws <- reactiveValues()
  sfs    <- reactiveValues()
  
  plot_names <- reactive({
    p_names <- c("plotComp", "plotStack", paste0("plotMap", mapIds()))
    for(plt in p_names)
      plots[[plt]] <- NULL
    return(p_names)
  })
  
  tab_names <- reactive({
    p_ids <- function(nm, ids = mapIds()[-1]) paste(nm, ids, sep = "_")
    t_names <- c(p_ids("extentTable"), p_ids("extentPercentTable"), 
                 p_ids("extentMatrix"), p_ids("expTable", ids = mapIds()))
    for(tab in t_names)
      tables[[tab]] <- NULL
    return(t_names)
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
    if(any(endsWith(shpdf$name, ".zip"))){
      upfiles   <- unzip(file.path(updir, shpdf$name[1]), exdir = updir)
      tmp_file1 <- upfiles[endsWith(upfiles, ".shp")]
    } else{
      tmp_file1 <- file.path(updir, shpdf$name[endsWith(shpdf$name, ".shp")])
    }
    return(st_read(tmp_file1, quiet = TRUE))
  }

  #to avoid errors, if map intersections return NULLs, just return zero
  blank_zero <- function(x) {
    if(length(x) == 0)
      return(0)
    return(as.numeric(x))
  }
  
  #replace NAs and Infs with 0
  clean_zero <- function(x) replace(x, is.na(x) | is.infinite(x) | abs(x) < 1e-3, 0)
  
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
    p <- leaflet(options = leafletOptions(crs = leafletCRS(code = input$sel_crs))) %>%
      addTiles() %>%
      addPolygons(data         = data %>% st_transform(default_crs),
                  fillColor    = plotCols()(code_lookup(data[[column]])),
                  fillOpacity  = 0.7,
                  color        = "#b2aeae", #boundary colour, use hex color codes.
                  weight       = 0.5,
                  smoothFactor = 0.2)
    if(input$show_legend){
      p <- p %>% 
        addLegend(pal      = plotCols(),
                values   = code_lookup(data[[column]]),
                position = "bottomleft",
                title    = "Code <br>")
    }
    return(p)
  }

  #extract from a list and suppress  warnings e.g. NAs, geometry issue, for now
  lazy_unlist <- function(x) suppressWarnings(unlist(x))
  
  get_sf_name <- function(id, inp = input){
    x <- inp[[paste(sfid(id), "name", sep = "_")]]
    if(is.null(x))
      return(" ")
    return(x)
  }
  
  #selectInput for what column of sf data to colour in the map and for accounts
  renderMapSel <- function(id){
    output[[paste0("map", id, "col")]] <- renderUI({
      selectizeInput(get_msc(id), "Select Grouping Column", 
                     options = list(dropdownParent = 'body'),
                     choices = names(sfRaws[[id]]))
      })
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
  
  get_window_width <- function(x){
    if(!is.null(input$window_width))
      return(paste0(floor(0.4*input$window_width), "px"))
    else
      return(NULL)
  }
  
  output$sf_group <- renderUI({
    width <- get_window_width(input$window_width)
    mapTitle <- function(idx, inp = mapIds()) 
      paste("Upload", map_oc(idx, inp), "Map", paste0("(", idx, ")"))
    
    do.call(sfdiv, 
            purrr::map(mapIds(),
              ~ sfInput(.x, sfid(.x), mapTitle(.x), width = width, inp = input)
              )
            )
  })
  
  makeExtent <- function(tabname, brow = TRUE, bcol = FALSE){
    req(input$gen_extent)
    if(any(sapply(mapIds(), sf_null)))
      return(NULL)
    if(!input[[paste0("include_", tabname, "_group")]])
      return(NULL)
    do.call(sfdiv, 
            purrr::map(as.character(mapIds()[-1]),
                       ~ sfdivi(h5(tabtitle(.x, tabname)),
                             extentObj(tabname, .x, brow, bcol))
            )
    )
  }
  
  output$extentTable_group <- renderUI({makeExtent("extentTable")})
  
  output$extentPercentTable_group <- renderUI({makeExtent("extentPercentTable")})
    
  output$extentMatrix_group <- renderUI({makeExtent("extentMatrix")})
  
  output$extentPair_group <- renderUI({makeExtent("extentPair", 
                                                  brow = FALSE, bcol = TRUE)})

  # Read shapefiles and render other objects
  observe({
    lapply(as.character(mapIds()), function(id) {
      sf_id <- sfid(id)
      observeEvent(input[[sf_id]], {
      sfRaws[[id]] <- setup_read_sf(input[[sf_id]])
      sfs[[id]]    <- sfRaws[[id]] %>% 
        st_transform(as.numeric(input$sel_crs))
      
      #UI with dropdown for grouping of the datasets e.g. habitat codes
      renderMapSel(id)
      })
    })
  })
  
  observe({
    for(id in mapIds()){
      coppybttnOutput("expTable", id)
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
    return(is.null(input[[sfid(id)]]) | is.null(input[[get_msc(id)]]))
  
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
      m_col <- input[[get_msc(i)]]
      if(!plot_wait(i))
        cols  <- union(cols, sfs[[paste0(i)]][[m_col]])
    }
    return(sort(cols %>% as.character()))
  })
  
  codeGroupsLookup <- reactive({code_lookup(codeGroups())})

  output$colour_map <- renderUI({
    code_grp <- codeGroupsLookup()
    if(length(code_grp) == 0)
      return(NULL)

    vir_palette <- colorFactor(
      palette = "viridis",
      domain  = code_grp
      )
    
    do.call(div, 
            purrr::map(code_grp, 
                       ~ colourInput(colpicker_id(.x),
                                     label = .x,
                                     value = vir_palette(.x)
                                     )
                       )
            )
  })
  
  #common colour palette between the two maps for easier visualisation of groups
  plotCols <- reactive({
    code_grp <- codeGroupsLookup()
    col_vec  <- sapply(code_grp, function(x) input[[colpicker_id(x)]])
    
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
        p <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
          setView(lng = 10*id, lat = 0, zoom = 2)
      else
        p <- gen_map_leaflet(sfs[[paste0(id)]], input[[get_msc(id)]])
      return(p %>% suspendScroll(wakeTime = 2000, sleepNote = FALSE))
    })
    return()
  }

  extentData <- reactive({
    do.call(req, lapply(mapIds(), function(i) input[[get_msc(i)]]))
    
    #get opening, closing, changes for each code  from extent change matrix
    res_list <- lapply(as.character(mapIds()[-1]), function(i) {
      df <- as.data.frame(sapply(codeGroups(), change_area, extentMat()[[i]]))
      colnames(df) <- codeGroupsLookup()
      return(df)
    })
    names(res_list) <- as.character(mapIds()[-1])
    return(res_list)
  })
  
  chk1 <- function(id, y, n, make_numeric = TRUE) {
    if(id == "1")
      return(y)
    else if(make_numeric)
        return(as.numeric(n))
    else
      return(n)
  }
  
  changeData <- reactive({
    change_df <- data.frame()
    for(id in as.character(mapIds())){
      extent_df <- extentData()[[chk1(id, "2", id, make_numeric = FALSE)]]
      row_df <- data.frame(time   = id,
                           id     = colnames(extent_df),
                           open   = chk1(id, NA, extent_df["opening", ]),
                           close  = as.numeric(extent_df["closing", ]),
                           change = chk1(id, NA, extent_df["net change", ]))
      change_df <- rbind(change_df, row_df)
    }
    return(change_df)
  })
  
  sf_null <- function(i) is.null(input[[sfid(i)]])
  
  renderExtentTable <- function(id){
    t_id <- paste("extentTable", id, sep = "_")
    output[[t_id]] <- renderTable({
      extent_df       <- extentData()[[id]]
      extent_df$Total <- rowSums(extent_df)
      tables[[t_id]] <- extent_df <- apply(extent_df, 2, clean_zero)
      return(extent_df)
    }, rownames = TRUE)
    return()
  }
  
  renderExtentPercentTable <- function(id){
    #the change portions can be represented as a percent of the opening
    t_id <- paste("extentPercentTable", id, sep = "_")
    output[[t_id]] <- renderTable({
      extent_df  <- extentData()[[id]]
      df <- as.data.frame(sapply(extent_df, function(x) x[2:4] / x[1]))
      rownames(df) <- rownames(extent_df)[2:4]
      tables[[t_id]] <- df <- apply(df, 2, clean_zero)
      return(df)
    }, rownames = TRUE)
  }

  #A bit more complicated. This now has a matrix where:
  ##diagonals: amounts unchanged between opening and closing in that group
  ##off-diagonals: amount changed from type in the row to type in the column
  extentMat <- reactive({
    if(any(sapply(mapIds(), plot_wait)))
      return(NULL)
    
    sf_use_s2(input$use_s2)
    
    res_l <- list()
    
    for(id in mapIds()[-1]){
      df1 <- sfs[[paste0(id - 1)]] %>% st_make_valid()
      df2 <- sfs[[paste0(id)]] %>% st_make_valid()
  
      code_grps <- codeGroups()
      
      grp_col1 <- input[[get_msc(id - 1)]]
      grp_col2 <- input[[get_msc(id)]]
      
      #I think it's faster to intersect everything up front and then lookup
      df_int <- st_intersection(df1, df2)
      
      cross_mat <- do.call(rbind, 
              lapply(code_grps, function(grp1) {
                res <- sapply(code_grps, function(grp2) {
                  df_int %>%
                    filter((df_int[[grp_col1]] == grp1) &
                           (df_int[[grp_col2]] == grp2)) %>%
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
    t_id <- paste("extentMatrix", id, sep = "_")
    output[[t_id]] <- renderTable({
      code_grps <- codeGroupsLookup()
      ext_mat <- extentMat()[[id]]
      colnames(ext_mat)[1:length(code_grps)] <- code_grps
      rownames(ext_mat)[1:length(code_grps)] <- code_grps
      tables[[t_id]] <- ext_mat
      return(ext_mat)
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
        mutate(from   = code_lookup(from),
               to     = code_lookup(to),
               change = res,
               perc   = change/sum(change)) %>%
        mutate(across(c('change', 'perc'), \(x) round(x, digits = 2)))
      total_df <- data.frame(from = "Total change", to = "", 
                             change = sum(pair_df$change), perc = 1.00)
      pair_df <- rbind(pair_df, total_df)
      colnames(pair_df) <- c("Change from", "Change to", "Area (Ha)", "% change")
      
      return(pair_df)
    })
  }
  
  render_copybttns <- function(id, time){
    if(any(sapply(mapIds(), plot_wait)))
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
                                        input[[get_msc(id)]], 
                                        get_sf_name(id))
      print(p)
    })
    return()
  }
  
  render_download_bttn <- function(id, resize = TRUE, use_date = FALSE){
    downloadHandler(
      filename = function() paste0(id, ifelse(use_date, paste0('-', Sys.Date()), ""), '.png'),
      content  = function(con) {
        if(resize){
          # get image code from URI
          img_src <- gsub("data:.+base64,", "", input[[paste0(id, "_img_src")]])
          # decode the image code into the image
          img_src <- image_read(base64_decode(img_src))
          # save the image
          image_write(img_src, con)
        } else {
          ggsave(con, plots[[id]])
        }
      })
  }
  
  downloadPlotOutput <- function(plt){
    output[[paste0("download_", plt)]]  <- render_download_bttn(plt, resize = FALSE, use_date = input$use_date)
    output[[paste0("download_rs_", plt)]]  <- render_download_bttn(plt, resize = TRUE, use_date = input$use_date)
    return()
  }
  
  observe({
    for(plt in plot_names())
      downloadPlotOutput(plt)
    for(tab in tab_names()){
      #do nothing
    }
  })
  
  output$habitatExplorer <- renderUI({
    if(all(sapply(mapIds(), sf_null)))
      return(NULL)
    
    do.call(div, 
            purrr::map(mapIds(),
                       ~ div(h3(sprintf("%s Data (%s) - %s", 
                                        map_oc(.x, mapIds()), .x, 
                                        get_sf_name(.x))),
                             tableOutput(paste("expTable", .x, sep = "_")),
                             uiOutput(paste("copybttn", "expTable", .x, sep = "_"))
                             ))
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
    t_id <- paste("expTable", id, sep = "_")
    output[[t_id]] <- renderTable({
      col  <- ifelse(id == "1", "open", "close")
      time <- ifelse(id == "1", "2", id)
      tables[[t_id]] <- df <- get_explore_table(time, col, changeData())
      return(df)
    }, sanitize.text.function = function(x) x)
    return()
  }
  
  observeEvent(input$col_diag, {
      for(id in mapIds()[-1]){
        diag_col <- ""
        ext_id   <- paste("extentMatrix", id, sep = "_")
        if(input$col_diag)
          diag_col <- "rgba(255, 255, 145, 0.5)"
        runjs(sprintf("colourExtentDiag('%s', '%s')", ext_id, diag_col))
      }
  })
  
  output$bundleResults <- downloadHandler(
    filename = function() paste0("bundleData-", Sys.Date(), ".zip"),
    content  = function(con) {
      temp_dir <- tempdir()
      setwd(tempdir())
      
      download_table <- function(tab){
        tab_df <- tables[[tab]]
        if(is.null(tab_df))
          return()
        write.csv(tab_df, paste0(tab, ".csv"))
        write.table(tab_df, paste0(tab, ".txt"))
        print(xtable::xtable(tab_df), type = "latex", file = paste0(tab, ".tex"))
        print(xtable::xtable(tab_df), type = "html", file = paste0(tab, "html"))
      }
      
      #tables in latex, text and HTML format
      for(tab in tab_names())
        download_table(tab)
      
      #plots in png and pdf format
      for(plt in plot_names()){
        ggsave(paste0(plt, ".png"), plots[[plt]])
        ggsave(paste0(plt, ".pdf"), plots[[plt]])
      }
      
      #might need to save Rdata or log (at least inputs)
      z_files <- list.files(pattern = "\\.(pdf|png|html|txt|tex)$")
      #browser()
      zip(zipfile = con, files = z_files)
  }, contentType = "application/zip")
}

shinyApp(uifunc(), server)
