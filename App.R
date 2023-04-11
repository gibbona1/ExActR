#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(tidyverse)
library(shiny)
library(plotly)
library(kableExtra)
library(shinythemes)
library(sf)
library(dplyr)
library(leaflet)
library(stringr)
library(hrbrthemes)
library(forcats)
#library(shinymanager) commented out the use of the login page as it's annoying when prototyping
library(leaflet.extras2)


#load and create columns

##will be better to use relative paths long term

farm <- st_read("C:\\Users\\cwhite4\\Documents\\Shinagh_2\\Farm Map\\Biodiversity_App\\Farm_Maps\\Shinagh.shp")

sf_use_s2(FALSE)# this line can be useful for avoiding issues, it prevents sf reading polygons using the s2 package.

#get area
st_area(farm$geometry) -> farm$Area

#create other data columns to be layers in map
farm %>% mutate(Non_Farmed = case_when(Code %in% c("BL2",
                                                   "ER3",
                                                   "FS1", "FW4",
                                                   "GM1", "GS2",
                                                   "HH3",
                                                   "WD1", "WD2", "WD3","WD5",
                                                   "WL1", "WL2",
                                                   "WN1", "WN2", "WN5",
                                                   "WS1", "WS2",
                                                   "WNX") ~ "Yes",
                                       Code %in% c("GNi",
                                                   "GA1", "GA2",
                                                   "GSi", "GS1", "GS2", "GS4",
                                                   "WS3") ~ "No"),
                Biodiversity_area = case_when(Code %in% c("BL2",
                                                          "ER3",
                                                          "FS1", "FW4",
                                                          "GM1", "GNi",
                                                          "GS1", "GS2", "GS4",
                                                          "HH3",
                                                          "WD1", "WD2", "WD5",
                                                          "WL1", "WL2",
                                                          "WN1", "WN2", "WN5",
                                                          "WS1", "WS2",
                                                          "WNX") ~ "Yes",
                                              Code %in% c("GA1", "GA2", "GSi", "WD3",
                                                          "WS3") ~ "No")
) -> farm

#sum habitats for table in app
farm %>% group_by(Code) %>% 
  summarise(summed_area = sum(Area),
            Code = first(Code),
            Habitat = first(Habitat)) %>% 
  st_drop_geometry() -> Summed_Habitats


Total_Farm_Area_ha <- as.numeric(sum(farm$Area)/10000)
Total_Farm_Area_m2 <- sum(Summed_Habitats$summed_area)


#creating leaflet settings
highlight <- highlightOptions(color = "white", weight = 5,bringToFront =TRUE)
labeloptions <- labelOptions(style = list("font-weight" = "normal",
                                          padding = "3px 8px"),
                             textsize = "15px",
                             direction = "auto")


####
#pop up contents
####
popup_content_habitats <- paste("<strong>", "Habitat: ", "</strong>", farm$Name, "<br/>",
                                "<strong>", "Fossit Code: ", "</strong>", farm$Code, "<br/>",
                                "<strong>", "Area (ha): ", "</strong>", round(farm$Area/10000,2)) %>% 
  lapply(htmltools::HTML)


popup_content_biodiversity_area <- paste("<strong>", "Biodiversity Area: ", "</strong>", farm$Biodiversity_area, "<br/>",
                                         "<strong>", "Habitat: ", "</strong>", farm$Name, "<br/>",
                                         "<strong>", "Fossit Code: ", "</strong>", farm$Code, "<br/>",
                                         "<strong>", "Area (ha): ", "</strong>", round(farm$Area/10000,2)) %>% 
  lapply(htmltools::HTML)

popup_content_non_farmed <- paste("<strong>", "Non-Farmed Area: ", "</strong>", farm$Non_Farmed, "<br/>",
                                  "<strong>", "Habitat: ", "</strong>", farm$Name, "<br/>",
                                  "<strong>", "Fossit Code: ", "</strong>", farm$Code, "<br/>",
                                  "<strong>", "Area (ha): ", "</strong>", round(farm$Area/10000,2)) %>% 
  lapply(htmltools::HTML)



####
#palettes for layers
####

habitat_palette <- colorFactor(
  palette = "viridis",
  domain = unique(farm$Code))

NonFarmed_palette <-  colorFactor(
  palette = c("grey","green"),
  domain = unique(farm$Non_Farmed))


Biodiveristy_palette <-  colorFactor(
  palette = c("green","grey"),
  domain = unique(farm$Non_Farmed))


####
# creating graphs for side panel of map
####

Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("WN2","WN5", "WD2", "WD1", "WD5")] <- "Woodland"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("WL1", "WL2")] <- "Hedgerow"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("GS1", "GS2", "GS4", "GNi")] <- "Grassland"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("FW4")] <- "Ditch"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("BL2")] <- "Vegetated Bank"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("FS1", "GM1")] <- "Wetland"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("WS1")] <- "Scrub"
Summed_Habitats$Broad_Habitat[Summed_Habitats$Code %in% c("HH3")] <- "Peatland"

Summed_Habitats$Percentage_Area <- as.numeric(round((Summed_Habitats$summed_area/Total_Farm_Area_m2)*100,2))
Summed_Habitats$Percent <- round(Summed_Habitats$Percentage_Area, 2)
Summed_Habitats$Farm <- "Shinagh"

library(tidyr)
Summed_Habitats %>% drop_na() %>% 
  group_by(Broad_Habitat) %>% 
  summarise(Percent = sum(Percent),
            Farm = first(Farm)) -> Broad_Habitats


#data comes from Larkin et al. 
# https://www.sciencedirect.com/science/article/pii/S0264837718318210

#woodland is composed of semi-natural woodland + field copse + woody mosaic
#hedgeow is hedgerow/treeline
#grassland is semi-natural grassland + wet grassland + earth bank +
#wetland is wetlands + pond
#scrub is transitional grass/scrub
#peatland is heath
Larkin_Data <- tibble(Broad_Habitat = c("Woodland", "Hedgerow", "Grassland", "Wetland", "Scrub", "Peatland"),
                      Percent = c(1.62+0.49+0.94, 2.73, 0.03+3.8, 0.28+0.07, 0.17, 0.72),
                      Farm = rep("Average", 6))

full_join(Broad_Habitats, Larkin_Data) -> Comparison_Data


#creating datasets for graphs
Non_Farmed_Data <- tibble(Non_Farmed_Area = c(as.numeric(round((sum(farm$Area[farm$Non_Farmed == "Yes"])/Total_Farm_Area_m2)*100,2)), 100-82.87),
                          Farm = c("Shinagh", "Average"))

Biodiversity_Area_Data <- tibble(Biodiversity_Area = c(as.numeric(round((sum(farm$Area[farm$Biodiversity_area == "Yes"])/Total_Farm_Area_m2)*100,2)), 12.99),
                                 Farm = c("Shinagh", "Average"))


library(hrbrthemes)

labels <- rev(Broad_Habitats$Percent)

pallete <- c("#FDEADA", "#D8E4BE", "#C4BEDF",  "#B3BEA4",  "#E6B8B3", "#A1D6DB", "#C0B9BF", "#FCF1C0")




# plot for accompanying habitat map
g1 <- ggplot(Comparison_Data, aes(y=Percent, x=Farm, group = Broad_Habitat,)) + 
  geom_bar(aes(fill = Broad_Habitat), position="stack", stat="identity") +
  scale_fill_manual(values = pallete, aesthetics = "fill") +
  geom_hline(yintercept = 10) +
  #geom_label(aes(label = Percent), fill = "grey 60", size = 3,
  #           position = position_stack(vjust = 0.5, reverse = FALSE),
  #           colour = "white",
  #           fontface = "bold") +
  annotate("text", label = "10% Target",
           x = 0.75, y = 12.5, size = 3, colour = "black") +
  ggtitle("") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent', color=NA)) +
  xlab("")


# plot for accompanying biodiversity area map
g2 <- ggplot(Biodiversity_Area_Data, aes(y=Biodiversity_Area, x=Farm)) + 
  geom_bar(fill = "green", stat="identity") +
  geom_hline(yintercept = 10) +
  annotate("text", label = "10% Target",
           x = 0.75, y = 12.5, size = 4, colour = "black") +
  ggtitle("") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent', color=NA)) +
  xlab("")


# plot for accompanying non-farmed area map
g3 <- ggplot(Non_Farmed_Data, aes(y=Non_Farmed_Area, x=Farm)) + 
  geom_bar(fill = "green", stat="identity") +
  ggtitle("") +
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(fill='transparent', color=NA),
        legend.box.background = element_rect(fill='transparent', color=NA)) +
  xlab("")



# table for habitat explorer
farm %>%
  group_by(Code, Non_Farmed, Biodiversity_area) %>% 
  summarise(Habitat = first(Habitat),
            Code = first(Code),
            Non_Farmed = first(Non_Farmed),
            Biodiversity_area = first(Biodiversity_area),
            Area_ha = round(as.numeric(sum(Area)/10000), 2)
  ) %>% 
  st_drop_geometry() -> Habitat_table



##
#credentials for login ## commented out
#credentials <- data.frame(
#  user = c("admin", "Sean Condon", "Gubbeen"), # mandatory
#  password = c("4dM{4q", "*aZw6{", "iZ!809"), # mandatory
#  admin = c(TRUE, FALSE, FALSE),
#  comment = "Simple and secure authentification mechanism 
#  for single ‘Shiny’ applications.",
#  stringsAsFactors = FALSE
#)


ui <- navbarPage("Biodiversity Dashboard", id = "nav",
                 #tags$h2("My secure application"),
                 verbatimTextOutput("auth_output"),
                 tabPanel("Interactive map",
                          div(class="outer",
                              tags$style(type = "text/css", "#farm_map {height: calc(100vh - 80px) !important; width: calc(66vw - 80px) !important}"),
                              
                              fluidRow(
                                column(width = 8,
                                       
                                       leafletOutput("farm_map")
                                       
                                ),
                                column(width = 4,
                                       selectInput("habitat", "Select",
                                                              choices = c(
                                                                "Habitats",
                                                                "Biodiversity Area",
                                                                "Non-Farmed Area"),
                                                              selected = c("Habitats"),
                                                   multiple = FALSE
                                           ),
                                           #p(
                                          #   class = "text-muted",
                                          #   paste("Habitats displays all habitats on the farm, including farmed habitats.", "<br/>",
                                          #         "Biodiversity Area includes all habitats that are considered to add to biodiversity", "<br/>",
                                          #         "Non-Farmed Area includes only habitats that are not actively involved in farming"
                                          #   ) %>% 
                                          #     lapply(htmltools::HTML)
                                          # ),
                                       plotlyOutput("habitatPlot", height = "75vh")
                                       ),
                                )
                              )
                          ),
                 tabPanel("Habitats Explorer", div(class="outer",
                                                   DT::dataTableOutput("habitatTable")
                                                   )
                          ),
                 tabPanel("EPA Explorer", div(class="outer", 
                                              tags$style(type = "text/css", "#epa_map {height: calc(100vh - 80px) !important; width: calc(100vw - 80px) !important}"),
                                              leafletOutput("epa_map")
                 )
                 )
                 
                 
)


# Wrap your UI with secure_app
#ui <- secure_app(ui)              



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##### Secure App ##### commented out
  # call the server part
  # check_credentials returns a function to authenticate users
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #)
  
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #})
  
  ##### Habitat Map ######
  
  map = leaflet() %>%
    addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
    addMapPane("polygons", zIndex = 420) %>%        # Level 2: middle
    addProviderTiles(providers$Esri.WorldImagery,
                     options = pathOptions(pane = "background_map"))%>%
    addPolygons(data = farm,
                fillColor = ~habitat_palette(farm$Code), 
                fillOpacity = 0.7,
                color = "#b2aeae", #boundary colour, need to use hex color codes.
                weight = 0.5, 
                smoothFactor = 0.2,
                popup =  popup_content_habitats,
                popupOptions = labeloptions,
                highlightOptions = highlight,
                options = pathOptions(pane = "polygons"),
                group = "Habitats") %>% 
    addLegend(pal = habitat_palette,
              values = farm$Code, 
              position = "bottomleft", 
              title = "Habitat Type <br>") %>% 
    addPolygons(data = farm,
                fillColor = ~NonFarmed_palette(farm$Non_Farmed), 
                fillOpacity = 1,
                color = "#b2aeae", #boundary colour, need to use hex color codes.
                weight = 0.5, 
                smoothFactor = 0.2,
                popup =  popup_content_non_farmed,
                popupOptions = labeloptions,
                highlightOptions = highlight,
                options = pathOptions(pane = "polygons"),
                group = "Non-Farmed Area") %>% 
    addPolygons(data = farm,
                fillColor = ~NonFarmed_palette(farm$Biodiversity_area), 
                fillOpacity = 1,
                color = "#b2aeae", #boundary colour, need to use hex color codes.
                weight = 0.5, 
                smoothFactor = 0.2,
                popup =  popup_content_biodiversity_area,
                popupOptions = labeloptions,
                highlightOptions = highlight,
                options = pathOptions(pane = "polygons"),
                group = "Biodiversity Area") %>% 
    addLayersControl(
      overlayGroups = c("Habitats", "Biodiversity Area", "Non-Farmed Area"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup(c("Biodiversity Area", "Non-Farmed Area"))
  
  
  
  output$farm_map <- renderLeaflet({ map %>% showGroup(
    if(input$habitat == "Habitats"){c("Habitats")}
    else if(input$habitat =="Biodiversity Area"){c("Biodiversity Area")}
    else if(input$habitat == "Non-Farmed Area"){c("Non-Farmed Area")}
  )})
  
  
  output$habitatPlot <- renderPlotly({
    if(input$habitat == "Habitats"){g1}
    else if(input$habitat =="Biodiversity Area"){g2}
    else if(input$habitat == "Non-Farmed Area"){g3}
    })
  
  
  #####Habitat Explorer######

  
  output$habitatTable <-  DT::renderDataTable({Habitat_table})
  
  
  #####EPA Explorer######
  
  
epa_map_ <- Farm_map <- leaflet() %>%
    addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
    addMapPane("hydrology", zIndex = 420) %>% # Level 2: middle
    addMapPane("P_flowpath", zIndex = 430) %>% # Level 2: middle
    addMapPane("P_point", zIndex = 440) %>% # Level 2: middle
    addMapPane("polygons", zIndex = 425) %>%
    
    addProviderTiles(providers$Esri.WorldImagery,
                     options = pathOptions(pane = "background_map"))%>%
    addPolygons(data = farm,
                fillColor = ~habitat_palette(farm$Code), 
                fillOpacity = 0.7,
                color = "#b2aeae", #boundary colour, need to use hex color codes.
                weight = 0.5, 
                smoothFactor = 0.2,
                popup =  popup_content_habitats,
                popupOptions = labeloptions,
                highlightOptions = highlight,
                options = pathOptions(pane = "polygons"),
                group = "Habitats") %>% 
    addLegend(pal = habitat_palette,
              values = farm$Code, 
              position = "bottomleft", 
              title = "Habitat Type <br>") %>%
    addWMS(                                                 ### ESSENTIAL TO USE addWMS AND NOT addWMSTILES, if you want pop on click to work
      baseUrl="https://gis.epa.ie/geoserver/EPA/ows",
      layers = "EPA:SOILS_WETDRY",
      group = "Hydrology",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE,
                                        info_format = "text/html",
                                        pane = "hydrology"),
      attribution = "Soil Hydrology data © EPA") %>% 
    
    addWMSTiles(
      baseUrl="https://gis.epa.ie/geoserver/EPA/ows",
      layers = "EPA:PIP_FLOWPATHS",
      group = "P Flow Paths",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE,
                                        pane = "P_flowpath"),
      attribution = "PIP Flowpath data © EPA") %>%
    
    addWMSTiles(
      baseUrl="https://gis.epa.ie/geoserver/EPA/ows",
      layers = "EPA:PIP_P_DELIVERYPOINTS",
      group = "P Delivery Points",
      options = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE,
                               info_format = "text/html",
                               pane = "P_point"),
      attribution = "PIP Flowpath data © EPA") %>% 
    
    addLayersControl(
      overlayGroups = c("Habitats", "Hydrology", "P Flow Paths", "P Delivery Points"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    hideGroup(c("Hydrology", "P Flow Paths", "P Delivery Points"))
  
  
  output$epa_map <- renderLeaflet({ epa_map_ })
}

# Run the application 
shinyApp(ui = ui , server = server)
