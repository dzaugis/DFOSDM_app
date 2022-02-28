library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)

# Some data
data<-structure(list(scientificName = c("Turdus merula Linnaeus, 1758", 
                                        "Passer domesticus (Linnaeus, 1758)", "Cantharellus cinereus (Pers.) Fr.", 
                                        "Flammulina fennae Bas", "Mycena crocata (Schrad.) P.Kumm.", 
                                        "Lepista luscina (Fr.) Singer", "Mycena permixta (Britzelm.) Sacc.", 
                                        "Rhodophyllus byssisedus (Pers.) Quel.", "Rhodophyllus porphyrophaeus (Fr.) J.E.Lange", 
                                        "Panaeolus rickenii Hora"), decimalLatitude = c(52.204429, 51.387818, 
                                                                                        52.176667, 50.066111, 49.179167, 49.419444, 52.3, 52.3, 49.419444, 
                                                                                        49.179167), decimalLongitude = c(21.189275, 19.62673, 19.088056, 
                                                                                                                         19.502778, 22.434722, 20.380556, 20.566667, 20.566667, 20.380556, 
                                                                                                                         22.434722)), row.names = c(1L, 2L, 32L, 35L, 37L, 38L, 39L, 40L, 
                                                                                                                                                    41L, 42L), class = c("spec_tbl_df", "tbl_df", "tbl", "data.frame"
                                                                                                                                                    ))
speciesList <- tibble(comname = c("American lobster"))
sspList <- tibble(ssp = c("SSP1 2.6", "SSP5 8.5"))
fileNames <- list.files(gmRi::box_path(box_group = "Mills lab", 
                                       subfolder = "Projects/sdm_workflow/targets_output/shiny"))
  
# Define the side panel UI and server
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("species"),
      label = "Species name", 
      choices = unique(speciesList$comname),
      selected = unique(speciesList$comname)[1]
    ),
    selectInput(
      inputId = ns("ssp"),
      label = "SSP scenario", 
      choices = unique(sspList$ssp),
      selected = unique(sspList$ssp)[1]
    ),
    numericInput(
      inputId = ns("year"),
      label = "Year", 
      choices = unique(sspList$ssp),
      selected = unique(sspList$ssp)[1]
    )
  )
  
}

sideServer <- function(id, valueType = "predictions", Season = "Spring") {
  moduleServer(
    id,
    function(input, output, session) {
      
      # define a reactive and return it
      toListen <- reactive({
        list(input$species,input$ssp)
      })
     
      path_r<- observeEvent(toListen(), {
        
        name_ <- str_replace_all(input$species, "[:space:]", "_")
        ssp_ <- str_replace_all(input$ssp, c("[:space:]" = "_", "\\." = ""))
      
        fileName <- str_subset(fileNames, paste(name_, ssp_, sep="_")) %>% str_subset()
        
        path <- paste0(here::here("Data/"),
                       fileName)
      })
      
      df <- reactive(input$year, {
        df <- read_csv(path_r())
        dfs <- df %>% mutate(year = lubridate::year(Date),
                            mon = lubridate::month(Date),
                            season = case_when(mon == 3 ~ "Spring",
                                               mon == 7 ~ "Summer",
                                               mon == 9 ~ "Fall"))
        dfs <- dfs %>% filter(year == 1990,
                            season == Season)
      })
      
      df_r <- sf::st_as_sf(dfs, coords = c("x", "y"), crs = 32619, remove = FALSE) %>% drop_na(Log_Biomass)
      dfts <- dfs %>% drop_na(Log_Biomass)
      df_r <-  raster::rasterFromXYZ(dfts[,c("x","y","Log_Biomass")])
      return(df_r)
      
    })
}


# In this case this server not needed but using uiOuput/renderUI in real case
# sideServer <- function(id) { moduleServer(id,function(input, output, session) { })}

# Define the UI and server functions for the map
mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}

mapServer <- function(id, city) {
  moduleServer(
    id,
    function(input, output, session) {
      output$map<-renderLeaflet({

        leaflet() %>% 
          addTiles() %>%
          addRasterImage(x = df_r, color = ~Log_Biomass)
      })
    })
}

# Build ui & server and then run
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(sideUI("side")),
  dashboardBody(column(width = 6, 
                       mapUI("mapBiomass")),
                column(width = 6,
                       mapUI("mapPrediction")))
)
server <- function(input, output, session) {
  
  # use the reactive in another module
  city_input <- sideServer("side")
  mapServer("mapBiomass", city_input)
  
}
shinyApp(ui, server)

