
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)

# List of the available species
speciesList <- c("American lobster", "Black sea bass")

# List of the available SSP scenarios
sspList <- c("SSP5 8.5")

# Box paths
sdmShiny_path <- gmRi::box_path(box_group = "Mills lab", 
                           subfolder = "Projects/sdm_workflow/targets_output/shiny")
Res_Data_path <- gmRi::box_path(box_group = "Res_Data",
                                subfolder = "Shapefiles/ne_50m_land/")

# Convert points in sf polygon for plotting

bSquare <- function(x, a) {
  a <- sqrt(a)/2
  x <- sf::st_as_sf(x, coords = c("x", "y"), crs = 32619, remove = FALSE) %>% drop_na(Log_Biomass)
  x <- mutate(x, geometry = sf::st_buffer(geometry, dist = a, nQuadSegs=1, 
                                          endCapStyle = "SQUARE"))
  return(x)
}


# List files in the baseline_data folder
baseline_fileNames <- list.files(here::here("baseline_data"), full.names = TRUE)
names(baseline_fileNames) <- list.files(here::here("baseline_data"), full.names = FALSE)

# Read in data from projected_data folder
baseline_dataList <- imap(baseline_fileNames, ~data.table::fread(.x, stringsAsFactors = FALSE))
baseline_dataList <- map(baseline_dataList, ~bSquare(.x, 25000*25000))

# List files in the projected_data folder
fileNames <- list.files(here::here("projected_data"), full.names = TRUE)
names(fileNames) <- list.files(here::here("projected_data"), full.names = FALSE)

# Read in data from projected_data folder
dataList <- imap(fileNames, ~data.table::fread(.x, stringsAsFactors = FALSE))

dataList <- imap(dataList, ~mutate(.x, year = lubridate::year(Date),
                                       mon = lubridate::month(Date),
                                       season = case_when(mon == 3 ~ "Spring",
                                                          mon == 7 ~ "Summer",
                                                          mon == 9 ~ "Fall")))

dataList <- map(dataList, ~bSquare(.x, 25000*25000))

# List files in annual summary data from spatial_summary folder
annual_fileNames <- list.files(here::here("spatial_summary"), full.names = TRUE)
names(annual_fileNames) <- list.files(here::here("spatial_summary"), full.names = FALSE)

# Read in data from spatial_summary folder
annual_dataList <- imap(annual_fileNames, ~data.table::fread(.x, stringsAsFactors = FALSE))
annual_dataList <- imap(annual_dataList, ~mutate(.x, meanBio = mean,
                                                 totalBio = sum,
                                                 std = sd))


# List files in annual summary data from spatial_summary folder
center_fileNames <- list.files(here::here("center_biomass"), full.names = TRUE)
names(center_fileNames) <- list.files(here::here("center_biomass"), full.names = FALSE)

# Read in data from center_biomass folder
center_dataList <- imap(center_fileNames, ~data.table::fread(.x, stringsAsFactors = FALSE))

# Getting land mask
bbox <- c(xmin = -78.5, ymin = 34.5, xmax = -54.5, ymax = 48.25)
land_sf<- sf::st_read(paste0(Res_Data_path, "ne_50m_land.shp")) %>% 
  sf::st_crop(bbox)
land_sf<- sf::st_transform(land_sf, crs = 32619)

# SDM plotting function
sdmPlotFun <- function(dataInput, land_sf, plot_lims, mapTitle){
  ggplot(dataInput()) + 
    geom_sf(aes(fill=Log_Biomass, color=Log_Biomass, geometry=geometry)) +
    scale_fill_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims()) +
    scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims()) +
    geom_sf(data = land_sf, fill = "#d9d9d9", lwd = 0.2, na.rm = TRUE) +
    coord_sf(xlim = c(-165000, 1585000), ylim = c(3865000, 5385000), expand = FALSE, crs = 32619) +
    labs(title = mapTitle()) +
    theme_map()
}

# Annual_summary plotting function
annualPlotFun <- function(dataInput, plot_title){
  
  spat_summ_df <- dataInput()
  
  plot_regions <- unique(spat_summ_df$Region)
  
  color_pal <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
  colors_use <- color_pal[1:length(plot_regions)]
  
  # Date stuffs
  date_col_class<- class(spat_summ_df$Date)
  if(is.character(date_col_class)){
    spat_summ_df$Date<- as.Date(spat_summ_df$Date)
  }
  date_breaks<- seq.Date(from = as.Date(min(spat_summ_df$Date)), to = as.Date(max(spat_summ_df$Date)), by = "5 years")
  
  plot_out <- ggplot() +
    geom_point(data = spat_summ_df, aes_string(x = "Date", y = "meanBio", color = "Region")) +
    geom_line(data = spat_summ_df, aes_string(x = "Date", y = "meanBio", color = "Region")) +
    scale_color_manual(values = colors_use) +
    scale_x_date(breaks = date_breaks, date_labels = "%Y") +
    xlab("Year") +
    ylab("Mean log biomass (kg)") +
    ggtitle(plot_title()) + 
    theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  return(plot_out)
  
}

# Center of Biomass plotting function
centerBioPlotFun <- function(dataInput, plot_title){
  plot_out <- dataInput() %>% 
    ggplot() +
    geom_line(aes(year, latitude)) +
    xlab("Year") +
    ylab("Center of Biomass (latitude)") +
    ggtitle(plot_title()) + 
    theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  return(plot_out)
  
}

# Plotting map theme
theme_map <- function(x){
  theme(panel.background = element_blank(), 
       panel.border = element_blank(), 
       axis.text.x=element_blank(), 
       axis.text.y=element_blank(), 
       axis.ticks=element_blank(),  
       plot.margin = margin(t = 10, r = 0.05, b = 0.05, l = 0.05, unit = "pt"),
       legend.position = c(.85, .25))}

## Shiny UI modules

# sidebar user inputs
sideUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("species"),
      label = "Species name", 
      choices = unique(speciesList),
      selected = unique(speciesList)[1]
    ),
    selectInput(
      inputId = ns("ssp"),
      label = "SSP scenario", 
      choices = unique(sspList),
      selected = unique(sspList)[1]
    ),
    selectInput(
      inputId = ns("season"),
      label = "Season", 
      choices = c("Spring", "Summer", "Fall"),
      selected = "Spring"
    ),
    sliderInput(
      inputId = ns("year"),
      label = "Year", 
      value = 2020,
      min=1985,
      max=2100,
      sep=""
    )
  )
  
}

# map output
mapUI <- function(id) {
  ns <- NS(id)
  tagList(plotOutput(ns("plot")))
}

# download button output
downloadUI <- function(id) {
  ns <- NS(id)
  tagList(downloadButton(ns('downloadPlot'), 'Download Plot'),
          downloadButton(ns('downloadData'), 'Download Data'))
}

## Shiny server modules

# Filter the data list based on species and ssp
sideFilter <- function(id, datalist, baseline = FALSE){
  moduleServer(
    id,
    function(input, output, session){
      
       df <- reactive({
        name_ <- str_replace_all(input$species, "[:space:]", "_")
        ssp_ <- str_replace_all(input$ssp, c("[:space:]" = "_", "\\." = ""))
        
        if(isTRUE(baseline)){
          dfName <- str_subset(names(datalist), paste(name_, sep="_"))
        }
        
        if(isFALSE(baseline)){
          dfName <- str_subset(names(datalist), paste(name_, ssp_, sep="_"))
        }
        
        df <- datalist[[dfName]]
        
        return(df)
      })
    }
  )
}

# Filter selected species and ssp by year and season
sideServer <- function(id, df) {
  moduleServer(
    id,
    function(input, output, session) {
      
      df_r <- reactive({
        
        df <- df() %>% dplyr::filter(year == input$year,
                              season == input$season)

        return(df)
      })
      return(df_r)
    })
}

# Create the plot limits
plot_limsServer <- function(id, df){
  moduleServer(
    id,
    function(input, output, session) {
      
      plotLims <- reactive({
        plotlims <- c(min(tibble(df())[, "Log_Biomass"], na.rm = TRUE), max(tibble(df())[, "Log_Biomass"], na.rm = TRUE))
      })
      return(plotLims)
    })
}

# Create map name
mapName <- function(id, plotType){
  moduleServer(
    id,
    function(input, output, session){
      mapName <- reactive({
        
        if(plotType == "baseline"){
          titleName <- paste(input$species, "baseline log biomass", sep = " ")
        }
        
        if(plotType == "sdmPlot"){
          titleName <- paste(input$species, input$ssp, input$season, input$year, sep = " ")
        }
        
        if(plotType == "difference"){
          titleName <- paste(input$species, "difference in log biomass", sep = " ")
        }
        
        if(plotType == "annualPlot"){
          titleName <- paste(input$species, input$ssp, "annual", sep = " ")
        }
        
        return(titleName)
      })
    }
  )
}

# SDM draw plot
mapServer <- function(id, dataInput, land_sf, plot_lims, mapTitle) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot({
        
        sdmPlotFun(dataInput, land_sf, plot_lims, mapTitle)
        
      })
    })
}

# Differencing Server
differenceServer <- function(id, baselineInput, projectionInput, mapTitle){
  moduleServer(
    id,
    function(input, output, session){
      df_r <- reactive({
        df <- sf::st_join(baselineInput(), projectionInput(), by = c("x", "y", "Species", "geometry")) %>% 
          mutate(Log_Biomass = Log_Biomass.y - Log_Biomass.x,
                 Biomass = Biomass.y - Biomass.x) %>% 
          dplyr::select(-Log_Biomass.x, -Log_Biomass.y, -Biomass.x, -Biomass.y)
        return(df)
      })
      
      return(df_r)
    }
  )
}


# Annual summary draw plot
tsPlotServer <- function(id, dataInput, plotType, plotTitle){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "annualPlot"){
        output$plot <- renderPlot({
          annualPlotFun(dataInput = dataInput, plot_title = plotTitle)
        })
      }

      if(plotType == "centerPlot"){
        output$plot <- renderPlot({
          centerBioPlotFun(dataInput = dataInput, plot_title = plotTitle)
        })
      }
      
    }
  )
}

# draw and download plot
downloadServer <- function(id, plotType, dataInput, land_sf, plot_lims, mapTitle){
  moduleServer(
    id,
    function(input, output, session){
      
      if(plotType == "sdmPlot"){
        plot1 <- reactive({
          sdmPlotFun(dataInput, land_sf, plot_lims, mapTitle)
        })
      }
      
      if(plotType == "annualPlot"){
        plot1 <- reactive({
          annualPlotFun(dataInput = dataInput)
        })
      }
      
      if(plotType == "centerPlot"){
        plot1 <- reactive({
          centerBioPlotFun(dataInput = dataInput)
        })
      }
      
      output$downloadPlot <- downloadHandler(
        filename = function() {paste0(mapTitle(), ".png")},
        content = function(file) {
          ggsave(file, plot = plot1(), width = 5, height = 5)
        }
      )
      
      output$downloadData <- downloadHandler(
        filename = function() {paste0(mapTitle(), ".csv")},
        content = function(file) {
          write_csv(dataInput(), file)
        }
      )
    }
  )
}




# Build ui & server and then run
ui <- dashboardPage(
  dashboardHeader(title = "Marine species distribution under CMIP6",
                  titleWidth = 500),
  dashboardSidebar(h3("Selection Panel"),
                   sideUI("side1")),
  dashboardBody(
    fluidRow(
             box(width = 4,
               title = "Baseline SDM Map",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               mapUI("baseline"),
               downloadUI("baseline")),
             box(width = 4,
               title = "Projected SDM Map",
               status = "warning",
               solidHeader = TRUE,
               collapsible = TRUE,
               mapUI("map1"),
               downloadUI("map1")
               ),
             box(width = 4,
                 title = "Difference in biomass",
                 status = "danger",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 mapUI("difference"),
                 downloadUI("difference")
             )
             ),
    fluidRow(
      box(width = 6,
          title = "Annual Summary",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("annualPlot1"),
          downloadUI("annualPlot1")
          ),
      box(width = 6,
          title = "Center of Biomass",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          mapUI("centerPlot"),
          downloadUI("centerPlot")
          )
    )
    )
  )

server <- function(input, output, session) {

# Projected Map
  # sdm plot
  df1 <- sideFilter("side1", datalist = dataList)
  plotLims1 <- plot_limsServer("side1", df1)
  mapTitle1 <- mapName("side1", plotType = "sdmPlot")
  data_input1 <- sideServer("side1", df1) # season year data
  mapServer("map1", dataInput = data_input1, land_sf = land_sf, plot_lims = plotLims1, mapTitle = mapTitle1)
  downloadServer("map1", plotType = "sdmPlot", dataInput = data_input1, land_sf = land_sf, plot_lims = plotLims1, mapTitle = mapTitle1)

# Baseline Map
  baselineDF <- sideFilter("side1", datalist = baseline_dataList, baseline = TRUE)
  baseline_mapTitle <- mapName("side1", plotType = "baseline")  
  mapServer("baseline", dataInput = baselineDF, land_sf = land_sf, plot_lims = plotLims1, mapTitle = baseline_mapTitle)
  downloadServer("baseline", plotType = "sdmPlot", dataInput = data_input1, land_sf = land_sf, plot_lims = plotLims1, mapTitle = baseline_mapTitle)
  
# Difference Map  
  diff_mapTitle <- mapName("side1", plotType = "difference") 
  diffDF <- differenceServer("side1", baselineInput = baselineDF, projectionInput = data_input1, mapTitle = differenceTitle)
  plotLims2 <- plot_limsServer("side1", diffDF)
  mapServer("difference", dataInput = diffDF, land_sf = land_sf, plot_lims = plotLims2, mapTitle = diff_mapTitle)
  downloadServer("difference", plotType = "sdmPlot", dataInput = diffDF, land_sf = land_sf, plot_lims = plotLims2, mapTitle = diff_mapTitle)
  
# annual plot
  annual_df1 <- sideFilter("side1", datalist = annual_dataList, baseline = FALSE)
  ann_mapTitle1 <- mapName("side1", plotType = "annualPlot")
  tsPlotServer("annualPlot1", dataInput = annual_df1, plotType = "annualPlot", plotTitle = ann_mapTitle1)
  downloadServer("annualPlot1", plotType = "annualPlot", dataInput = annual_df1, land_sf = land_sf, plot_lims = plotLims1, mapTitle = ann_mapTitle1)
  
# Center of Biomass
  center_df1 <- sideFilter("side1", datalist = center_dataList, baseline = FALSE)
  center_mapTitle1 <- mapName("side1", plotType = "annualPlot")
  tsPlotServer("centerPlot", dataInput = center_df1, plotType = "centerPlot", plotTitle = center_mapTitle1)
  downloadServer("centerPlot", plotType = "centerPlot", dataInput = center_df1, land_sf = land_sf, plot_lims = plotLims1, mapTitle = center_mapTitle1)

}

shinyApp(ui, server)
