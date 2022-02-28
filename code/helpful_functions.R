######
## R functions associated for visualizing VAST model output
######

#' @title Map VAST inverse distance weighted predictions
#' 
#' @description Creates a map of VAST inverse distance weighted predictions for a given time period
#'
#' @param pred_idw = Data frame inverse distance weighted VAST predictions
#' @param pred_res = Resolution of the prediction grid. Default to 25000 m.
#' @param pred_crs = Prediction CRS as a string or integer value that can be accepted into `st_transform()`. Default to 32619.
#' @param plot_scale = A character string specifying which scale to plot predictions, either "raw" or "log".
#' @param plot_date = A date to plot predictions. Must be in YYYY-MM-DD, with years from 1985-2100 and month days only being "03-16" (spring), "07-16" (summer) or "10-16" (fall).
#' @param land_sf = Land sf object
#' @param xlim = A two element vector with the min and max longitudes to set focal window of the map
#' @param ylim = A two element vector with the min and max latitudes to set focal window of the map
#' @param out_dir = Output directory to save the plot
#' 
#' @return A ggplot map object
#' @examples 
#' # Set box root path
#' box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
#' # Read in prediction data
#' pred_idw<- read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_idw_predictions.csv"))
#' # Read in land shapefile
#' land_sf<- st_read(paste0(box_root, "RES_Data/Shapefiles/ne_50m_land/ne_50m_land.shp"))
#' # Run the function
#' plot_out<- plot_map_idw(pred_idw = pred_idw, pred_res = 25000, pred_crs = 32619, plot_scale = "log", plot_date = "1985-03-16", land_sf = land_sf, xlim = c(-78.5, -54.5), ylim = c(34.5, 48.25), lab_lat = 36, lab_lon = -67.5, out_dir = here:here(), land_color = "#d9d9d9")
#' 
#' @export

plot_map_idw<- function(pred_idw, pred_res = 25000, pred_crs = 32619, plot_scale = "log", plot_date, land_sf, xlim = c(-78.5, -54.5), ylim = c(34.5, 48.25), lab_lat = 36, lab_lon = -67.5, out_dir, land_color = "#d9d9d9", ...){
  # For debugging
  if(FALSE){
    box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
    pred_idw<- read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_idw_predictions.csv"))
    pred_res = 25000
    pred_crs = 32619
    plot_scale = "log"
    plot_date = "1985-03-16"
    land_sf<- st_read("/Users/aallyn/Library/CloudStorage/Box-Box/RES_Data/Shapefiles/ne_50m_land/ne_50m_land.shp")
    xlim = c(-78.5, -54.5)
    ylim = c(34.5, 48.25)
    lab_lat = 36
    lab_lon = -67.5
    out_dir = "/Users/aallyn/Library/CloudStorage/Box-Box/Mills Lab/Projects/sdm_workflow/targets_output/shiny/"
    land_color = "#d9d9d9"
  }
  
  # Getting time info
  all_times = unique(pred_idw$Real_Date)
  
  # Getting spatial information
  land_sf<- st_transform(land_sf, crs = pred_crs)
  
  # Plotting bits
  plot_variable<- ifelse(plot_scale == "log", "Log_Biomass", "Biomass")
  legend_label<- ifelse(plot_scale == "log", "Log biomass (kg)", "Biomass (kg)")
  plot_label<- paste(format(as.Date(plot_date), "%Y"), ifelse(grepl("03-16", plot_date), "SPRING", ifelse(grepl("07-16", plot_date), "SUMMER", "FALL")), sep = " ")
  plot_lims<- c(0, max(pred_idw[,{{plot_variable}}], na.rm = TRUE))
  
  # Filter to time of interest
  plot_points<- pred_idw %>%
    filter(., Date == plot_date) %>%
    st_as_sf(., coords = c("x", "y"), crs = pred_crs, remove = FALSE) %>%
    drop_na(., {{plot_variable}})
  
  # Want this as polygons...data based on 25 km square grid
  bSquare <- function(x, a) {
    a <- sqrt(a)/2
    return( sf::st_buffer(x, dist = a, nQuadSegs=1, 
                          endCapStyle = "SQUARE") )
  }
  
  plot_polys<- bSquare(plot_points, pred_res*pred_res)
  
  # Saving bits
  nice_category_names<- unique(plot_dat$Species)
  climate_scenario<- unique(plot_dat$Climate_Scenario)
  
  if(plot_variable == "Log_Biomass"){
    plot_out<- ggplot() +
      geom_sf(data = plot_polys, aes(fill = Log_Biomass, color = Log_Biomass)) +
      scale_fill_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims) +
      scale_color_viridis_c(name = "Log_Biomass", option = "viridis", na.value = "transparent", limits = plot_lims) +
      annotate("text", x = lab_lon, y = lab_lat, label = plot_label) +
      geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = 4326) +
      theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
  } else {
    plot_out<- ggplot() +
      geom_sf(data = plot_polys, aes(fill = Biomass, color = Biomass)) +
      scale_fill_viridis_c(name = "Biomass", option = "viridis", na.value = "transparent", limits = plot_lims) +
      scale_color_viridis_c(name = "Biomass", option = "viridis", na.value = "transparent", limits = plot_lims) +
      annotate("text", x = lab_lon, y = lab_lat, label = plot_label) +
      geom_sf(data = land_sf, fill = land_color, lwd = 0.2, na.rm = TRUE) +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = 4326) +
      theme(panel.background = element_rect(fill = "white"), panel.border = element_rect(fill = NA), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title = element_blank(), plot.margin = margin(t = 0.05, r = 0.05, b = 0.05, l = 0.05, unit = "pt"))
  }
 
  
  # Save it and return it
  ggsave(filename = paste0(out_dir, "/", nice_category_names, "_", climate_scenario, "_", plot_variable, "_map.png"), plot_out, width = 8, height = 11, units = "in")
  return(plot_out)
 
}


#' @title Calculate annual spatial summaries of point level predictions
#' 
#' @description Calculates summaries of point level predictions for each year and spatial regions of interest by overlaying poly (or multipolygon) sf object with spatial point predictions.
#'
#' @param pred_df = A dataframe with point predictions, containing a minimum of columns "x", "y" and "Date". Why? Because.
#' @param pred_crs = Prediction dataframe CRS as a string or integer value that can be accepted into `st_transform()` to use. Default to 32619.
#' @param spatial_areas = A single or multipolygon sf object with at least columns labeled "Region" and "geometry". The mean (and standard deviation?) of point level predictions are calculated for each of the areas specified in the sf object. 
#' @param summ_stats = A character vector of summary statistics to calculate for each spatial area.
#' @param summ_scale = A character string specifying which scale to calculate summaries, either "raw" or "log". Default "log".
#' @param out_dir = Output directory to save the plot. Default here::here() (i.e., working directory).
#' 
#' @return A dataframe with columns for date, region, mean and sd of either log biomass or biomass.
#' @examples 
#' # Set box root path
#' box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
#' # Read in prediction data
#' pred_df<- read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_idw_predictions.csv"))
#' # Set spatial CRS
#' crs_use<- 32619
#' # Read in spatial_areas shapefile
#' spatial_areas<- st_read(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/spatial_areas.shp"))
#' # Set summary statistic and scale
#' summ_stats<- c("sum", "mean", "sd")
#' summ_scale<- "log"
#' # Run the function
#' spat_summs_out<- calc_annual_spatial_summary(pred_df = pred_df, crs_use = crs_use, spatial_areas = spatial_areas, summ_stats = summ_stats, summ_scale = "log", out_dir = here:here())
#' 
#' @export

calc_annual_spatial_summary<- function(pred_df, crs_use = 32619, spatial_areas, summ_stats, summ_scale = "log", out_dir){
  # For debugging
  if(FALSE){
    box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
    pred_crs<- 32619
    pred_df = read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_idw_predictions.csv"))
    spatial_areas = st_read(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/spatial_areas.shp"))
    summ_scale = "log"
    summ_stats = c("sum", "mean", "sd")
    out_dir = paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/")
  }
  
  # Convert prediction df to spatial dataframe...
  pred_sp<- st_as_sf(pred_df, coords = c("x", "y"), crs = pred_crs, remove = FALSE)
  
  # Join up with the spatial areas after checking crs
  if(st_crs(pred_sp) != st_crs(spatial_areas)){
    spatial_areas<- st_transform(spatial_areas, st_crs(pred_sp))
  }
  pred_sp<- pred_sp %>%
    st_join(., spatial_areas) %>%
    st_drop_geometry()
  
  # Summaries
  summ_variable<- ifelse(summ_scale == "log", "Log_Biomass", "Biomass")
  spat_summ_out<- pred_sp %>%
    mutate(., "Year" = format(as.Date(Date), "%Y")) %>%
    group_by(., Year, Region) %>%
    summarize_at(., vars({{summ_variable}}), c({{summ_stats}}), na.rm = TRUE) %>%
    mutate(., "Date" = as.Date(paste(Year, "06-15", sep = "-")),
           "Species" = unique(pred_sp$Species),
           "Climate_Scenario" = unique(pred_sp$Climate_Scenario)) %>%
    drop_na(., Region)
  
  # Save and return it
  # Saving bits
  nice_category_names<- unique(pred_sp$Species)
  climate_scenario<- unique(pred_sp$Climate_Scenario)
  write.csv(spat_summ_out, file = paste0(out_dir, "/", nice_category_names, "_", climate_scenario, "_", summ_variable, "_spatial_annual_summaries.csv"))
}


#' @title Plot annual spatial summaries 
#' 
#' @description Plots annual summaries for each region.
#'
#' @param spat_summ_df = A dataframe with spatial summaries and columns for Date, Region, Species, Climate Scenario along with summary values. Why? Because.
#' @param plot_variable = A character string specifying which spatial summary variable to plot.
#' @param plot_regions = A character string specifying a subset of spatial regions to plot, or NULL to plot results for all regions in `spat_summ_df$Region`.
#' @param nice_xlab = Label for x axis. Default "Year".
#' @param nice_ylab = Label for the y axis. 
#' @param color_pal = A color palette to use for the plot or NULL to select from c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02') colors.
#' @param out_dir = Output directory to save the plot. Default here::here() (i.e., working directory).
#' 
#' @return A time series ggplot object
#' @examples 
#' # Set box root path
#' box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
#' # Read in annual summary data
#' spat_summ_df<- read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_Log_Biomass_spatial_annual_summaries.csv"))
#' # Define which variable to plot
#' plot_variable = "mean"
#' # Define focal regions or NULL to plot all of them
#' plot_regions = NULL
#' # Set nice labels
#' nice_x_lab = "Year"
#' nice_y_lab = "Total log biomass (kg)"
#' # Color palette
#' color_pal = NULL
#' # Output directory
#' out_dir = here::here()
#' # Run the function
#' spat_summs_out<- plot_annual_spatial_summaries(spat_summ_df = spat_summ_df, plot_variable = "mean", plot_regions = NULL, , crs_use = crs_use, spatial_areas = spatial_areas, summ_stats = summ_stats, summ_scale = "log", out_dir = here:here())
#' 
#' @export

plot_annual_spatial_summaries<- function(spat_summ_df, nice_xlab = "Year", nice_ylab = "Total log biomass (kg)", color_pal = NULL, out_dir){
  if(FALSE){
    box_root<- "/Users/aallyn/Library/CloudStorage/Box-Box/"
    spat_summ_df<- read.csv(paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/American_lobster_SSP5_85_mean_Log_Biomass_spatial_annual_summaries.csv"))
    plot_variable = "mean"
    plot_regions = NULL
    nice_xlab = "Year"
    nice_ylab = "Total log biomass (kg)"
    color_pal = NULL
    out_dir = paste0(box_root, "Mills Lab/Projects/sdm_workflow/targets_output/shiny/")
  }
  
  # Color selection
  if(is.null(plot_regions)){
    plot_regions<- unique(spat_summ_df$Region)
  }
  if(!is.null(color_pal)){
    colors_use<- color_pal
  } else {
    color_pal<- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')
    colors_use<- color_pal[1:length(plot_regions)]
  }
  
  # Date stuffs
  date_col_class<- class(spat_summ_df$Date)
  if(is.character(date_col_class)){
    spat_summ_df$Date<- as.Date(spat_summ_df$Date)
  }
  date_breaks<- seq.Date(from = as.Date(min(spat_summ_df$Date)), to = as.Date(max(spat_summ_df$Date)), by = "5 years")
  
  plot_out<- ggplot() +
    geom_point(data = spat_summ_df, aes_string(x = "Date", y = plot_variable, color = "Region")) +
    geom_line(data = spat_summ_df, aes_string(x = "Date", y = plot_variable, color = "Region")) +
    scale_color_manual(values = colors_use) +
    scale_x_date(breaks = date_breaks, date_labels = "%Y") +
    xlab({{nice_xlab}}) +
    ylab({{nice_ylab}}) +
    ggtitle(paste0(unique(spat_summ_df$Species), " ", unique(spat_summ_df$Climate_Scenario))) + 
    theme_bw() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  
  # Save and return the plot
  ggsave(filename = paste0(out_dir, "/", unique(spat_summ_df$Species), "_", unique(spat_summ_df$Climate_Scenario), "_", plot_variable, "_annual_summary.png"), plot_out, width = 8, height = 11, units = "in")
  return(plot_out)
}
