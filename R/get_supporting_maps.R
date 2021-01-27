# Load auxiliary ~/Projects/india-energy-poverty/Datasets for create-india_maps

# Map objects are loaded in full onto the global environment, 
# but then also filtered as needed


get_supporting_maps <- function(states_to_plot, need_nation, need_states,
                                 cities_w_pop, pp_w_MW, show_disputed) {
  
  elegir_estados <- function(supporting_map) {
    supporting_map %>% 
      filter(STATE %in% states_to_plot)
  }
  
  filepath <- "~/Projects/india-energy-poverty/"
  
  st_read_quiet <- function(filename) {
    
    st_read(paste0(filepath, filename),
            quiet = TRUE)
    
  }
   
  
  if ("Jammu and Kashmir" %in% states_to_plot && show_disputed) {
    
    disputed <- st_read_quiet("Processed/admin_maps/GADM/pok.gpkg")
    
  } else {disputed <- NULL}
  
  if (!exists("GADM2018_states"))
  {
    
    GADM2018_states <<- st_read_quiet("Processed/admin_maps/GADM/GADM2018_states.gpkg")
  
  }

  if(all(is.na(states_to_plot))) {
    
    message("No states to plot found or all missing values.\n 
Plotting all states in GADM. \n")
    states_to_plot = unique(GADM2018_states$STATE)
    
    }
  
  states <- GADM2018_states %>% elegir_estados() 

  # Loading main datasets
  if(!exists("nation") & need_nation) 
  {
    nation <<- st_read_quiet("Processed/admin_maps/GADM/GADM2018_border.gpkg")
    
  } else {nation <- NULL}
  

  if(!exists("GADM2018_dist"))
  {GADM2018_dist <<- st_read_quiet("Processed/admin_maps/GADM/GADM2018_dist.gpkg")}
  
 districts <- GADM2018_dist %>% elegir_estados()
 
  
  # Loading add-ons
  calc_inside <- function(original_df) {

    inside_states_lgl <- original_df %>%
      st_within(states) %>%
      lengths() > 0 # Yields a list where length 0 means no match
    
    out_df <- original_df %>%
      filter(inside_states_lgl) %>%
      mutate(lon = map_dbl(geometry, 1),
             lat = map_dbl(geometry, 2)) %>%
      st_set_geometry(NULL)

    if(interactive()){
      
      if(nrow(out_df) > 1000) {
        
        message("Attempting to plot", nrow(out_df), "individual points. \n
          Are you sure? \n")
        
        invisible(readline(prompt="Press [Enter] to continue, [Esc] to cancel"))
        
      }
      
    }
    
    out_df
        
  }
  
  if(!is.null(cities_w_pop)) 
    {
    
    if (!exists("sett_original"))
        {sett_original <<- st_read_quiet("Original/DS_Indian_GeoFeatures/Geonames_settlements.gpkg")}
    
    settlements <- sett_original %>% 
      filter(pop > cities_w_pop) %>% 
      calc_inside()
    
    } else {settlements <- NULL}
  
  if(!is.null(pp_w_MW)) 
    {

    if(!exists("power_original")) 
      {power_original <<- st_read_quiet("Original/DS_PowerPlant/Ind_power_plants.shp")} 
    
    power_plants <- power_original %>% 
      filter(capacity_m > pp_w_MW) %>% 
      calc_inside()
    
  
    
    
  } else {power_plants <- NULL}
  
  list(
    "disputed" = disputed,
    "nation" = nation,
    "states" = states,
    "districts" = districts,
    "pp" = power_plants,
    "sett" = settlements
  )
  
  
  }
  
