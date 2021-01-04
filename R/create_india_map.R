# R version 3.6.3 (2020-02-29)
# author: Alfonso Martínez Arranz
# 2020-04-14 10:33:55

#' Create maps of India from sf objects with sensible defaults
#'
#' @param df An dataframe with a simple features geometry column
#' @param title The title of the plot
#' @param subtitle 
#' @param fill_var The variable to fill the text with. If empty, it takes the 
#' relevant State variable.
#' @param fill_var_text Variable as a string. The labelling to be applied to each observation in df
#' @param state_labels One of "none", "type" or "acronym" to indicate 
#' how states should be labelled. "Type" depicts a letter depending on whether
#' the state is considered 
#' Backward (B), Conflictual (C), Med. HDI (M) or High HDI (H).
#' "Acronym" uses the official abbreviation for
#' each state.  
#' @param state_labels_colour Value passed to \code{geom_sf(colour = )}
#' @param district_display A choice of whether to have only borders ("borders"), no borders
#' and no labels ("none"), or both ("labels"). It defaults to "borders".
#' @param district_label_colour Value passed to\code{geom_sf_text(colour =)}
#' @param cities_w_pop Numeric value indicating the minimum population size of
#' settlements to be plotted. None plotted with default \code{NULL}. 
#' @param pp_w_MW Numeric value indicating the minimum capacity size of
#' power plants to be plotted. None plotted with default \code{NULL}.
#' @param legend_omit Boolean.
#' @param legend_scalar To tweak the size of the legend box.
#' @param text_scalar To tweak the sizes of most elements.
#' @param legend_fill Passed on to \code{theme(legend.background)}
#' @param leg_just Passed on to \code{theme(legend.justification)}
#' @param leg_x Passed on to \code{theme(legend.position)}
#' @param leg_y Passed on to \code{theme(legend.position)}
#' @param background_colour Passed on to \code{theme(panel.background)}
#'
#' @return A ggplot object
#' @export
#' 
create_india_map <-  function(df, 
                              title = NULL, subtitle = NULL, 
                              fill_var = NULL,
                              fill_var_text = NULL,
                              cities_w_pop = NULL, 
                              pp_w_MW = NULL, 
                              state_labels = "acronym",
                              state_labels_colour = "black",
                              district_display = "none",
                              district_label_colour = "white",
                              background_colour = "antiquewhite1",
                              legend_omit = FALSE,
                              legend_scalar = 2, text_scalar = 1,
                              legend_fill = "transparent",
                              leg_just = c("right", "bottom"),
                              leg_x = 0.85, leg_y = 0.05,
                              show_disputed = FALSE) {
  
  library(ggrepel)
  library(ggplot2)
  library(cptools)
  
  # throw error early if variable doesn't exist
  
  if(!is.null(fill_var)) {
    
    invisible(select(df, fill_var)) 
      }
  
  
  
  # Calculate scale of map-------
  
  
  state_col_name <- find_st_name_col(df)
  
  states_to_plot <- unique(pull(df, state_col_name))
  
  need_nation <- length(states_to_plot) > 10
  need_states <- length(states_to_plot) > 1
  
  if(!need_states) {
    
    if(is.null(title)) {
      
      title <- states_to_plot
      state_labels <- "none"
    
    }
     
  }
  
  # Get supporting maps----
  supporting_maps <- get_supporting_maps(states_to_plot = states_to_plot,
                       need_nation = need_nation,
                       need_states = need_states,
                       cities_w_pop = cities_w_pop, 
                       pp_w_MW = pp_w_MW,
                       show_disputed = show_disputed)

  
  nation <- supporting_maps[["nation"]]
  states <- supporting_maps[["states"]]
  districts <- supporting_maps[["districts"]]
  power_plants <- supporting_maps[["pp"]]
  settlements <- supporting_maps[["sett"]]
  disputed <- supporting_maps[["disputed"]]
  
  
  # Labelling geoms----
  
  
  acronym_geoms <- function() {
    # Label states that are not usually part of CP in a different colour
    # and smaller font
    
    cp_states <- states %>% 
      filter_cpw1_states()
    
    noncp_states <- states %>% 
      anti_join(mutate_if(rm_list_cols(cp_states), 
                          is.factor, as.character), 
                by = "STATE") #  doesn't work with sf directly
    
    list(
      if(nrow(noncp_states) > 0) 
        {geom_text_repel(data = noncp_states, 
                       aes(x = LON, y = LAT, label = ACRONYM), 
                       color = "grey30",
                       fontface = "bold",
                       size = 3.25*text_scalar,
                       force = 0.75)},
      
      if(nrow(cp_states) > 0) {
        geom_text_repel(data = cp_states, 
                       aes(x = LON, y = LAT, label = ACRONYM,
                           color = Gov2014), 
                       fontface = "bold",
                       size = 4*text_scalar,
                       force = 0.01)} 
      )
  }
  
  state_types_geoms <- function() {
    
    groupings <- c("Backward (B)", "Conflictual (C)", 
                   "Med. HDI (M)", "High HDI (H)")
    abbreviations <- c("B", "C", "M", "H")
    
    names(abbreviations) <- groupings
    
    map(unique(states$STATE_GROUPING), 
         ~geom_text_repel(
           data = subset(states,
                         STATE_GROUPING == .x),
           aes(x = LON, y = LAT, label = abbreviations[.x]),
           # Label represented states
           color = state_labels_colour,
           fontface = "bold",
           size = 4.5 * text_scalar,
           force = 0.01
         )
    )
    
  }
  
  if(is.null(fill_var)) {fill_var <- sym(state_col_name)} 
  else {fill_var <- sym(fill_var)} 
  
  
  if ("AC_NO" %in% str_to_upper(names(df))) {lowest_leg_label <- "AC boundary"}
  else {lowest_leg_label <- "District-region boundary"}
  
  if(all(is.na(states_to_plot))) {
    
    need_states <- TRUE
    # If using default value when States actually all NA, a default
    # will be better
    if(sym(fill_var) == "ST_NAME") {fill_var <- "grey10"}
    
  }

  ggplot(df) +
    
    # Labels
    
    labs(
      title = title,
      subtitle = subtitle,
      colour = "Boundaries",
      size = "Megawatts (power plants)",
      shape = "Large cities"
    ) +
    
    # Disputed areas
    
    {if (show_disputed) 
      geom_sf(data = disputed,
              fill = "beige",
              colour = "mistyrose2")} +
    
    # Filling variable
    
    geom_sf(aes(fill = !!fill_var, colour = "Lowest"), 
            size = 0.4 * text_scalar) +
    {if(!is.null(fill_var_text)) geom_sf_text(aes_(label = df[[fill_var_text]]),
                                               colour = "white")} +
    
    
    # Borders
    
    {if(str_to_lower(district_display) != "none") list(geom_sf(
      data = districts,
      aes(colour = "District"),
      fill = NA,
      size = 0.4 * text_scalar))} + 
    {if (need_states) list(geom_sf(
          data = states,
          aes(colour = Gov2014),
          fill = NA,
          size = 0.6 * text_scalar
        ))} + 
    {if (need_nation)
        list(geom_sf(
          data = nation,
          color = "mistyrose2",
          fill = NA,
          size = 0.6 * text_scalar
        ))} +
    scale_colour_manual(
      values = c(
        "State" = "black",
        "District" = "grey50",
        "Lowest" = "grey70"
      ),
      labels = c(
        "State" = "State boundary",
        "District" = "District boundary",
        "Lowest" = lowest_leg_label
      )
    ) +
  # Cities, power plants, labels
    {if (!is.null(cities_w_pop))
        list(
          geom_point(
            data = settlements,
            aes(x = lon, y = lat, shape = "Cities"),
            colour = "blue"
          ),
          scale_shape_manual(
            labels = c("Cities" = "> 500,000 hab."),
            values = c("Cities" = 11)
          )
        )} + {if (!is.null(pp_w_MW))
        list(geom_point(
          data = power_plants,
          aes(x = longitude, y = latitude, size = capacity_m),
          colour = "blue",
          alpha = 0.3
        ))} + {
      switch(
        state_labels,
        "none" = {},
        "type" = {state_types_geoms()},
        "acronym" = {acronym_geoms()}
      )} +  {if(str_to_lower(district_display) == "labels")
        list(geom_sf_text(data = districts,
                          aes(label = DISTRICT_NAME),
                          color = district_label_colour,
                          size = 3.5 * text_scalar
        ))} +

    # Themes

    theme_minimal() +
    # If inside the map, position legend accordingly
    {if(!is.null(leg_x) && !is.null(leg_y))
      theme(legend.position = c(leg_x, leg_y),
            legend.justification = leg_just,
            legend.text = element_text(size = 4*legend_scalar,
                                       face = "bold"),
            legend.title = element_text(size = 3.5*legend_scalar,
                                        face = "bold"),
            legend.margin = margin(1*legend_scalar, 1*legend_scalar,
                                   1*legend_scalar, 2*legend_scalar),
            legend.spacing = unit(0, "pt"),
            legend.key.size = unit(0.3*legend_scalar, "cm"),
            legend.background = element_rect(fill = legend_fill,
                                             colour = "transparent")
      ) } + {if (legend_omit) theme(legend.position = "none")} + # If legend unnecessary
    # Remove usual plot lines
        theme(
      panel.grid.major = element_line(colour = "transparent"),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = background_colour)
    ) +
    guides( # To order the appearance of legends
      colour = guide_legend(order = 1, override.aes = list(size = 3)),
      fill = guide_legend(order = 2),
      size = guide_legend(order = 3),
      shape = guide_legend(override.aes = list(size = 3))
    )
  }
  
