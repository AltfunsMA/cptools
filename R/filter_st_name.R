#' Filtering types of Indian states and/or union territories
#'
#' @param df An object with class data.frame that has ONE column of Indian states
#' @param state_col_name If more than column is known to be present, introduce
#' the name of the correct column here as a string.
#' @param states What types of top level admin unit should be excluded. The 
#' default is to filter out those states that are not in Wave 1 
#' of Consumer Pyramids (but maintaining Union Territories if present). 
#' Other options are \code{"small_rich"} (Goa, Delhi, Puducherry and Chandigarh), 
#' \code{"no_vidhan"} (without legislative assembly), and \code{"all_above"}.
#' 
#' It also accepts a character vector of state names.
#' 
#' @param exclude Do we exclude the matches of state names or do we keep those?
#' 
#' @return The same object withouth rows referring to the relevant states
#' @export
#'
filter_st_name <- function(df, states = NA,
                           exclude = TRUE, state_col_name = NULL) {
  
  if(length(states) > 1) {
    several <- states
    states <- "several"}
  
  if(is.na(states)) {
    warning("Nothing to filter. Please see function help for options (e.g. 'small_rich')")
    return(df)
    }
  
  not_in_cpw1 = c("Arunachal Pradesh", "Nagaland",
                  "Manipur", "Mizoram", "Tripura",
                  "Meghalaya", "Sikkim")
  small_rich = c("Goa", "Delhi", "Puducherry", "Chandigarh")
  no_vidhan = c("Lakashdweep", "Daman and Diu", 
                "Dadra and Nagar Haveli", "Andaman and Nicobar",
                "Dadra and Nagar Haveli and Daman and Diu", 
                "Chandigarh")
  
  
  states_to_filter <- switch (states,
                       not_in_cpw1 = not_in_cpw1,
                       small_rich = small_rich,
                       no_vidhan = no_vidhan,
                       all_above = c(not_in_cpw1, small_rich, no_vidhan),
                       several = several,
                       states
  )
  
  
  if(is.null(state_col_name)) {state_col_name <- find_st_name_col(df)}
  
  buscar <- function(strings, exclude) {
    library(stringi)
    
    if (exclude) {
      map_lgl(strings, ~ !any((
        stri_detect_regex(.x, states_to_filter,
                          opts_regex = stri_opts_regex(case_insensitive = TRUE))
      )))
    }
    
    else {
      map_lgl(strings, ~ any(
        stri_detect_regex(.x, states_to_filter,
                          opts_regex = stri_opts_regex(case_insensitive = TRUE))
      ))
      
    }
    
  }

  
    filter_at(df, vars(all_of(state_col_name)),
              any_vars(buscar(., exclude)))
    
                         
}



#' Filtering out (types of) Indian states or Union Territories
#'
#' @inheritParams filter_st_name 
#'
#' @return The same data.frame object withouth rows referring to the relevant states
#' @export
#'
exclude_states <- function(df, states = NA, state_col_name = NULL) {
  
  
  filter_st_name(df = df, states = states, state_col_name = state_col_name,
                 exclude = TRUE)
}


#' Filtering for (types of) Indian states or Union Territories
#'
#' @inheritParams filter_st_name 
#'
#' @return The same data.frame object withouth rows referring to the relevant states
#' @export
#'
include_states <- function(df, states = NA, state_col_name = NULL) {
  
  
  filter_st_name(df = df, states = states, state_col_name = state_col_name,
                 exclude = FALSE)
}
  
  


#' Filtering out states not in Consumer Pyramids Wave 1
#'
#' @inheritParams filter_st_name
#'
#' @return The same object withouth rows referring to 
#' Arunachal Pradesh, Nagaland, Manipur, Mizoram, Tripura, Meghalaya, Sikkim
#' @export

filter_cpw1_states <- function(df, state_col_name = NULL) {
  
  filter_st_name(df, states = "not_in_cpw1", state_col_name = NULL)
  
}



