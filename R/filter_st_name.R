filter_st_name <- function(df, states, 
                           exclude = TRUE, state_col_name = NULL,
                           warn = TRUE, df_nm = "df") {
  
  # states <- rlang::enquos(states)
  
  # if(is.character(rlang::eval_tidy(states))) {
    
       if(length(states) > 1) {
          several <- states
          states <- "several"}
        
      if(length(states) > 1) {
        several <- states
        states <- "several"}
    
    
      # If called from other cptools functions, they know what they should be selecting
      if(is.na(states)) {
        return(df)
        }
      
      poor_coverage <- c("Jammu & Kashmir", "Uttarakhand")
      
      not_in_cpw1 = c("Arunachal Pradesh", "Nagaland",
                      "Manipur", "Mizoram", "Tripura",
                      "Meghalaya", "Sikkim")
      
      northeast <- c("Assam", not_in_cpw1)
      
      small_rich = c("Goa", "Delhi", "Puducherry", "Pondicherry", "Chandigarh")
      
      no_vidhan = c("Lakashdweep", "Daman", "Diu",  "Dadra", "Nagar Haveli", 
                    "Andaman", "Nicobar", "Chandigarh")
      
      
      states_to_filter <- switch (states,
                           poor_coverage = poor_coverage,   
                           not_in_cpw1 = not_in_cpw1,
                           small_rich = small_rich,
                           no_vidhan = no_vidhan,
                           all_above = c(poor_coverage, not_in_cpw1, small_rich, no_vidhan, northeast),
                           several = several,
                           states
      ) %>% 
        bound_rx(leftbound = "", rightbound = "")
      
  # } else {
  #   
  #   
  #   rlang::as_label(states)
  #   
  # }
  
  if(is.null(state_col_name)) {state_col_name <- find_st_name_col(df, df_nm)}
  
  buscar <- function(strings, exclude) {
    
      stringi::stri_detect_regex(strings, states_to_filter, 
                                 case_insensitive = TRUE, 
                                 negate = exclude)
      
    }
    

    filter_at(df, vars(all_of(state_col_name)),
              any_vars(buscar(., exclude)))
    

}



#' Exclude (types of) Indian states or Union Territories

#' @param df An object with class data.frame that has ONE column of Indian states.
#' @param state_col_name If more than column is known to be present, introduce
#' the name of the correct column here as a string.
#' @param states Accepts either a character vector of state names, which will be made into a regex with \code{bound_rx(vector, "", "")} and ignore case, or the following options: 
#' \itemize{
#' \item{\code{"poor_coverage"}: Jammu & Kashmir and Uttarakhand because of frequent problems in maps and dataset coverage}
#'  \item{\code{"not_in_cpw1"} for those not in Consumer Pyramids Wave 1}
#'  \item{\code{"small_rich"}: Goa, Delhi, Puducherry and Chandigarh}
#'  \item{\code{"no_vidhan"}: those Union Territories without legislative assembly}
#'  \item{\code{"northeast"}: Assam, Arunachal Pradesh, Nagaland, Manipur, Mizoram, Tripura, Meghalaya, Sikkim)}
#'  \item{\code{"all_above"}: all states in previous options}
#' }
#' 
#'
#'
#' @return The same object WITHOUT rows referring to the relevant states
#'
#' #'
#' @return The same data.frame object WITHOUT rows referring to the relevant states
#' @export
#' 
#'
exclude_states <- function(df, states, state_col_name = NULL) {
  
  df_nm <- deparse(substitute(df))
  
  cptools:::filter_st_name(df = df, states = states, state_col_name = state_col_name,
                 exclude = TRUE, df_nm = df_nm)
  
  
}


#' Select only rows with given (types of) Indian states or Union Territories
#'
#' @inheritParams exclude_states  
#'
#' @return The same data.frame object WITH ONLY rows referring to the relevant states
#' @export
#'
include_states <- function(df, states, state_col_name = NULL) {
  
  df_nm <- deparse(substitute(df))
  
  cptools:::filter_st_name(df = df, states = states, state_col_name = state_col_name,
                 exclude = FALSE, df_nm = df_nm)
}
  
  


#' Exclude states missing in Consumer Pyramids Wave 1
#'
#' @inheritParams exclude_states
#'
#' @return The same object withouth rows referring to 
#' Arunachal Pradesh, Nagaland, Manipur, Mizoram, Tripura, Meghalaya, Sikkim
#' @export

filter_cpw1_states <- function(df, state_col_name = NULL) {
  
  df_nm <- deparse(substitute(df))
  
  cptools:::filter_st_name(df, states = "not_in_cpw1", state_col_name = NULL, 
                           df_nm = df_nm)
  
}



