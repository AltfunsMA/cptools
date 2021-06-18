#' @import rlang
filter_st_name <- function(df, input, 
                           exclude = TRUE, state_col_name = NULL,
                           warn = TRUE, df_nm = "df") {
  
  
  if (length(input) == 1 && is.na(input)) {
    
    # when passing from odd_val_col or other internal functions
    
    return(df)
    
  }
  
  if(is.null(state_col_name)) {state_col_name <- find_st_name_col(df, df_nm)}
  
  if(is.null(state_col_name)) { 
    
    stop("No column with states found.") 
    
  } 
  
  
  if(inherits(input, "quosures")) {

    expressions <- lapply(input, rlang::quo_get_expr)
    
    
    types <- sapply(expressions, typeof)
    
    
    str_from_symbols <- sapply(expressions[types == "symbol"], rlang::as_name)
    

    if(length(str_from_symbols) > 0) {
      
      
      str_from_objects <- mget(str_from_symbols, ifnotfound = list(NULL), 
                               envir = globalenv())
      
      confirmed_symbols <- sapply(str_from_objects, is.null)
      
      str_from_symbols_confirmed <- str_from_symbols[confirmed_symbols]
      
      
        str_from_objects[confirmed_symbols] <- NULL
      
        confirmed_char_obj <- sapply(str_from_objects, is.character)
      
      if(length(confirmed_char_obj) > 0) {
        
        str_from_objects_cleaned <- str_from_objects[confirmed_char_obj]

        non_char_obj <- str_from_objects[!confirmed_char_obj]
        
            if(length(non_char_obj) > 0) {
              
              warning("You have passed the following symbol(s) that refer to non-character object(s) in the global environment. ",
                      "Perhaps you named an object after a location? \n",
                      paste0(" - ", names(non_char_obj), sep = "\n"),
                      "The function assumed you meant them as normal location names and used them for filtering.")
            }
        
        } else {
          
          str_from_objects_cleaned <- NULL
          
          non_char_obj <- NULL
          
        }
      

      str_from_symbols <- c(unlist(str_from_objects_cleaned), 
                            str_from_symbols_confirmed,
                            names(non_char_obj))

    }
    
    str_from_languages <- sapply(expressions[types == "language"], rlang::eval_tidy)
    
    str_from_character <- sapply(expressions[types == "character"], as.character)
    
    
    
    if(types == "NULL" || 
      (types == "language" && length(unlist(str_from_languages)) == 0)) {
      
      warning("NULL value or empty vector supplied. No rows returned.")
      
      return(df[FALSE,])
      
    } 
    
    
    poor_coverage <- c("Jammu & Kashmir", "Uttarakhand")
    
    not_in_cpw1 = c("Arunachal Pradesh", "Nagaland",
                    "Manipur", "Mizoram", "Tripura",
                    "Meghalaya", "Sikkim")
    
    northeast <- c("Assam", not_in_cpw1)
    
    small_rich = c("Goa", "Delhi", "Puducherry", "Pondicherry", "Chandigarh")
    
    no_vidhan = c("Lakashdweep", "Daman", "Diu",  "Dadra", "Nagar Haveli", 
                  "Andaman", "Nicobar", "Chandigarh")
    
    all_above <- c(poor_coverage, not_in_cpw1, northeast, small_rich, no_vidhan)
    
    
    option_nm <- c("poor_coverage", "not_in_cpw1", "northeast", "small_rich", 
                       "no_vidhan", "all_above")
    
    
    all_options <- list(poor_coverage, not_in_cpw1, northeast,
                        small_rich, no_vidhan, all_above)
    
    names(all_options) <- option_nm
    
    
    final_strings <- unlist(c(str_from_symbols, 
                              str_from_character, str_from_languages))
    
    
    selected_options <- grep(cptools:::bound_rx(option_nm), 
                             final_strings, value = T)
    
    if(length(selected_options) == 0) selected_options <- ""
    
    # browser()

    states_to_filter <- bound_rx(
      c(
        final_strings[!final_strings %in% selected_options],
        all_options[[selected_options]]
        ),
    leftbound = "", rightbound = "")
    
    
  }  else {
    
    warning("This function should not be called directly. Use 'include/exclude_states' ")
    
    return(df)
    
  }

  

  if(exclude) lgl_fun <- Negate(grepl) else lgl_fun <- grepl
  
  # reflects tidyverse 'filter' behaviour
  out <- df[!is.na(df[[state_col_name]]),] 
  

  rows_selected <- lgl_fun(states_to_filter, out[[state_col_name]], ignore.case = TRUE)
    
  out[rows_selected, ]
  

}



#' Exclude (types of) Indian states or Union Territories

#' @param df An object with class data.frame that has ONE column of Indian states.
#' @param state_col_name If more than column is known to be present, introduce
#' the name of the correct column here as a string.
#' @param ... Accepts symbols or a character vector of state names, which will be made into a regex with \code{bound_rx(vector, "", "")} and ignore case. The following options: 
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
exclude_states <- function(df, ..., state_col_name = NULL) {
  
  df_nm <- deparse(substitute(df))
  
  states <- rlang::enquos(...)
  
  cptools:::filter_st_name(df = df, states, state_col_name = state_col_name,
                 exclude = TRUE, df_nm = df_nm)
  
  
}


#' Select only rows with given (types of) Indian states or Union Territories
#'
#' @inheritParams exclude_states  
#'
#' @return The same data.frame object WITH ONLY rows referring to the relevant states
#' @export
#'
include_states <- function(df, ..., state_col_name = NULL) {
  
  df_nm <- deparse(substitute(df))

  states <- rlang::enquos(...)
  
  cptools:::filter_st_name(df = df, states, state_col_name = state_col_name,
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
  
  states <- rlang::quos(not_in_cpw1)
  
  cptools:::filter_st_name(df, input = states, 
                           state_col_name = NULL, 
                           df_nm = df_nm)
  
}



