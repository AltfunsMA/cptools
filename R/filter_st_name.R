#' @import rlang
filter_st_name <- function(df, input, 
                           exclude = TRUE, state_col_name = NULL,
                           warn = TRUE, df_nm = "df", 
                           fixed = FALSE, ignore_case = TRUE) {
  
  
  
  if(is.null(state_col_name)) {state_col_name <- find_st_name(df, df_nm)}
  
  if(is.null(state_col_name)) { 
    
    stop("No column with states found.") 
    
  } 
  

  if(length(input) == 0) {
    
    stop("No selection passed", immediate. = T, call. = F)
    
    
  } else if(inherits(input, "quosures")) {

    expressions <- lapply(input, rlang::quo_get_expr)
    
    
    types <- vapply(expressions, typeof, character(1))
    
    
    str_from_symbols <- vapply(expressions[types == "symbol"], rlang::as_name, character(1))
    

    if(length(str_from_symbols) > 0) {
      
      
      str_from_objects <- mget(str_from_symbols, ifnotfound = list(NULL), inherits = T)
      
      confirmed_symbols <- vapply(str_from_objects, is.null, logical(1))
      
      str_from_symbols_confirmed <- str_from_symbols[confirmed_symbols]
      
      
        str_from_objects[confirmed_symbols] <- NULL
      
        confirmed_char_obj <- vapply(str_from_objects, is.character, logical(1))
      
      if(length(confirmed_char_obj) > 0) {
        
        str_from_objects_cleaned <- str_from_objects[confirmed_char_obj]
      


        non_char_obj <- str_from_objects[!confirmed_char_obj]
        
            if(length(non_char_obj) > 0) {
              
              warning("You have passed the following symbol(s) that refer to non-character object(s) in one of your environments.",
                      paste0(" - ", names(non_char_obj), sep = "\n"),
                      "We have used them as literal location names for filtering. Was this your intention?")
            }
        
        } else {
          
          str_from_objects_cleaned <- NULL
          
          non_char_obj <- NULL
          
        }
      

      str_from_symbols <- c(unlist(str_from_objects_cleaned), 
                            str_from_symbols_confirmed,
                            names(non_char_obj))

    }
    
    str_from_languages <- sapply(expressions[types == "language"], 
                                 rlang::eval_tidy)
    
    str_from_character <- vapply(expressions[types == "character"], 
                                 as.character, character(1))
    
    
    
    if(types == "NULL" || 
      (types == "language" && length(unlist(str_from_languages)) == 0)) {
      
      warning("NULL value or empty vector supplied. No rows returned.", call. = FALSE)
      
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
    
    
    selected_options <- grep(bound_rx(option_nm), 
                             final_strings, value = T)
    
    if(length(selected_options) == 0) selected_options <- ""
    
    # browser()
    str_to_filter <- c(final_strings[!final_strings %in% selected_options],all_options[[selected_options]])
    
    
    if(length(str_to_filter) == 0) {
      
      warning("The arguments passed evaluated to an empty string. Returning full data.frame...", 
              call. = FALSE)
      
      if(length(str_from_objects) > 0) {
        
        warning("Perhaps check the following objects:\n", 
                paste0(" - ", names(str_from_objects), sep = "\n"), call. = FALSE)
        
      }
      
      return(df)
      
      
    }
    states_to_filter <- bound_rx(str_to_filter, leftbound = "", rightbound = "")
    
    
    
  }  else {
    
    warning("This function should not be called directly. Use 'include/exclude_states' ")
    
    return(df)
    
  }

  

  if(exclude) lgl_fun <- Negate(grepl) else lgl_fun <- grepl
  
  # reflects tidyverse 'filter' behaviour
  out <- df[!is.na(df[[state_col_name]]),] 
  

  rows_selected <- lgl_fun(states_to_filter, out[[state_col_name]], 
                           fixed = fixed,
                           ignore.case = ignore_case)
    
  out[rows_selected, ]
  

}



#' Exclude (types of) Indian states or Union Territories

#' @param df An object with class data.frame that has a column of Indian states.
#'
#' @param state_col_name If more than column is known to be present, introduce
#'   the name of the correct column here as a string.
#' @param ... State or Union Territory names either in full or in part. Case
#'   insensitive. Accepts symbols (bare text) or quoted strings (see details
#'   below). If a symbol refers to a character vector in the global environment,
#'   the vector contents and not the symbol will be part of the search. The
#'   following options are special and can be used in addition to others both as
#'   strings and as symbols: \itemize{ \item{\code{"poor_coverage"}: Jammu &
#'   Kashmir and Uttarakhand because of frequent problems in maps and dataset
#'   coverage} \item{\code{"not_in_cpw1"} for those not in Consumer Pyramids
#'   Wave 1} \item{\code{"small_rich"}: Goa, Delhi, Puducherry and Chandigarh}
#'   \item{\code{"no_vidhan"}: those Union Territories without legislative
#'   assembly} \item{\code{"northeast"}: Assam, Arunachal Pradesh, Nagaland,
#'   Manipur, Mizoram, Tripura, Meghalaya, Sikkim)} \item{\code{"all_above"}:
#'   all states in previous options} }
#' @param fixed should the names be matched strictly.
#' @param ignore_case should case be ignored.
#'
#' @details The elements from ... are processed and coerced into a string vector
#' that is then turned into an atomic regex by \code{cptools::bound_rx(vector,
#'   "", "")}, and finally passed to \code{grepl}.
#'
#'   \code{exclude_states(df, and, "Mizoram", my_states)} will exclude Andhra
#'   Pradesh, Mizoram and the contents of \code{my_states} if it is in the Global
#'   Environment
#'
#' @return The same object WITHOUT rows referring to the relevant states
#'
#'
#'
#' @export
#'
#' 
exclude_states <- function(df, ..., state_col_name = NULL, fixed = FALSE, ignore_case = TRUE) {
  
  df_nm <- deparse(substitute(df))
  
  states <- rlang::enquos(...)
  
filter_st_name(df = df, states, state_col_name = state_col_name,
                 exclude = TRUE, df_nm = df_nm)
  
  
}


#' Select only rows with given (types of) Indian states or Union Territories
#'
#' @inheritParams exclude_states  
#' @details The elements from ... are processed and coerced into a string
#'   vectorthat is then turned into a regex by \code{cptools::bound_rx(vector,
#'   "", "")} ignoring case.
#'
#'   `include_states(df, and, "Mizoram", my_states)` will include Andhra
#'   Pradesh, Mizoram and the contents of `my_states` if it is in the Global
#'   Environment
#'   
#' @return The same data.frame object WITH ONLY rows referring to the relevant states
#' @export
#'
include_states <- function(df, ..., state_col_name = NULL, fixed = FALSE, ignore_case = TRUE) {
  
  df_nm <- deparse(substitute(df))

  states <- rlang::enquos(...)
  
filter_st_name(df = df, states, state_col_name = state_col_name,
                 exclude = FALSE, df_nm = df_nm)
  
}
  




