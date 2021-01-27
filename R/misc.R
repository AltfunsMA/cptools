# R 3.6.2.
# Alfonso Martinez Arranz
# General utility functions and those used by other functions

#' Match column classes of df before joining
#'
#' @param df1 
#' @param df2 
#'
#' @return df2 with its shared columns (the ones with the same name) in the same class as df1
#' @export
#'
#' @examples
match_col_classes <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}



#' Remove list columns
#'
#' @param x An R object with the class "data.frame"; others will be ignored and
#' the original will be silently returned untouched.
#'
#' @return The same object as x but without the list columns
#' @export
rm_list_cols <- function(x) {

  if(!inherits(x, "data.frame")) {return(x)} 
  
  # SF geometry will not, by design, be removed by select_if below
  if (identical(class(x)[1], "sf")) {sf::st_geometry(x) <- NULL}

  # as_tibble allows for prettier printing, etc. and is very useful precisely in the
  # context of sf objects
   select_if(x, negate(is.list)) %>% 
     as_tibble() 

}


#' Rename states
#'
#' @param df dataframe with a column containing Indian states (automatically found by \code{find_st_name_col}), potentially with 
#' older names like 'Orissa', 'Pondicherry', etc.
#'
#' @return The same dataframe but with STATE/ST_NAME columm recoded 
#' @export
#'
rename_states <- function(df) {
  
  col <- sym(find_st_name_col(df))
  
  # TODO: fuzzy matching with list of correct names

  # From common mistakes already observed
  straightforw <- c("Arunanchal Pradesh" = "Arunachal Pradesh",
                    "Himachal Pradeshh" = "Himachal Pradesh",
                    "Orissa" = "Odisha",
                    "Pondicherry" = "Puducherry",
                    "Jammu and Kashmir" = "Jammu & Kashmir")
  
  delhi <- function(v) {
    
    v[grepl("Delh", v)] <- "Delhi"
    
    v
  }
  
  df %>% 
    mutate({{ col }} := recode({{ col }}, !!! straightforw),
           {{col}} := fct_relabel({{col}}, delhi))
  
  
}

#' Transform character strings into one regular expression
#' 
#' @description Passing a list of patterns to a `stringr` function that do not match the length of the `text` argument yields unexpected results.
#' Converting the expression with a `map` function is cumbersome and sometimes slow.
#' This transformation solves that problem and may provide an increase in speed 
#' Using `fixed()` and a `map`function may be faster but will match different things. 
#' 
#' @param rx_var A charater vector
#' @param by_element 
#'
#' @return An atomic character vector with a valid regex
#' @export
#'
#' @examples
#' 
#' Find all individual letters in a text regardless of case.
#' 
#' sample_text <- c("ag;als", "w a;lsg;lak B", "l A jlajksf k")
#' 
#' str_count(sample_text, bound_rx(letters)
bound_rx <- function(rx_var, leftbound = "\\b", rightbound = "\\b", 
                     by_element = FALSE) {
  
  if(!is.character(rx_var)) 
  {stop("The supplied argument is not a character vector")}
  
  rx_var <- gsub("\n", "", rx_var)
  
  leftmost <- paste0("(", leftbound)
  rightmost <- paste0(rightbound, ")")
  collapse_str = paste0(rightbound, "|", leftbound)
  
  add_bounds <- function(s)  paste0(leftmost, s, rightmost)
  
  
  if(length(rx_var) > 1 && by_element == FALSE) {
    
    add_bounds(paste0(rx_var, collapse = collapse_str))
    
  } else if (length(rx_var) > 1 && by_element == TRUE) {
    
    add_bounds(rx_var)
    
  }
  
  else if(is.na(rx_var)) {return(rx_var)}
  

  else if(grepl("(?", rx_var, fixed = T)) {
    
    warning("Bound_rx detected lookarounds, string supplied untouched.")
    
    rx_var
    
  }
  
  
  else if(grepl("\\|", rx_var)) 
    
  { stringr::str_replace_all(rx_var,  stringr::fixed(c("|" = "\\b|\\b",
                                                       "(" = "(\\b",
                                                       ")" = "\\b)")
  )
  )
  }
  
  
  else {add_bounds(rx_var)}
  
  
}



#' Re-attaches package (restarting session)
#'
#' @param pkg An R package as a string 
#' (this was designed to quickly fix and test my own packages)
#'
#' @return By default, it restarts R and reattaches \code{cptools}.
#' Otherwise, it detaches the named package if it is loaded 
#' and tries to attach it again.
#' @export
#'
#' @examples
#' # Another mistake in my own package
#' # (fixing it)
#' reattach_package(cptools)
restart_reattach <- function(pkg= NULL) {

  reattach <- function(pkg) {
  
  search_item <- paste("package", pkg, sep = ":")
  while (search_item %in% search())
  {
    detach(search_item,
           unload = TRUE,
           character.only = TRUE)
  }
  library(pkg, character.only = TRUE)
  
  }
  
  if(is.null(pkg)) {
    pkg <- "cptools"
    rstudioapi::restartSession("library(cptools)")
    
  }
  
  else(reattach(pkg))
  
}


#' Find the column containing state names
#'
#' @param df A dataframe with one or more columns containing Indian state names
#'
#' @return The name of the column as a string literal.
#' 
#' @details The function first checks if "ST_NAME" or "STATE" are present
#' preferring always one of those two, and always in that order. Otherwise,
#' it will search in character columns for those containing the word "Pradesh"
#' (Hindi for "state" or "province").
#' @export
find_st_name_col <- function(df) {
  
  # The two most common in order of preference
  # This function is called to remove non-CP states
  for (i in c("^ST_NAME$", "^STATE$")) {
   
     if (any(str_detect(names(df), regex(i, ignore_case = TRUE))))
    {
      {
        state_vector <- str_extract(names(df), regex(i, ignore_case = TRUE))
        
        out <- state_vector[!is.na(state_vector)]
        
        if (sum(is.na(pull(df, out))) > 0)  {
          message(
            "State column contains NA values.",
            "This may affect the results of other cptools functions \n"
          )
          
        }
        
        return(out)
        
      }
      
    }
    
  }
  
  
  sample_state_name <- "\\bPradesh"
  
  df_search <- df %>%
    rm_list_cols() %>%
    mutate_if(is.factor, as.character) %>%
    select_if(is.character)
  
  rm(df)
  
  cols_w_state_name <- map_lgl(df_search,
                               ~any(str_detect(.x, regex(sample_state_name,
                                                         ignore_case = TRUE))
                               )
  ) %>% 
    # the "select_if" above also returns columns that are all NAs
    replace_na(FALSE) 
  
  
  state_col_vector <- names(df_search)[cols_w_state_name]
  
  if(length(state_col_vector) > 1) {
    cat("No columns named STATE or ST_NAME found. The following columns contained 'Pradesh': \n") 
    cat(state_col_vector, "\n", sep =", ")
    cat(state_col_vector[1], "used.  \n")
  }
  
  state_col_vector[1]
  
}



#' Replace NA in .x  cols by .y cols (or vice versa) after incomplete *_join 
#'
#' @param tbl A tibble or data_frame
#' @param vars If \code{NULL}, function searches for variables with .x/.y suffixes.
#' Alternatively, a character vector of names of variables that are duplicated
#' can be provided. This Useful when operating with sf objects because 'geometry'
#' column will throw an error with the automated search.
#' These variable must be entered without .x or .y suffixes.
#' @param ending_to_keep Choose which column suffix should be kept for rows
#' where both columns have values. Currently only implemented for .x/.y.
#'
#' @return A tibble with merged columns
#' @export
replace_xy <- function(tbl, vars = NULL, ending_to_keep = ".x"){

  # Generate a character vector with only variable names that need to be 
  # deduplicated
  if(is.null(vars)) {

    vars <- tbl %>%
      rm_list_cols() %>% 
      dplyr::select(dplyr::contains(ending_to_keep)) %>%
      dplyr::rename_all(~stringr::str_replace(., stringr::fixed(ending_to_keep), "")) %>%
      colnames() %>%
      unique()

  }

  
  if(length(vars) < 1)
    {
    warning(cat("There were no duplicates ending in", ending_to_keep, ". \n",
                "Check any preceding '*_join' and consider removing this function call \n"),
            .call = TRUE)
    }

  if(ending_to_keep == ".x") {
    keep <- ".x"
  eliminate <- ".y"} else {
    keep <- ".y"
    eliminate <- ".x"
  }
  
 
  # 'transfer' variables with ending to keep to name without ending
  
  for(var in vars){
    
    var_transfer <- paste0(var, keep)
    var_eliminate <- paste0(var, eliminate)
  
  
    tbl <- tbl %>%
      # Coalesce throws errors when types are different
      mutate_if(is.factor, as.character) %>% 
      mutate(!!sym(var) := dplyr::coalesce(!!sym(var_transfer),
                             !!sym(var_eliminate))) %>%
      dplyr::select(-all_of(c(var_transfer, var_eliminate)))
  }
  
    tbl
}


