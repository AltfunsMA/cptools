# R 3.6.2.
# Alfonso Martinez Arranz
# General utility functions and those used by other functions


#' Return a filename-friendly string with the current time with the typical defaults
#'
#' @param format sprintf compliant. Passed to \code{format}.
#' @param tz OlsonName compliant string. Passed as "tzone" attribute to POSIXct object
#'
#' @return
#' @export
#'
#' @examples
#' 
#' paste0("/my/path/file_from_date_", date_str())
#' 
date_str <- function(format = "%Y%m%d_%H%M", tz = "Australia/Melbourne") {
  
  datetime <- Sys.time()
  
  attr(datetime, "tzone") <- tz
  
  format(datetime, format)
  
}


#' Split an R object into approximately equally sized chunks
#'
#' @param x A dataframe or another . A dataframe will be chunked by rows; other
#'   objects by length
#' @param chunk_size An integer value.
#'
#' @return A list. See 'help(split)' for details.
#' @export
#'
#' @examples
split_into_chunks <- function(x, chunk_size) {
  
  if(inherits(x, "data.frame")) {
    
    n <- nrow(x)
    
  } else {
    
    n <- length(x)  
    
  }
  
  r  <- rep(1:ceiling(n/chunk_size),each=chunk_size)[1:n]
  
  split(x, r)
  
}




#' Renames an sf object geometry column (e.g. to allow rbind to work)
#'
#' @param obj An sf object
#' @param name The new name
#'
#' @return An sf object
#' @export
#'
#' @examples
rename_geometry <- function(obj, name) {
  
  current = attr(obj, "sf_column")
  
  names(obj)[names(obj)==current] = name
  
  st_geometry(obj)=name
  
  obj
  
  
}






#' Match column classes of df before joining
#'
#' @param df1 
#' @param df2 
#'
#' @return df2 with its shared columns (the ones with the same name) in the same class as df1
#' @export
match_col_classes <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}



#' Remove list columns and return a tibble
#'
#' @param x An R object with the class "data.frame"; others will be ignored and
#'   the original will be silently returned untouched. Particularly useful for
#'   removing \code{sf} geometry columns which slow down operation and
#'   visualisation of other data in the dataframe
#'
#' @return The same object as x but without the list columns
#' @export
rm_list_cols <- function(x) {

  if(!inherits(x, "data.frame")) {return(x)} 
  
  # SF geometry will not, by design, be removed by select_if below
  if (identical(class(x)[1], "sf")) {

    # Remove the column and sf properties without using SF itself
    # so that a simple function like this doesn't depend on SF with all its baggage
    
    # sf::st_geometry(x) <- NULL
    
    geom_col <- attr(x, "sf_column")
    
    x[geom_col] <- NULL
    
    attr(x, "sf_column") <- NULL
    
    x <- as.data.frame(x)
    
    }

  # as_tibble allows for prettier printing, etc. and is very useful precisely in the
  # context of sf objects
   dplyr::select(x, where(Negate(is.list))) %>% 
     tibble::as_tibble() 

}


#' Rename states
#'
#' @param df dataframe with a column containing Indian states (automatically
#'   found by \code{find_st_name}), potentially with older names like
#'   'Orissa', 'Pondicherry', etc.
#'
#' @return The same dataframe but with STATE/ST_NAME columm recoded
#' @export
#' 
rename_states <- function(df) {
  
  col <- sym(find_st_name(df))
  
  # TODO: fuzzy matching with list of correct names

  # Old spellings and some common mistakes observed in datasets
  straightforw <- c("Arunanchal Pradesh" = "Arunachal Pradesh",
                    "Himachal Pradeshh" = "Himachal Pradesh",
                    "Orissa" = "Odisha",
                    "Uttaranchal" = "Uttarakhand",
                    "Pondicherry" = "Puducherry",
                    "Jammu and Kashmir" = "Jammu & Kashmir")
  
  find_n_replace_delhi <- function(v) {
    
    v[grepl("Delh", v)] <- "Delhi"
    
    v
  }
  
  df %>% 
    mutate({{ col }} := recode({{ col }}, !!! straightforw),
           {{col}} := fct_relabel({{col}}, find_n_replace_delhi))
  
  
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
#' str_count(sample_text, bound_rx(letters))
bound_rx <- function(rx_var,
                     leftbound = "\\b",
                     rightbound = "\\b",
                     by_element = FALSE) {
  
  
  if (!is.character(rx_var) || length(rx_var) == 0) {
    
    stop("Cannot build regex. ",
         deparse(substitute(rx_var)),
         " is not a character vector with length greater than 0")
    
  }
  
  rx_var <- gsub("\n", "", rx_var)
  
  leftmost <- paste0("(", leftbound)
  rightmost <- paste0(rightbound, ")")
  collapse_str = paste0(rightbound, "|", leftbound)
  
  add_bounds <- function(s)
    paste0(leftmost, s, rightmost)
  
  
  if (length(rx_var) > 1 && by_element == FALSE) {
    add_bounds(paste0(rx_var, collapse = collapse_str))
    
  } else if (length(rx_var) > 1 && by_element == TRUE) {
    add_bounds(rx_var)
    
    
  }  else if (is.na(rx_var)) {
    return(rx_var)
    
    
  }  else if (grepl("(?", rx_var, fixed = T)) {
    warning("bound_rx detected lookarounds, string supplied returned untouched.")
    
    rx_var
    
  } else if (grepl("\\|", rx_var)) {
    
    first <- gsub("(", "(\\b", rx_var, fixed = T)
    
    medium <- gsub("|", "\\b|\\b", first, fixed = T)
    
    gsub(")", "\\b)", medium, fixed = T)
    
    
    
  } else {
    add_bounds(rx_var)
    
  }
  
}


#' Checking if object contains a pattern in its names
#'
#' @param obj An R object with names
#' @param pattern A regex pattern as a character vector to use with grepl
#' @param ... Additional arguments passed to grepl
#'
#' @return the matches of `pattern` in `obj` names
#' @export
#'
#' @examples
#' 
#' peek_names(mtcars)
#' 
peek_names <- function(obj, pattern, ...) {
  
  grep(pattern, names(obj), value = TRUE, ...)
  
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


#' Find the element containing state names
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
find_st_name <- function(obj, df_nm = "<your_object>") {


  common_patterns <- "^ST_NAME$|^STATE$"
  
  ST_NAME <- grep(common_patterns, names(obj), ignore.case = TRUE, value = T)
        
  if(length(ST_NAME) == 1) {
        
        if (sum(is.na(obj[[ST_NAME]])) > 0)  {
          
          warning("State column ", ST_NAME, " in ", df_nm, " contains NA values.",
          " cptools functions will always remove/ignore those rows",
          immediate. = TRUE)
          
        }
        
        return(ST_NAME)
        
  } 
  
  
      all_state_names <- c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", 
                           "Chhattisgarh", "Delhi", "Goa", "Gujarat", "Haryana",
                           "Himachal Pradesh", "Jammu & Kashmir", "Jharkhand", 
                           "Karnataka", "Kerala", "Madhya Pradesh", "Maharashtra", 
                           "Manipur", "Meghalaya", "Mizoram", "Nagaland", "Odisha",
                           "Puducherry", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", 
                           "Telangana", "Tripura", "Uttar Pradesh", "Uttarakhand", 
                           "West Bengal")
    
      all_state_names <- bound_rx(all_state_names)
  
  
  if (length(ST_NAME) > 1) {
    
    
    # TODO: select the vector with the greatest number of states
     # lapply(obj[[ST_NAME]], grepl, FUN.VALUE = logical(1), sample_state_name)
      
      
    warning("More than one column named 'ST_NAME' or 'STATE' present.\n",
            ST_NAME[1], " selected. Please specify in the function call.",
            call. = FALSE)
    
    return(ST_NAME[1])
    
   }


  
  df_search <- obj %>%
    rm_list_cols() %>%
    dplyr::mutate(across(where(is.factor), as.character)) %>%
    dplyr::select(where(is.character))
  
  rm(obj)
  
  cols_w_state_name <- vapply(df_search, FUN.VALUE = logical(1), function(col) { 
    any(grepl(all_state_names), col, ignore.case = TRUE)
    }) %>% 
    tidyr::replace_na(FALSE) 
  
  
  ST_NAME <- names(df_search)[cols_w_state_name]

  
  if (length(ST_NAME) == 0) {

        return(NULL)
    
    }
  
  if(length(ST_NAME) > 1) {
    warning("No columns named STATE or ST_NAME found.",
            " The following columns contained at least one Indian state name: \n") 
    cat(ST_NAME, "\n", sep =", ")
    cat(ST_NAME[1], "used.  \n")
  }
  
  
  
  ST_NAME[1]
  
}



#' Replace NA in .x  cols by .y cols (or vice versa) after incomplete *_join 
#'
#' @param tbl A tibble or data_frame
#' @param vars By default, the function searches for variables with .x/.y suffixes.
#' Alternatively, a character vector of names of variables that are duplicated
#' can be provided. This is useful when operating with \code{sf} objects because 'geometry'
#' column will throw an error with the automated search.
#' These variable must be entered without .x or .y suffixes.
#' @param ending_to_keep Choose which column suffix should be kept for rows
#' where both columns have values. Currently only implemented for .x/.y.
#'
#' @return A tibble with merged columns
#' @export
#' @importFrom magrittr %>% 
#' 
replace_xy <- function(tbl, vars = NULL, ending_to_keep = ".x"){

  # Generate a character vector with only variable names that need to be 
  # deduplicated
  if(is.null(vars)) {

    
    cols_nm <- colnames(rm_list_cols(tbl))
    
    affected_cols <- grep(ending_to_keep, cols_nm, value = T, fixed = T)
    
    vars <- unique(gsub(ending_to_keep, "", affected_cols, fixed = TRUE))


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
      dplyr::mutate_if(is.factor, as.character) %>% 
      dplyr::mutate(!!sym(var) := dplyr::coalesce(!!sym(var_transfer),
                             !!sym(var_eliminate))) %>%
      dplyr::select(-dplyr::all_of(c(var_transfer, var_eliminate)))
  }
  
    tbl
}


