#' Check whether there are different steps between the elements of a vector
#'
#' @param x A numeric vector.
#' @inheritParams find_acno_incomplete
#'
#' @return A logical vector
#' @export
is.sequential <- function(x, comparison = `==`, step = NULL) {
  #  TRUE for __full__ sequences where numbers are repeated
  #  FALSE for those with gaps
  #  i.e. it'll label false an entire state missing one AC_NO
  
  
  out <- sort(x)
  
  if(is.null(step)) {step <- diff(out)[1]}
  
  all(comparison(diff(out), step)) 
  
  
}


#' Find gaps in AC No. sequence in Indian states
#'
#' @description wrapper around \code{find_seq_incomplete} with additional checks for
#'   use with (Indian) Assembly Constituency data
#'
#'   Identifies **gaps** only, if the sequence is broken because of a
#'   duplication, use \code{duplicheck}.
#'   
#' @param df A dataframe. The default assumes one column for state names and one
#'   column for AC numbers. The state column name is automatically detected and
#'   the AC number column name assumed to be either AC_NO or ac_no.
#' @param checkCols To override defaults, provide a character vector with
#'   exactly two strings. The first names the grouping variable, and the second,
#'   the accompanying numeric sequence variable.
#'
#' @inheritParams find_seq_incomplete
#'
#'
#' @return If any gaps found, the function returns a data frame with two rows before and one after the gap in the
#'   sequence, extracted by group. If no gaps found, NULL invisibly.
#'
#' @export
#' @importFrom magrittr %>%
#' 
find_acno_incomplete <- function(df, checkCols = NULL,
                             comparison = `==`,
                             step = NULL,
                             verbose = FALSE) {
  
  
  if(is.null(checkCols)) {
    
    group_name <- find_st_name(df) %>% tolower() %>% sym()
    
    seq_name <- sym("ac_no")
    
  }
  
  else {
    
    checkCols <- tolower(checkCols)
    
    group_name <- sym(checkCols[1])
    seq_name <- sym(checkCols[2])
    
  }
  
  #So warning test below works better
  df_preprocessed <- dplyr::rename_with(df, tolower)  %>% 
    dplyr::filter(!is.na(!!group_name)) %>% # Trying to fill in gaps in **known** st_names
    rm_list_cols()
  
  if("year" %in% tolower(colnames(df_preprocessed))) {
    
    uni_vals <- df_preprocessed %>% 
      dplyr::distinct(!!group_name, year) %>% 
      dplyr::count(!!group_name)
    
    if(any(uni_vals$n > 1)) {
      
      if(any(is.na(df_preprocessed$year))) {
        
        warning("\nNA values detected in 'Year' column. \n",
                "This alone will trigger a 'non-unique year value per year' warning.")
        
      }
      
      warning(
        "\nYear column found with more than one unique value per group.", 
        "\nResults will be confusing/wrong if more than one year's worth of",
        " election data per state is analysed through this function.\n")}
    
  }
  
  
  
  find_seq_incomplete(df_preprocessed, c(group_name, seq_name))
  
  
  
  
}



#' Check for gaps in numeric sequence by group 
#' 
#' @description Identifies **gaps** only, if the sequence is broken
#' because of a duplication, use \code{duplicheck} 
#'
#' @param df A dataframe
#' @param checkCols A character vector with exactly two strings. 
#' The first names the grouping variable, and the second,
#' the accompanying _numeric_ sequence variable.
#' @param comparison A relational operator for the check, with back-ticks.
#' @param step Step to be used in comparison. Defaults to the difference 
#' between the first and second elements of the vector.
#' @param verbose Should messages be reported? 
#' 
#' @return If any gaps found, the function returns a data frame with two rows 
#' before and one after the gap in the sequence, extracted by group. 
#' If no gaps found, NULL invisibly.
#' @export
#' @import dplyr

find_seq_incomplete <- function(df, 
                             checkCols = NULL, 
                             comparison = `==`,
                             step = NULL,
                             verbose = FALSE) {
  

  
  if(is.null(checkCols)) { 
    
    stop("The columns to check cannot be left empty.") 
    
  }  else if (length(checkCols) != 2) {
    
    stop(length(checkCols), "provided. 
         \nYou need to provide exactly one grouping column and one sequence column")
    
  } else if (is.symbol(checkCols[[1]]) && is.symbol(checkCols[[2]])) {
      
    group_name <- checkCols[[1]]
    
    seq_name <- checkCols[[2]]
    
    }  else {
    
    checkCols <- tolower(checkCols)
    
    group_name <- sym(checkCols[1])
    seq_name <- sym(checkCols[2])
    
    }
  
  
  
  #So warning test below works better
  df_preprocessed <- rename_with(df, tolower)  %>% 
    filter(!is.na(!!group_name)) %>% # Trying to fill in gaps in **known** st_names
    rm_list_cols()
  
  
  checked_by_group <- df_preprocessed %>%
    distinct(!!group_name, !!seq_name) %>% 
    arrange(!!group_name, !!seq_name) %>% 
    group_by(!!group_name) %>%
    mutate(complete_seq = is.sequential(!!seq_name, comparison, step)) %>%
    ungroup() %>% 
    filter(complete_seq == FALSE) %>% 
    tibble::rowid_to_column() %>% 
    select(-complete_seq)
  
  if(nrow(checked_by_group) < 1) {
    if(verbose) message("No missing sequences. \n")
    return(invisible(NULL))
    
  }
  
  
  
  extract_non_seq <- function(df_ext, seq_name) {
    # Only works if the first two values have the correct 
    # difference in the sequence  
    
    vector <- pull(df_ext, !!seq_name)
    
    y <- sort(vector)
    
    if(is.null(step)) {
      
      step <- (y[2] - y[1])
      
      message("Step defaulting to ", step)
      
    }
    
    if((y[2] - y[1]) != step) {
      
      print(y)
      
      warning("Sequence of does not start with the provided step value of", step, 
              "Results will be unreliable.")
      
      }
    
    difference_first_vs_second_position <- diff(y)[1]
    
    before_gap_positions <- diff(y) != difference_first_vs_second_position
    
    y[before_gap_positions]  
    
  }
  
  extracted <- checked_by_group %>% 
    # Split by state so that only one full ac_no sequence is passed
    # to the extractor function
    split(pull(checked_by_group, !!group_name), drop = TRUE) %>% 
    purrr::map(~extract_non_seq(.x, seq_name)) %>% 
    purrr::keep(~length(.x) > 0)
  
  group_str <- rlang::expr_text(group_name)
  seq_str <- rlang::expr_text(seq_name)
  
  before_gap_id <- purrr::map_dfr(extracted, .id = group_str, tibble::as_tibble) %>% 
    rename(!!seq_name := "value") %>% 
    left_join(mutate_if(checked_by_group, is.factor, as.character), 
              by = c(group_str, seq_str)) %>% 
    pull(rowid)
  
  
  gap_surrounds <- checked_by_group %>% 
    filter(rowid %in% (before_gap_id - 1) |
             rowid %in% before_gap_id | 
             rowid %in% (before_gap_id + 1)) %>% 
    select(-rowid) %>% 
    rename_with(toupper)
  
  if(verbose) {
  cat("Gaps identified at", sum(lengths(extracted)), 
      "point(s) in the AC_NO sequence(s)", 
      "of", length(extracted), "State(s). \n")
  
  }
  
  gap_surrounds
  
}
