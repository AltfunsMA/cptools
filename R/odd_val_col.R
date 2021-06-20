#' Checking Columns for NA, Negative or Other (Odd) Values and Returning an
#' easy to inspect dataframe with meaningful reference columns that don't have
#' NAs
#'
#' @param df Dataframe in which to check for odd values.
#' @param refCols The columns that will be presented in the output. Passed to a 
#' If \code{"dist"} or \code{"ac"} are typed in, the function selects the pairs 
#' STATE/DISTRICT_NAME and ST_NAME/AC_NO.
#' @param oddValues The type of odd values with three options. The default
#' \code{"na"} checks all types of missing values in Consumer Pyramids. 
#' The option \code{"negative"} checks if any
#' numeric column is negative. Alternatively, a string vector can be supplied
#' that will used to check.
#' @param df_type 
#' as \code{refCols}.
#' @param verbose Whether to print messages to the console.
#' 
#' @details The \code{states} argument calls \code{exclude_states}
#'
#' @return A tibble with the rows where the odd values where found.
#' @export
#' @import dplyr
#'
#' @examples
#'
#' x <- na_if(mtcars, 0)
#'
#' odd_val_col(x)
#' 
odd_val_col <- function(df, refCols = c(1,2), oddValues = "NAs", verbose = TRUE) {
  
  if(!"data.frame" %in% class(df)) {
    
    stop("This function checks objects with data.frame class \n")
    
    return(invisible(NULL))
    
  }

  
  found_st_name <- toupper(find_st_name(df))
  
  df_processing <- df %>% rm_list_cols() %>% 
    rename_with(toupper) %>% 
    tibble::as_tibble()
  
  
  
  if(refCols == "ac" && !is.na(found_st_name)) {
    refCols <- c(found_st_name, "AC_NO")
    } else if(refCols == "dist" && !is.na(found_st_name)) {
    refCols <- c(found_st_name, ifelse(found_st_name == "STATE",
                                       "DISTRICT_NAME",
                                       "DIST_NAME"))
  }
  
  
  # Selecting options
  oddValues <- tolower(oddValues)
  
  neg_val_optns <- c("negative", "negatives", "neg")
  
  
  if(!oddValues %in% neg_val_optns) {
    
    if(verbose) {cat("Finding NA value types for CP (NA, NaN, -99, etc.) \n")}
    
    oddValues <- c(-99, -100, NA, NaN, -Inf, Inf, "Data Not Available",
                   "Not Applicable", "NA")
    
    comparison_fun <- `%in%`
    
    
  } else  {
    
    if(verbose) {cat("Finding negative values in numeric columns \n")}
    
    oddValues <- 0
    
    comparison_fun <- `<`
    
  }
  
  # Carrying out filtering operations
  # 
  # List columns wreak havoc with some of the steps below (probs distinct)
  # Missing values in states without Consumer Pyramids information are normal

  
  
  # Reference columns will be lost in subsequent steps if they
  # don't have oddvals
  ref_df <- df_processing %>% select(all_of(refCols))
  
  # Select **columns** with oddvals and attach to reference columns
  na_cols <- df_processing %>% 
    {if(oddValues[1] == 0) select_if(., is.numeric) else . } %>% 
    select_if(purrr::map_lgl(., ~any(comparison_fun(.x, oddValues))))
  

  if(length(na_cols) < 1) {
    
    if(verbose) {cat("No such values found. \n")}
    
    return(invisible(NULL))
           
           }
  
  # bind_cols silently appends new cols, even repeated ones
  if(is.numeric(refCols)) {
    
    refCols <- colnames(df_processing)[refCols]
  
      }
  

  unrepeated_cols <- unique(c(colnames(na_cols), refCols))
  
  na_ref_cols <- select(df_processing, all_of(unrepeated_cols)) %>% 
    select(all_of(refCols), everything()) # Front reference nes
  
  # Filter **rows** with oddvals
  out_df <- na_ref_cols %>%
    filter(if_any(everything(), ~comparison_fun(., oddValues))) %>% 
    distinct()
  
  
  # Selecting output because the full output might be overwhelming in CP
  if(verbose) {
  
  cat("Found in", nrow(out_df), "rows across these columns: \n",
      paste0(names(na_cols), collapse = ", "), ". \n\n",
      "First", min(10, nrow(out_df)), "rows listed by unique values in reference columns: \n")
  
  max_row_out <- min(10, nrow(out_df)) 
  
  print(out_df %>%
          select(all_of(refCols)) %>%
          slice(1:max_row_out)) %>%
    distinct()
  
  }
  
  invisible(out_df)
  
}
