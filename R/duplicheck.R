#' Check for duplicates (in Consumer Pyramids and Indian election data)
#'
#' @param df A dataframe, including sf, potentially containing duplicates.
#' @param checkCols Column names as strings that will build the minimum distinct.
#' dataset to be checked for duplicates. Defaults to ST_NAME/STATE and AC_NO.
#' @param fromLast As in \code{duplicated}. The default \code{NULL} means that 
#' both sides will be checked and returned (i.e. )
#' @param ... Arguments passed on to \code{duplicated}.
#' 
#' @return The same as \code{df} with only the duplicated rows (all of them).
#' @export
duplicheck <- function(df, checkCols = NULL, fromLast = NULL, ...) {


  sfcol <- attr(df, "sf_column")
  
  df <- rename_with(df, str_to_upper, .cols = -any_of(sfcol))
  
  
  if (is.null(checkCols)) {
    if (!"STATE" %in% colnames(df)) {
      
      checkCols = c("ST_NAME", "AC_NO")
    
    }  else {checkCols <- c("STATE", "AC_NO")}
  
    } else {checkCols <- str_to_upper(checkCols) }

  
  vars <- syms(checkCols)
  
  minimum_distinct <- df %>% 
    rm_list_cols() %>% 
    select(!!! checkCols)
  
  checking <- function(option) {
    
    df %>% 
      filter(duplicated(minimum_distinct, fromLast = option, ...))
    
  }
  
  
  if (is.null(fromLast)) {
    
    # browser()
    
    one_end <- checking(FALSE)
    other_end <- checking(TRUE)
    
    out  <- rbind(one_end, other_end) 
    
  } else {out <- checking(fromLast)
  
  if(nrow(out) < 1) {
    
    cat("No duplicated combinations of", checkCols, "found. \n") 
    
    return(NULL)
    
  }
  
  }
  
  cat(nrow(out), "rows with the same combination of", 
      checkCols[1], "and",
      checkCols[2], " \n")
  
  out %>% 
    arrange(!!!vars)
  
}