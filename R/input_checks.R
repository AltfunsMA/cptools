check_df <- function(df) {

df_class <- class(df)

  fun_name <- sys.call(-1)[[1]]

  
  if(!inherits(df, "data.frame")) {
    
    message("'", fun_name, "' can only handle objects that inherit the data.frame class.")
    
    if(grepl("_complete", fun_name)) message("\nYou may use 'cptools::is.sequential' for vectors.")
    
    invisible(stop("\r      ", call. = F))
    
    
  }
  
  if(inherits(df, "sf") && !"package:sf" %in% search()) {
    
    stop("'", fun_name, "' has been passed an sf object.
          The sf package must be loaded before using cptools functions with sf objects", call. = F)
    
  }

  if(nrow(df) == 0) {
    
    
    stop("'", fun_name, "' has taken a zero-row dataframe. Its results will be misleading.", call. = F)
    
    
  }
  
  
  
}





