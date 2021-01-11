#' View (without crashing Rstudio)
#'
#' @description R's View() always tries to open any object; but some common objects make Rstudio crash or slow things down.
#' This function is designed to mask the default View(). This version removes by default any list-columns,
#' including geometry columns from \code{data.frame} objects. It also blocks viewing large dataframes.
#' 
#'
#' @param x An R object
#' @param title An optional title chosen for the window that will display the object
#' @param block_large_df Should large dataframes be blocked from displaying?
#' @param rm_lc Should list-columns be removed?
#' 
#' @details Global options can be set to alter behaviour:
#' \itemize{
#'  \item{"cpview_mblim":} {an integer for maximum dataframe size in Mb (rough approximation)}
#'  \item{"cpview_celllim":} {an integer for maximum number of dataframe columns}
#'  \item{"cpview_warn":} {anything but `NULL` to silence warnings when removing list-columns}
#' }
#'
#' 
#' @return Both the object (invisibly) and its display
#'
#' @examples
#' iris_same <- iris %>%
#'   mutate(listcol = purrr::map(Petal.Width, ~`+`(Petal.Length, .x))) %>%
#'   as_tibble() %>%
#'   View()
#'
#' iris_same
#' @import rlang
#'
#' @export
View <- function(x, title = NULL,
                 block_large_df = TRUE,
                 rm_lc = TRUE) {
  
  if (is.null(title)) {
    t <- deparse(substitute(x))
  } else t <- title
  
  # Code taken from tibble::utils, which was in development and seemed to
  # have a bug in the extraction of the title
  
  objmb <- object.size(x)/10**6 
  
  cells <- length(x)*nrow(x)
  
  mblim <- getOption("cpview_mblim", default = 200)
  
  celllim <- getOption("cpview_celllim", default = 1000000)
  
  objname <- deparse(substitute(x))
  
  stopview <- function(message, lim, lim_opt) {
    
    if(isTRUE(block_large_df)) {  warning("The '", objname, "' dataframe ", message, 
                                          "\ncptools::View has a limit of ", lim, " because viewing large dataframes may crash Rstudio.",
                                          "\nSet option '", lim_opt, "' to change this limit.",
                                          "\nDeactivate data.frame size checks with: `View(", objname, ", block_large_df = FALSE)`",
                                          "\nA small sample of the dataframe has been opened instead.",
                                          call. = FALSE)
      
      
      x <<- x[1:min(5, nrow(x)), 1:min(20, length(x))]
      
      
    }
    
  }
  
  if (inherits(x, "data.frame")) {
    
    if (objmb > mblim) stopview(paste("is of size", format(objmb, units = "MB")),
                                paste(mblim, "Mb (approx.)"), 
                                "cpview_mblim")
    
    else if (cells > celllim) stopview(paste("has", scales::comma(cells), "cells"), 
                                       paste(scales::comma(celllim), "cells"), 
                                       "cpview_lenlim")
    
  }
  
  
  if(rm_lc) {
    
    before <- length(x)
    
    x <- rm_list_cols(x)
    
    after <- length(x)
    
    if(is.null(getOption("cpview_warn")) && before > after) {
      
      warning("List-columns removed. 
             \nTo hide this warning, set option 'cpview_warn' to FALSE.
             \nTo keep list-columns, set the argument 'block_lc' to FALSE")
      
    }
    
    
  }
  
  # Returns object invisibly
  if (!interactive())
    return(invisible(x))

  
 # The following formulation comes from (the old version of) tibble::view()
  # that makes display happen in RStudio as opposed to in R GUI Viewer.
  
  view_fun <- get("View", envir = as.environment("package:utils"))
  eval_tidy(quo(view_fun(!!x, t)))
  invisible(x)
  
}