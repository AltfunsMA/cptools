ellipsis_to_vector <- function(...) {
  # Convert to a list, but with the variables still as an expression
  args_as_expression = substitute(list(...))
  # Deparse each item in the expression, turning it into a char vector
  # Why the [-1]? Because the first element in the list expression is "list"! :-)
  args_as_char_vector = sapply(args_as_expression,deparse)[-1]  
  args_as_char_vector
}

#' Create a list and name it with its own objects
#'
#' @param ... Any objects that can be passed to `list()`
#'
#' @return A named list 
#' @export
#'
#' @examples
#' 
#' solar <- c("photovoltaic", "solar cell")
#' wind <- c("wind turbine", "wind farm")
#' 
#' named_energy <- lnm(solar, wind)
#' 

lnm <- function(...) {
  
 out <- list(...)
  
  
 nm <- cptools:::ellipsis_to_vector(...) 
 
 names(out) <- nm
  
  out
  
}



#' Create a vector and name it with its own objects
#'
#' @param ... Character objects that can be passed to `c()`
#'
#' @return A named vector 
#' @export
#'
#' @examples
#' 
#' solar <- c("photovoltaic", "solar cell")
#' wind <- c("wind turbine", "wind farm")
#' 
#' named_energy <- cnm(solar, wind)
#' 

cnm <- function(...) {
  
  
  out <- c(...)
  
  nm <- cptools:::ellipsis_to_vector(...) 
  
  names(out) <- nm
  
  out
  
}