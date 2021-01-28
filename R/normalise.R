#' Normalise values in a vector (default 0-1)
#'
#' @param v A numeric vector
#' @param min_val A minimum value
#' @param max_val A maximum value
#'
#' @return A numeric vector
#' 
#' @export
#'
normalise <- function(v, min_val, max_val) {
  
  min_val+((v - min(v))*(max_val-min_val))/(max(v)-min(v))
  
}