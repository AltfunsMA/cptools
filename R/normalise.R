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
normalise <- function(v, min_val = 0, max_val = 1) {
  
  min_val+((v - min(v))*(max_val-min_val))/(max(v)-min(v))
  
}