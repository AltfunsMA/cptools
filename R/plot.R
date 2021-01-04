#' Quick plot of an sf object
#'
#' @param df An sf object.
#' @param colour Colour of borders.
#' @param fill Main variable in \code{df} to be represented in plot, as a string.
#' Defaults to grey.
#' @param label_col_int Integer value of column containing labels
#'
#' @return A ggplot object.
#' @import ggplot2
#' @import rlang
#' @examples
#' \dontrun{sfplot(CP_dist_map, fill = "MEDIAN_HRS_ACCESS", label_col_int = 2)}
#' @export

sfplot <- function(df, colour = "red", fill = NA, label_col_int = FALSE) {

  need_fill <- typeof(fill) != "logical"
  ggplot(df) +
    {if(need_fill == TRUE) geom_sf(aes_string(fill = fill), colour = colour)} +
    {if(need_fill == FALSE) geom_sf(fill = "grey70", colour = colour)} +
    {if(label_col_int != FALSE) geom_sf_text(aes_(label = df[[label_col_int]]),
                                                      colour = "white") }

}




#' Quick plot of a boxplot
#'
#' @param df A data frame or tibble
#' @param x_var Normally, a factor variable
#' @param y_var Normally, a continuous variable
#'
#' @return A ggplot object.
#' @export
#' @import rlang
#' @import ggplot2
#' @examples
#' create_boxplot(iris, Species, Petal.Width)

create_boxplot <- function(df, x_var, y_var) {

  # convert strings to variable
  x_var <- sym(x_var)
  y_var <- sym(y_var)

  # unquote variable using !!
  ggplot(df, aes(x = !! x_var, y = !! y_var)) +
    geom_boxplot() +
    coord_flip() +
    labs(x = quo_name(x_var), y = quo_name(y_var))
}
