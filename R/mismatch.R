#' Search for Elements in Vector2 Missing from vector2 and Vice Versa
#'
#' @description This function is intended to offer a quick visualisation of which districts or others are missing in Consumer Pyramids data.
#'
#' @param vector1 A vector for comparison with \%in\%. Geometry columns will be removed if vector originates in sf object.
#' @param vector2 As for vector1
#' @param min_obs_for_csv If no. of mismatches is above this number,
#' a CSV will be created under "Processed/Checks/mm_" (directory not created)
#' @param treatment The coercing method for the vectors
#'
#' @return A tibble with the mismatches, including missing values.
#' @export
#'
#' @examples
#' mismatch(iris$Species, iris$Species)
#'
#' # Currently gives one NA value if all correct.
mismatch <- function (vector1, vector2, min_obs_for_csv = 20,
                      treatment = as.character) {

  name1 <- deparse(substitute(vector1)) # extract name for meaningful filename/colnames
  name2 <- deparse(substitute(vector2))

  full_path <- create_full_path("Checks",
                                paste0("mm_", str_sub(name1, 1, 4),
                                       str_sub(name1, -3, -1),
                                       ".v.",
                                       str_sub(name2, 1, 4), str_sub(name2, -3, -1)),
                                ".csv")

  x <- treatment(vector1)
  y <- treatment(vector2)


  unique_mismatch1 <- unique(sort(x[!(x %in% y)]))
  unique_mismatch2 <- unique(sort(y[!(y %in% x)]))

  # to enable creation of a rectangular dataframe of results
  maxlength <- max(length(unique_mismatch1), length(unique_mismatch2))

  if(maxlength == 0) {return(cat("No different values found \n"))}

  df <- data.frame(unique_mismatch1[1:maxlength], unique_mismatch2[1:maxlength])

  na1 <- sum(is.na(df[1]))
  na2 <- sum(is.na(df[2]))

  colnames(df) <- c(name1, name2)

  if(dim(df)[1] > min_obs_for_csv) {write_csv(df, path = full_path)
    print(paste("File stored in ", full_path))}

  cat("Dataframe dimensions are: ")
  cat(dim(df))
  cat(name1, "has", na2, "values without correspondence")
  cat(name2, "has", na1, "values without correspondence")

  df
}
