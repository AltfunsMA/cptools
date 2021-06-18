#' Search for Elements in Vector2 Missing from vector2 and Vice Versa
#'
#' @description This function is intended to offer a quick visualisation of
#'   which elements are missing across two vectors.
#'
#' @param vector1 A vector for comparison with \%in\%. Geometry columns will be
#'   removed if vector originates in sf object.
#' @param vector2 As for vector1
#' @param min_obs_for_csv If NULL, output only to console. An integer value
#'   indicates the minimum number of mismatched observations for the output to
#'   be stored in a file in the folder given below.
#' @param treatment The coercing method for the vectors
#'
#' @return A tibble with the mismatches, including missing values.
#' @export
#'
#' @examples
#' mismatch(iris$Species, iris$Species)
#'
#' # Currently gives one NA value if all correct.
mismatch <- function(vector1, vector2, 
                      min_obs_for_csv = NULL,
                      folder = "mismatch",
                      treatment = as.character) {


  x <- treatment(vector1)
  y <- treatment(vector2)


  unique_mismatch1 <- unique(sort(x[!(x %in% y)]))
  unique_mismatch2 <- unique(sort(y[!(y %in% x)]))

  # to enable creation of a rectangular dataframe of results
  maxlength <- max(length(unique_mismatch1), length(unique_mismatch2))

  if(maxlength == 0) {
    
    message("No different values found \n")
    
    return(NULL)
    
    }

  df <- data.frame(unique_mismatch1[1:maxlength], unique_mismatch2[1:maxlength])

  na1 <- sum(is.na(df[1]))
  na2 <- sum(is.na(df[2]))
  
  
  name1 <- deparse(substitute(vector1))
  name2 <- deparse(substitute(vector2))

  colnames(df) <- c(paste(name1, "missing in", name2), paste(name2, "missing in", name1))

  if(!is.null(min_obs_for_csv) && dim(df)[1] > min_obs_for_csv) {
    
    if(!dir.exists(folder)) dir.create(folder)
    
    cat("Dataframe dimensions are: \n")
    cat(dim(df), "\n")
    cat(name1, "has", na2, "values without correspondence\n")
    cat(name2, "has", na1, "values without correspondence\n")
    
    
    
    full_path <- create_full_path(folder, paste0(name1, ".v.", name2, ".csv"))
    
    write.csv(df, path = full_path)
    cat(paste("File stored in ", full_path))
    
    
    }
    
  
  df

  }

