
find_file <- function(file, path, extension, verbose) {


  file_string <- as.character(deparse(file))

  match_files <- function(file_string, extension) {

    list.files(pattern = paste0(file_string, extension),
                           path = path, recursive = TRUE, full.names = TRUE)

  }

  matched_file <- match_files(file_string, extension)

  # Default to finding RDS files under the same name
  if(length(matched_file) < 1) {

    extension <- ".rds"
    matched_file <- match_files(file_string, extension)

  }

  # Solving problems
  if(length(matched_file) > 1) {

    message("More than one match for: ",
           file_string, extension, "\n", sep = "")
    
    return(NULL)

    }

  else if (length(matched_file) == 1) 
    {custom_read(file_string, matched_file, extension, verbose)}

  else{

    message("Object not found in working directory under ",
           path, ". \n", sep = "")
    
    return(NULL)

    }

}

custom_read <- function(file_string, matched_file, extension, verbose) {

  if(verbose) {
    cat("Loading file and assigning to Global Environment:", matched_file, "\n")
    cat("Last modified:", as.character(file.info(matched_file)$mtime), "\n")
  }
  
    switch(extension,
           ".csv" = assign(file_string, read_csv(matched_file, col_types = cols()), 
                           envir = .GlobalEnv),
           ".gpkg" = assign(file_string, st_read(matched_file, quiet = TRUE), 
                            envir = .GlobalEnv),
           ".shp" = assign(file_string, st_read(matched_file, quiet = TRUE), 
                           envir = .GlobalEnv),
           ".rds" = assign(file_string, read_rds(matched_file), 
                           envir = .GlobalEnv)
    )

  }



#' Read a CSV File and Create Object with Same Name
#'
#' @param object Filename, which will become object name
#' @param path Directory to look for filename
#'
#' @return A tibble
#' @export

rcsv <- function(object, path = "Processed/", verbose = FALSE)
{find_file(substitute(object), path, extension = ".csv", verbose)}

#' Read a GPKG File and Create Object with Same Name
#'
#' @param object Filename, which will become object name
#' @param path Directory to look for filename
#' @param verbose Boolean. Display loading messages?
#'
#' @return An sf object
#' @export
sfread <- function(object, path = "Processed/", verbose = FALSE) {

  out <- find_file(substitute(object), path, extension = ".gpkg", verbose)

  if(is.null(out))
    {out <- find_file(substitute(object), path, extension = ".shp", verbose)}

  }
