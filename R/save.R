# Save functions


#' Create full paths to files for other functions
#'
#' @param path Path for file as a string with folders created as required.
#' When \code{NULL}, it defaults to "Processed" under the pwd. Any additional folders
#' listed here will appear under "Processed". If only one subdirectory required,
#' it can be a simple string (e.g. "Outputs" will create "Data/Outputs/")
#' otherwise, a character vector  (c("Outputs", "Maps") for "Data/Outputs/Maps")
#' If you need to save outside the "Data" folder, use the relevant save function.
#'
#' @section Other arguments:
#' Passed from relevant save functions
#'
#' @return A string with a properly formatted absolute filename from the current working directory

create_full_path <- function(path, filename, extension) {

  if(!is.null(path)) {

    new_dir <- file.path("Processed", path)

    if(!dir.exists(new_dir)) {
      cat("New dir <<", new_dir, ">> (recursively) created \n" )
      dir.create(new_dir, recursive = TRUE)
    }

    full_path <- paste0(new_dir, "/", filename, extension)

  } else {full_path <- paste0("Processed/", filename, extension) }

  if(!dir.exists("Processed")) {
    dir.create("Processed")
    cat("New dir: 'Processed' created \n")
  }


  full_path

}



#' Quick overwrite GPKG file
#' @inheritParams create_full_path
#' @param x An sf object
#' @param overwrite The purpose of this function is to facilitate overwriting GPKG files.
#' However, in some situations, it may be best to change this switch to FALSE for a prompt.
#' If overwrite is rarely the desired outcome, use the default sf::st_write function.
#'
#' @return A GPKG file with the same name as \code{x}
#' @export
sf_GPKG <- function (x, path = NULL, overwrite = TRUE) {

  full_path <- create_full_path(path, deparse(substitute(x)), ".gpkg")

  if(file.exists(full_path) & overwrite == FALSE) {

    cat("This will overwrite existing file")

    readline(prompt =
               paste("Press 'Esc' to stop or 'Enter' to proceed"))

    file.remove(full_path)

  }

  else if (file.exists(full_path)) {file.remove(full_path)}


  sf::st_write(x, full_path,
               driver = "GPKG", layer_options = "GEOMETRY_NAME=geometry")
}


#' Quick save CSV file
#'
#' @param df A tibble
#' @inheritParams create_full_path
#'
#' @return A CSV file with the same name as \code{df}
#' @export

wcsv <- function(df, path = NULL) {

  fullpath <- create_full_path(path, deparse(substitute(df)), ".csv")

  readr::write_csv(df, fullpath)

}
