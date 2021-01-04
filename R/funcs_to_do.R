# Functions that need attention


limpiar <- function(pattern) {

  # TO-DO: NSE for the pattern and allowing clean-up

  # String of objects that have the pattern

  objs <- grep(pattern, ls(name = parent.env()), value = TRUE)

  # Actual removal function

  clean_up <- function(objs)
  {rm(list = objs, pos = parent.env())}

  # Below conditions give user options


  permission <- NULL

  if (length(objs) < 1) {print("No objects matched pattern entered")}

  else if (length(objs) == 1) {
    clean_up(objs)
    print(paste0(objs, " removed."))
  }

  else if (length(objs) > 1) {
    print(paste("About to remove:"))
    print(objs)

    permission <- readline(prompt =
                             paste("Press 'Esc' to stop or 'Enter' to proceed"))

    if (!is.null(permission)) {clean_up(objs)}

  }


}
