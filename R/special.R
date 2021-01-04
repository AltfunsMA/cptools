# Special functions for package

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Alfonso Martínez Arranz",
    devtools.desc.author = "Alfonso Arranz <alfonso.arranz@unimelb.com.au> [aut, cre]",
    devtools.desc.license = "MIT License",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}



.onAttach <- function(libname, pkgname) {
  packageStartupMessage("AMA custom functions loaded.")
}
