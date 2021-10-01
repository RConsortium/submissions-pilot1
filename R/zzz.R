# Modify with care!
# The file is to load startup file automatically when the package is loaded.

.onLoad <- function(libname, pkgname) {
  startup <- system.file("startup.R", package = pkgname)

  if (file.exists(startup)) {
    source(startup)
  } else {
    stop("Can not find", startup)
  }

  invisible(startup)
}
