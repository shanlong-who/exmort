# Internal helpers for reaching the bundled Shiny application.
#
# The application code lives under inst/app and is not part of the package
# namespace, so it cannot be checked by the usual example/vignette machinery.
# app_module_env() loads selected module files into a private environment,
# which lets the unit tests in tests/testthat exercise the app's model,
# validation and date helpers without starting a Shiny session.

#' Path to the installed application directory
#'
#' @return A length-one character vector with the path to \code{inst/app}
#'   inside the installed package.
#' @noRd
exmort_app_dir <- function() {
  path <- system.file("app", package = "exmort")
  if (!nzchar(path)) {
    stop("Could not locate the bundled app directory. ",
         "Try reinstalling the 'exmort' package.", call. = FALSE)
  }
  path
}

#' Load application module files into a private environment
#'
#' Mirrors the \code{source_module()} loader used by \code{inst/app/app.R}:
#' files are sourced into a dedicated environment (never into
#' \code{.GlobalEnv}) and each file is loaded only once.
#'
#' @param modules Character vector of file names inside
#'   \code{inst/app/modules}, for example \code{"utils.R"}.
#' @param envir Environment to load into. A fresh child of the global
#'   environment is created by default.
#'
#' @return The environment holding the loaded module objects.
#' @noRd
app_module_env <- function(modules = character(0),
                           envir = new.env(parent = globalenv())) {
  app <- exmort_app_dir()
  loaded <- character(0)

  # Module files source their own dependencies through source_module(); it has
  # to be visible from inside `envir` for that to resolve.
  envir$source_module <- function(path, force = FALSE) {
    if (!force && path %in% loaded) {
      return(invisible(FALSE))
    }
    loaded <<- c(loaded, path)
    file <- file.path(app, path)
    if (!file.exists(file)) {
      stop("Unknown application module: ", path, call. = FALSE)
    }
    source(file, local = envir, encoding = "UTF-8")
    invisible(TRUE)
  }

  for (module in modules) {
    envir$source_module(paste0("modules/", module))
  }
  envir
}
