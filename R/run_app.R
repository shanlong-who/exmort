#' Launch the All-Cause and Excess Mortality Calculator
#'
#' Starts the bundled 'shiny' application in your default web browser. The app
#' runs entirely on your own machine and does not upload any data.
#'
#' The application is copied to a per-session temporary directory before it is
#' launched, so it never writes into the (possibly read-only) package
#' installation directory.
#'
#' @param launch.browser Logical; whether to open the app in a web browser.
#'   Defaults to \code{interactive()}.
#' @param ... Additional arguments passed on to \code{\link[shiny]{runApp}}
#'   (for example \code{port} or \code{host}).
#'
#' @return Called for its side effect of running the Shiny application;
#'   invisibly returns \code{NULL} when the app is closed.
#'
#' @examples
#' \dontrun{
#' # Launch the calculator in your browser:
#' run_app()
#' }
#'
#' @export
run_app <- function(launch.browser = interactive(), ...) {
  app_src <- system.file("app", package = "exmort")
  if (!nzchar(app_src)) {
    stop("Could not locate the bundled app directory. ",
         "Try reinstalling the 'exmort' package.", call. = FALSE)
  }

  # Run from a temporary copy: app.R creates a 'data/' folder in its working
  # directory at runtime, and packages must not write to their own library.
  run_dir <- file.path(tempdir(), "exmort_app")
  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE)
  }
  file.copy(
    from = list.files(app_src, full.names = TRUE),
    to = run_dir, recursive = TRUE, overwrite = TRUE
  )

  shiny::runApp(run_dir, launch.browser = launch.browser, ...)
  invisible(NULL)
}
