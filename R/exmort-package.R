#' exmort: All-Cause and Excess Mortality Calculator
#'
#' An interactive 'shiny' application for estimating all-cause mortality and
#' excess mortality from country-level weekly or monthly death counts. See
#' \code{\link{run_app}} to start the application.
#'
#' The \code{@importFrom} tags below register one function from each runtime
#' dependency. The application code lives under \code{inst/app} and attaches
#' these packages itself at run time; the tags exist so that every package in
#' the DESCRIPTION Imports field is recorded in the NAMESPACE and installed
#' alongside \pkg{exmort}.
#'
#' @keywords internal
#' @importFrom base64enc base64encode
#' @importFrom data.table as.data.table
#' @importFrom dplyr across
#' @importFrom DT datatable
#' @importFrom forecast auto.arima
#' @importFrom ggplot2 ggplot
#' @importFrom ggrepel geom_text_repel
#' @importFrom htmltools HTML
#' @importFrom ISOweek ISOweek2date
#' @importFrom kableExtra kbl
#' @importFrom knitr knit
#' @importFrom lubridate ymd
#' @importFrom mgcv gam
#' @importFrom openxlsx read.xlsx
#' @importFrom parallel detectCores
#' @importFrom plotly plot_ly
#' @importFrom reactable reactable
#' @importFrom readxl read_excel
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt
#' @importFrom rmarkdown render
#' @importFrom scales percent
#' @importFrom shiny runApp
#' @importFrom shinyalert shinyalert
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs useShinyjs
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom zoo na.locf
"_PACKAGE"
