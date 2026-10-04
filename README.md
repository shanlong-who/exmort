# exmort

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/exmort)](https://cran.r-project.org/package=exmort)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/exmort)](https://cran.r-project.org/package=exmort)
<!-- badges: end -->

**All-Cause and Excess Mortality Calculator** — an R Shiny application for
estimating all-cause mortality and excess mortality from country-level weekly
or monthly death counts, developed at the WHO Regional Office for the Western
Pacific (WPRO).

You supply observed deaths and an event calendar (e.g. COVID-19 waves,
typhoons). The app fits one or more statistical baseline models on a
user-defined baseline period, projects expected deaths into the post-baseline
period, and reports excess deaths, P-scores and confidence limits with tables,
plots and downloadable reports.

Baseline models included:

- Historical average
- Negative binomial regression
- Quasi-Poisson regression
- Zero-inflated Poisson
- ARIMA / SARIMA
- Generalized additive model (GAM spline)
- Karlinsky–Kobak model

## Installation

Install from [CRAN](https://cran.r-project.org/package=exmort):

``` r
install.packages("exmort")
```

Development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("shanlong-who/exmort")
```

## Usage

``` r
library(exmort)
run_app()
```

The app opens in your default web browser and runs entirely on your machine —
no data leaves your computer. Built-in example datasets for several countries
are included; you can also upload your own workbook using the templates on the
**Data** tab.

## Optional features

- **Word (.docx) reports** need the `officedown` package:
  `install.packages("officedown")`.
- **PDF reports** need a LaTeX installation, e.g.
  `tinytex::install_tinytex()`.

## License

GPL-3. © World Health Organization Regional Office for the Western Pacific.
