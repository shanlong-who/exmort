# exmort <a href="https://shanlong-who.github.io/exmort/"><img src="man/figures/logo.png" align="right" height="140" alt="exmort hex logo showing observed deaths above an expected baseline" /></a>

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/exmort)](https://cran.r-project.org/package=exmort)
[![Lifecycle: mature](https://img.shields.io/badge/lifecycle-mature-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/exmort)](https://cran.r-project.org/package=exmort)
<!-- badges: end -->

**All-Cause and Excess Mortality Calculator** — an R Shiny application for
estimating all-cause mortality and excess mortality from country-level weekly
or monthly death counts, developed at the WHO Regional Office for the Western
Pacific (WPRO).

You supply observed deaths and an event calendar (e.g. COVID-19 waves,
typhoons). The app fits one or more statistical baseline models using periods
outside the supplied events, estimates expected deaths across the observed
series, and reports excess deaths, P-scores and confidence limits with tables,
plots and downloadable reports.

Baseline models included:

- Historical average
- Negative binomial regression
- Quasi-Poisson regression
- Zero-inflated Poisson
- ARIMA / SARIMA
- Generalized additive model (GAM spline)
- Karlinsky–Kobak model

Documentation: <https://shanlong-who.github.io/exmort/>

## Guides

| Guide | What you will learn |
|---|---|
| [Getting started](https://shanlong-who.github.io/exmort/articles/exmort.html) | Install the package, run a built-in example and export a report |
| [Preparing data](https://shanlong-who.github.io/exmort/articles/data-preparation.html) | Fill the weekly or monthly workbook and define the event calendar |
| [Models and interpretation](https://shanlong-who.github.io/exmort/articles/models-and-interpretation.html) | Compare baselines, read uncertainty and explain excess deaths and P-scores |

The website contains documentation. Launch the Shiny calculator from R to
perform an analysis.

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

Start with **Data → Upload Data → Built-in example data**, select a country
and its mortality and event sheets, then click **Merge Built-in Data**.
Review **View Data**, open **Model**, choose a group and click **Run Model**.
Inspect **Tables** and **Plots** before generating an HTML report.

The current models use periods outside the supplied events
(`event_index == "0"`) as their baseline. These may include observations
after an event; review the input time range and event calendar carefully.

Use RStudio's Stop button or Escape in the console to stop the app.

## Optional features

- **Word (.docx) reports** need the `officedown` package:
  `install.packages("officedown")`.
- **PDF reports** need a LaTeX installation, e.g.
  `tinytex::install_tinytex()`.

## License

GPL-3. © World Health Organization Regional Office for the Western Pacific.

