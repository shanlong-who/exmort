# exmort 0.1.1

* Added three vignettes covering installation and launch, input templates
  and data preparation, and baseline models and result interpretation.
* Added a pkgdown documentation website, linked from the package metadata
  and README, with a function reference and searchable articles.
* Added a package hex logo showing expected and observed mortality, used in
  the README, website and vignettes.
* Clarified that baseline models are fitted to all periods outside the
  supplied events, including any non-event periods after an event.

# exmort 0.1.0

* First release: the WHO WPRO All-Cause and Excess Mortality Calculator
  packaged as an R package.
* `run_app()` launches the bundled Shiny application from a temporary
  directory.
* Includes seven baseline models (historical average, negative binomial,
  quasi-Poisson, zero-inflated Poisson, ARIMA/SARIMA, GAM spline and the
  Karlinsky–Kobak model), event annotation, best-fit AIC comparison, and
  HTML / Word / PDF report generation.
* Application code is sourced into a private environment instead of the global
  environment, and the app no longer leaves `options(digits)` or the `LC_TIME`
  locale changed after a session ends.
* Adds a `testthat` suite covering the ISO year-week date helpers, the model
  date index and the upload validators.
