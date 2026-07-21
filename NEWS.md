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
