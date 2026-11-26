# exmort 0.1.2

* Added an English and Simplified Chinese interface with an in-session language selector.

* Added country and cause selection for monthly mortality workbooks, with
  official Australia and Philippines examples and a cause-specific template.
* Added separate historical-average and negative-binomial cause baselines,
  missing/zero-count guards, and count-prediction and P-score uncertainty.
* Preserved the seven all-cause models, GAM, Karlinsky-Kobak, and Best Fit.
  Switching series clears predictions, AIC and report snapshots.
* Checked both cause models against every category in the two bundled workbooks.
  In a one-year holdout, historical-average prediction intervals covered 55%
  of eligible category-months in Australia and 53% in the Philippines.
  Negative-binomial coverage was 97% and 87%, respectively. The model screen
  and generated reports explain the limits of these intervals for surveillance.
* Preserved cause-specific P-score precision in result data and fractional
  expected deaths in reports. The app shows the negative-binomial AIC
  without ranking a sole eligible cause model.
* Published English and Simplified Chinese homepages with a worked example.

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
