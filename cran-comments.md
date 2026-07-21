## Resubmission

This is a resubmission of **exmort** 0.1.0. Thank you to Konstanze Lauseker for
the review. All points raised have been addressed:

1. **Single quotes around COVID-19 removed.** The DESCRIPTION now writes
   COVID-19 without quotes. Quotes are kept only around the software name
   'shiny'.

2. **All acronyms are now explained in the Description text.** ARIMA
   (autoregressive integrated moving average), SARIMA (seasonal ARIMA) and GAM
   (generalized additive model) are spelled out on first use, and P-score is
   defined as excess deaths as a percentage of expected deaths. A reference for
   the Karlinsky-Kobak model has been added in the required format:
   Karlinsky and Kobak (2021) <doi:10.7554/eLife.69336>.

3. **`\dontrun{}` replaced with `if (interactive()) {}`** in the example for
   `run_app()`.

4. **Tests added for the non-exported code.** The package now ships a
   `testthat` (edition 3) suite in `tests/testthat/`. Because the Shiny
   interface cannot be checked automatically, the tests target the
   application's internal helper functions directly: the internal
   `app_module_env()` loader, the ISO year-week date helpers
   (`get_iso_weeks_in_year()`, `get_week_start_date()`), the continuous date
   index used by every baseline model (`calculate_dates()`), the plot palette,
   and the upload validators (`validate_mortality_data()`,
   `validate_event_data()`, `validate_uploaded_data()`). These will fail if a
   change in R or in a dependency breaks the app's core computations.

5. **`.GlobalEnv` is no longer modified.** `inst/app/app.R` previously used
   bare `source()` calls (which default to `local = FALSE`) plus
   `rm(..., envir = .GlobalEnv)`. All application code is now sourced into a
   private environment through a `source_module()` helper, and the `rm()` calls
   have been removed. Loading the app no longer creates or deletes any object
   in the global environment.

6. **The user's options and locale are no longer changed permanently.**
   * `inst/app/app.R` set `options(digits = 3)` inside the Shiny server
     function. The previous options are now captured and restored with
     `session$onSessionEnded()`. A plain `on.exit()` cannot be used here: the
     server function returns before any output is rendered, so the setting
     would be undone too early. `onSessionEnded()` is the Shiny equivalent and
     is registered immediately after the change.
   * The `LC_TIME` switch used to obtain English month labels now lives in a
     small helper function that registers
     `on.exit(Sys.setlocale("LC_TIME", original_locale))` immediately after the
     change.
   * `inst/app/modules/plot_event_module_new.R` set `LC_TIME` to "English" at
     load time and never restored it. Nothing in that file formats month or day
     names, so the call has been removed.

   There are no `par()` or `setwd()` calls anywhere in the package.

## Test environments

* Local: Windows 11, R 4.6.0 — `R CMD check --as-cran`
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* **New submission** (checking CRAN incoming feasibility) — expected for a
  first submission.

`checking package dependencies` reports an INFO (not a note) that Imports
includes 29 non-default packages. This package ships a full Shiny application
that attaches these packages at run time, so they are genuine runtime
requirements rather than optional dependencies. The two truly optional features
(Word `.docx` export via `officedown` and PDF export via a LaTeX install) are in
Suggests and are used conditionally, with the app surfacing a clear message when
they are absent.

## Notes for the maintainer

* The application runs from a per-session temporary copy of `inst/app`
  (`run_app()` copies the app to `tempdir()` before launching), so it never
  writes into the package's installation directory.

## Pre-submission checklist (run these before uploading)

1. `devtools::document()` — regenerate `man/` after editing roxygen comments.
2. `devtools::test()` — run the testthat suite.
3. `devtools::check(remote = TRUE, manual = TRUE)` — build the PDF manual
   (needs LaTeX) and run the checks with pandoc available.
4. `devtools::check_win_devel()` and `devtools::check_win_release()` — confirm
   a clean check on win-builder (results emailed to the maintainer).
5. Optionally `rhub::rhub_check()` for Linux/macOS.
6. Then submit: run `devtools::submit_cran()` (or upload the tarball at
   <https://cran.r-project.org/submit.html>). CRAN will email the maintainer a
   confirmation link that must be clicked to complete the submission.
