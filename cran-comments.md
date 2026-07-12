## Submission summary

This is a new submission of **exmort**, an R package that bundles an
interactive Shiny application for estimating all-cause and excess mortality.
The application is launched with `run_app()`.

## Test environments

* Local: Windows 11, R 4.6.1 — `R CMD check --as-cran`
* (Please also run win-builder / R-hub before submitting, see checklist below.)

## R CMD check results

0 errors | 0 warnings | 2 notes

The two notes are:

1. **New submission** (checking CRAN incoming feasibility) — expected for a
   first submission.
2. **README.md / NEWS.md cannot be checked without 'pandoc'** — this appears
   only in a local environment without pandoc on the PATH; it does not occur on
   CRAN's build machines or in RStudio (which bundle pandoc).

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
* The example for `run_app()` is wrapped in `\dontrun{}` because it starts an
  interactive Shiny app.

## Pre-submission checklist (run these before uploading)

1. `devtools::check(remote = TRUE, manual = TRUE)` — build the PDF manual
   (needs LaTeX) and run the checks with pandoc available.
2. `devtools::check_win_devel()` and `devtools::check_win_release()` — confirm
   a clean check on win-builder (results emailed to the maintainer).
3. Optionally `rhub::rhub_check()` for Linux/macOS.
4. Then submit: run `devtools::submit_cran()` (or upload the tarball at
   <https://cran.r-project.org/submit.html>). CRAN will email the maintainer a
   confirmation link that must be clicked to complete the submission.
