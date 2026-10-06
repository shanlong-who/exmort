This is a documentation update from exmort 0.1.0 to 0.1.1.

The update adds three executable vignettes, a pkgdown documentation website linked from DESCRIPTION and README, and a package hex logo. DESCRIPTION and the guides clarify the existing behavior: baseline models use all observations marked as non-event, which may include observations after an event. The exported API and bundled Shiny application computations are unchanged. The maintainer, copyright holder and GPL-3 license are unchanged.

R CMD check --as-cran was run on the exact source tarball with:
- R 4.6.1 on Ubuntu 24.04.5: 0 errors, 0 warnings, 0 notes.
- R-devel (2026-10-03 r90638) on Ubuntu 24.04.5: 0 errors, 0 warnings, 0 notes.

Both checks include CRAN incoming checks, tests, examples, rebuilding all three vignettes, and the PDF and HTML reference manuals. The repository's existing R-CMD-check workflow also passed on this source revision.

The vignette examples use bundled data and do not require network access or start an interactive Shiny session. The installed vignette documentation is approximately 3.7 MB. The application runtime dependencies are unchanged.

There are no reverse dependencies listed on the current CRAN package page.
