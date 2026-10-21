# Official cause-specific example preparation

The application bundles validated Excel snapshots in `inst/app/XLSX/`.
They do not require network access at runtime. Each workbook includes a
Sources sheet with URLs, coverage and interpretation limits.

To reproduce the snapshots, use the public raw inputs retained in the
[calculator backup](https://github.com/shanlong-who/exmort/tree/codex/cause-specific-calculator-backup-20261021/development).
Extract `calculator/data-raw/official_causes/` into
`inst/app/data-raw/official_causes/`. In RStudio, set the working directory
to `inst/app/` and run:

```r
source("../../data-raw/prepare_cause_examples.R", encoding = "UTF-8")
```

The script requires httr2, jsonlite, rvest and xml2 in addition to the
application dependencies. Existing source files are reused; this reproduces
the snapshot rather than refreshing all observations. The historical PSA
attachments must be supplied under their documented cache names if direct
downloads are unavailable. `philippines_history.R` contains their table
parsers and checks against published annual totals.

See `inst/app/CAUSE_SPECIFIC.md` for the complete source and model notes.
The Philippine snapshot covers 2019-2024. Its default 2024 comparison uses
pandemic-inclusive 2019-2023 history and is not a pre-pandemic counterfactual.
Monthly 2015-2018 values have not been obtained and validated.

This directory is excluded from R package builds.
