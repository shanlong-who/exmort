# Standalone cause-specific calculator backup

This branch preserves the working EM calculator snapshot from 21 October 2026. It is a backup for comparison and later integration, not a new exmort package release. The package application in `inst/app/` is unchanged.

## Download and run

1. Download [calculator-snapshot-2026-10-21.zip](calculator-snapshot-2026-10-21.zip) and extract it.
2. Open `calculator/calculator.Rproj` in RStudio.
3. From that project, run `source("scripts/preview_cause_calculator.R")`.

The archive contains 96 files: application code, templates, Australian and Philippine monthly cause examples, public source inputs, preparation scripts, tests and detailed usage notes. Every archived file was extracted and checked against the prepared snapshot before upload. Session history, local settings, deployment metadata and generated reports are excluded.

Archive SHA-256: `10e4014fe1076918adc11b9f69798719a753b1381de3d91572c5e183cade10f1`

## Main differences

The [file comparison](calculator-comparison.csv) compares shared R files with package commit `7d67c0584e0f85a33da59494f1f0ecef33de2e8c`. All 21 shared R files differ in text. Some differences are comments or cleanup, while others affect behaviour.

The package version already has GAM, Karlinsky-Kobak, Best Fit model comparison, package-local module sourcing, upload validators, normalized template filenames and a separate report template. The standalone version adds country/cause selection, separate monthly cause baselines, missing/zero safeguards, approximate uncertainty, cause-labelled outputs and public cause-specific example data. A later integration must preserve the package improvements rather than replace `inst/app/` wholesale.

The Australian example begins in 2015. The Philippine example covers 2019-2024 and compares 2024 with 2019-2023. That Philippine baseline includes pandemic years and is not a no-pandemic counterfactual. The 2015-2018 Philippine monthly cause data have not been obtained and validated; the official FOI lead and current retrieval limitation are documented inside the archive.

The cause-specific test runner completed with no failed assertions (`CAUSE_TESTS_COMPLETE`). The existing Windows R process-exit anomaly remains. Browser validation confirmed the Philippine ischaemic-heart-disease example produces model results and a chart. The snapshot does not establish that every sparse cause is estimable.

The development directory is excluded from R package builds. Installing this branch and calling `exmort::run_app()` still launches the existing package application; use the extracted standalone project to try the new cause feature.
