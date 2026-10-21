# Monthly cause-specific mortality

The development version of exmort supports selecting one country and one cause after importing a workbook. The existing all-cause examples remain available. This change does not update the installed CRAN package or publish a package release.

## Use the calculator

1. Install the GitHub development version in RStudio and run `exmort::run_app()`.
2. On **Data**, choose **Built-in example data**, then **Australia: monthly causes of death** or **Philippines: monthly causes of death**. Keep **National level** and **events** selected and merge the data.
3. Select **Country** and **Cause of death**. The data summary, model, tables, plots and report use this selection. Switching the country or cause clears previous results and reports.
4. On **Model**, select subgroups and run **Historical Average**, **Negative Binomial**, or both. Inspect the results before using them in a report.

## Bundled official data

| Dataset | Coverage | Scope and limitations |
| --- | --- | --- |
| Australia | January 2015 to May 2026; 12 categories including the doctor-certified total | Deaths by month of occurrence, doctor-certified deaths only. Some categories overlap. COVID-19 starts in 2020 and has no pre-2020 baseline. The example analysis starts in 2020, using 2015-2019 as baseline for other causes. |
| Philippines | January 2019 to December 2024; 116 categories including all causes | Registered deaths by month of occurrence. History uses the official PSA report and Excel tables; 2023-2024 use OpenSTAT. Categories include totals and subcategories and must not be added together. The example compares 2024 with 2019-2023. This historical comparison includes pandemic years and is not a pre-pandemic counterfactual. |

Both workbooks contain a **Sources** sheet with exact URLs, retrieval date, coverage, revisions, symbols and limitations. ABS revised observations replace earlier overlapping observations. Reported counts are preserved; no cause counts are allocated from all-cause totals. The available public data can be retrieved without an API key. Bundled workbooks work without a network connection; they are snapshots, not automatic live updates.

### Philippine history and baseline choice

The additional sources are PSA's 2019 Vital Statistics Report, Table 10 (printed pages 79-81), the 2020 statistical workbook T10, the 2021 workbook T11, and the 2022 workbook T13. Each table explicitly cross-tabulates cause of death and month of occurrence. The 2019 source was obtained as text through the web reader; its raw page/line references are retained in the local cache. The other three sources were downloaded as Excel files through the browser. The 2018 special-release workbook inspected during this search contains annual cause totals, not the required monthly cross-tabulation, and was not used to construct monthly values. The PSA digital library was inaccessible during retrieval. Earlier monthly cause data have not yet been verified.

All 453 historical cause/year rows reconcile exactly: their twelve monthly values equal the published annual total. There are 108 reported categories in 2019, 113 in 2020, and 116 in 2021-2022. Labels are matched to current published tabulation codes, with explicit mappings for COVID-19 and other special categories. The original source labels and controls are retained under `data-raw/official_causes/`. Existing 2023-2024 API observations are preserved.

#### Search for the 2015-2018 baseline (21 October 2026)

A public [PSA response to FOI request PSA-370244219376](https://www.foi.gov.ph/requests/causes-of-death-in-the-philippines-january-to-december/), dated 28 April 2022, explicitly identifies a Vital Statistics Division tabulation of registered deaths by cause, month of occurrence and region of usual residence for 2015-2020. Its attachment is named `2015 -2020 deaths, by cause, month and region - S. Clakr.xlsx`. This is a strong official lead for the missing baseline, rather than evidence that these monthly data do not exist.

The original [FOI document link](https://www.foi.gov.ph/documents/114406/2015_-2020_deaths_by_cause_month_and_region_-_S._Clakr_0kdkQE2.xlsx) returned HTTP 503 during retrieval. The live request page displayed the eFOI temporary-maintenance advisory ADV-2026-10-01; the historical response could be read through indexed web content. The attachment itself was not obtained or inspected. Its cause coverage, completeness, revisions and reconciliation with the existing 2019-2020 data remain unverified, so no values from it have been added to the calculator.

Other checks did not yield a verified replacement. The complete 2015, 2016 and 2017 VSR reports publish annual cause-by-sex tables separately from monthly death totals. The locally cached 2018 special-release workbook likewise does not provide the required general cause-by-month table; access to the separate 2018 VSR workbook was unsuccessful, so its contents remain unverified. A fresh OpenSTAT deaths catalogue contained 81 tables, with general cause-by-month tables for 2023 and 2024 only. The PSADA catalogue listed the historical VSR datasets but directed the documentation downloads to registration. These checks do not establish that no other official tabulation exists.

The next practical source step is to obtain the existing FOI attachment, or ask PSA's Vital Statistics Division for the same aggregate table, citing the request number. Request national monthly counts by underlying cause (with ICD-10/tabulation codes), year and month of occurrence for 2015-2019, together with revision status, annual controls, missing/zero conventions, and documentation of the 2017 Iris coding change. If regional rows are supplied, establish how national totals and regional subtotals relate before aggregating. Compare the overlapping 2019-2020 counts with the current snapshot before extending the workbook. Assess 2017-2019 as a possible primary baseline and 2015-2016 as an additional sensitivity analysis where coding comparability permits; do not assume five calendar years are automatically comparable.

Until the attachment is obtained and validated, the bundled Philippine data remain 2019-2024. They support descriptive monthly trends and seasonal patterns, and a model-based comparison of 2024 with the pandemic-inclusive 2019-2023 history for sufficiently complete and non-sparse causes. Such results describe differences from that selected history. They do not estimate deaths attributable to COVID-19 or a no-pandemic counterfactual. Only one complete pre-2020 cause-specific year is currently bundled, which is insufficient for the calculator's minimum two-year baseline when analysing 2020. Annual cause counts and monthly all-cause totals must not be combined to manufacture monthly cause counts.

The 2024 example has five complete comparison years for 108 categories. Some rare causes have all-zero or very sparse histories and still fail the model's existing adequacy guards. Categories absent from early publications remain blank, including COVID-19 in 2019; they are not assumed to be zero. Such categories cannot use the full example baseline. This addition makes a historical comparison computationally possible for common causes, including ischaemic heart diseases, neoplasms and diabetes. It does not establish an adequate pre-pandemic baseline for estimating the impact of COVID-19: that requires additional comparable pre-2020 monthly cause data. PSA also changed cause-coding processes with Iris in 2017; assess this break before adding earlier years.

To rebuild only the Philippine workbook, run `Rscript ../../data-raw/prepare_cause_examples.R --philippines-only` from `inst/app/`, using the script in the repository root. The history source files listed by `philippine_history_sources()` must be present in the local cache. If direct PSA downloads return HTTP 403, download the public attachments normally through the browser and retain their documented cache names; no authentication tokens are extracted or stored. The Australia workbook and empty template are not rebuilt by this option.

`data-raw/prepare_cause_examples.R` retrieves the public source files/API tables, validates monthly counts, normalizes labels, writes the workbooks and checks every reported count after reimport. Raw downloads and the retrieval manifest are cached under `data-raw/official_causes/`; they are excluded from deployment. Run this script from `inst/app/`, using the script in the repository root to rebuild the examples. It requires `httr2`, `jsonlite`, `rvest`, `xml2`, `readxl`, `openxlsx`, `dplyr`, `tidyr`, `lubridate` and the existing app dependencies. Downloads already present in the cache are reused; this reproduces the snapshot and does not refresh it.

## Import your own data

Download **Cause-specific monthly template** from Data. It preserves the original wide monthly format, adding `CAUSE`, `COUNTRY`, `ISO3` and `AREA` before the date columns. Add one row for each cause and subgroup. Enter counts from column G, keeping five header rows. Edit the **events** sheet to define the analysis period.

Alternatively, upload a long table with `COUNTRY`, `ISO3`, `CAUSE`, `AREA`, `SEX`, `AGE_GROUP`, `YEAR`, `PERIOD` and `NO_DEATHS`; include a separate **events** sheet. `PERIOD` is the month number. The existing all-cause templates still import, with `CAUSE = Total`. Use one frequency per imported dataset. Do not mix overlapping cause groups in a total.

Zero means no recorded deaths. Blank counts remain missing; they are never filled with zero. Duplicate observations, negative counts and fractional observed counts are rejected. Missing counts/months and incomplete baseline years stop cause modelling. At least two complete baseline years must precede all analysis months; three to five years are preferable. Future months with no published data are excluded. An all-zero baseline cannot support a P-score. Historical Average also requires at least one recorded death for each calendar month across baseline years; use a broader cause or consider Negative Binomial otherwise.

## Models and intervals

Cause-specific modelling currently supports monthly data with Historical Average and Negative Binomial. Other model options remain available for legacy all-cause analysis but are disabled for a selected cause.

- Historical Average retains the arithmetic average for each calendar month. A month-factor Poisson model supplies approximate uncertainty.
- Negative Binomial retains the existing linear year trend, cyclic seasonal term and month-length offset. Its expected count is the fitted **mean**, rather than a count-distribution quantile.
- Expected counts remain fractional, including values below one. P-score is `100 * (observed / expected - 1)` and requires a finite positive expected count.
- A fixed seed and 1,000 joint coefficient simulations provide reproducible, approximate intervals. Death-count and excess-death bounds use the counterfactual count prediction distribution. P-score bounds use uncertainty in the expected mean. Event and overall bounds use sums within each joint simulation, preserving dependence between months.
- Negative Binomial dispersion and smoothing parameters are treated as fixed in these simulations. Interval coverage has not been calibrated for every sparse cause; a minimum number of baseline years is a software guard, not a guarantee of model adequacy. Review trend extrapolation, registration changes and cause-coding changes before interpreting results. These are count-based analyses, without age-standardization or population offsets.

## Validation

Run `devtools::test()` and `devtools::check()` from the package directory. Checks cover official-workbook values and labels, legacy monthly/weekly imports, long-format country/cause fields, zero/missing/invalid counts, baseline guards, model identities and aggregate consistency, cause selection and stale-result clearing, fractional expected counts, chart context, and report context/reset.

## Integration with the packaged application

The GitHub application remains the base. GAM, Karlinsky-Kobak, ARIMA, Quasi-Poisson, Zero-Inflated Poisson and Best Fit remain available for all-cause inputs. Cause-specific runs use Historical Average and Negative Binomial only. Best Fit retains the existing convention: Historical Average is shown as N/A; the cause-specific Negative Binomial supplies its baseline AIC. A single eligible model does not establish comparative superiority.

The original source snapshot and raw retrieval files are preserved on the [backup branch](https://github.com/shanlong-who/exmort/tree/codex/cause-specific-calculator-backup-20261021/development). Extract its calculator snapshot to reproduce the preparation workflow, or place its documented raw inputs under `inst/app/data-raw/official_causes/`. These source files are not runtime dependencies.
