# Getting started with exmort

``` r

library(exmort)
```

![exmort hex logo showing observed deaths above an expected
baseline](../reference/figures/logo.png)

**exmort** is an R package that launches the All-Cause and Excess
Mortality Calculator. It helps you compare recorded deaths with an
estimated baseline, inspect differences across models, and prepare
tables, plots and reports.

The calculator accepts weekly or monthly death counts in Excel
workbooks. It includes example data for Australia, Japan, the Republic
of Korea, New Zealand and the Philippines. Start with an example before
preparing your own data.

## Install and launch

Install the released package from CRAN:

``` r

install.packages("exmort")
```

Or install the development version from GitHub:

``` r

install.packages("remotes")
remotes::install_github("shanlong-who/exmort")
```

Run this in the RStudio console:

``` r

library(exmort)
run_app()
```

The app opens in your browser. R does the calculations on your computer.
The documentation website can be read without R; using the calculator
requires a running R session. The website does not host the Shiny
application.

While the app runs, the R console is busy. Use RStudio’s Stop button or
press Escape in the console to stop it. Closing a browser tab may leave
R running.

If you want to open the browser yourself or choose a port:

``` r

run_app(launch.browser = FALSE, port = 3838)
```

Then open `http://127.0.0.1:3838` on the same computer. If that port is
already in use, choose another port.

## A first analysis with built-in data

### 1. Load an example

Open **Data → Upload Data**. Under **Open a data set**, select
**Built-in example data**. Choose a country, then select a mortality
sheet and an event sheet. Click **Merge Built-in Data**.

The example workbooks are historical training inputs. Their event dates
and observation periods need review before you use them for a new
analysis.

### 2. Check what was loaded

Open **Data → View Data** and inspect the processed mortality table,
event calendar and merged table. Check the area, age group, sex, year,
period and death counts. Use **Data Summary** to check the time coverage
and totals.

The app attaches an `event_index` to each period. A value of `0` means
that the period is outside the supplied events. **The current models use
these non-event rows as their baseline.** This can include data after an
event ends. Control the input time range and calendar deliberately;
there is no separate baseline-date selector in this version.

### 3. Run a small model comparison

Open **Model**. Select one age group and one sex category for your first
run. Keep **Historical Average** and **Negative Binomial Regression**
selected, then click **Run Model**. These are the default model choices.

Check the run messages. A selected model may fail or produce no result
when the series is too short or the baseline has insufficient
observations. Review the data and event definition before adding more
models.

### 4. Read the results

| Tab | What to examine |
|----|----|
| Tables | Observed deaths, expected deaths, excess deaths and P-scores for the selected groups and periods |
| Best Fit | Available AIC values, subject to the comparability limits explained on that page |
| Plots | Expected and observed trajectories, event patterns and summaries |
| Report | Filters, sections, preview and downloadable report |
| Methods | Definitions and the methodology material shipped with the app |
| Help and Resources | In-app help and links to the calculator’s wider resources |

Read [Choosing models and interpreting excess
mortality](https://shanlong-who.github.io/exmort/articles/models-and-interpretation.md)
before choosing a preferred estimate. Lower AIC alone does not establish
that a model gives the most credible counterfactual.

### 5. Save the analysis

Use the download buttons to save processed data and result tables. On
**Report**, select the relevant groups, models, events and sections.
Start with HTML, click **Generate Report**, review the preview, then
click **Download Report**.

Keep the input workbook, event calendar, selected models, package
version, date of analysis and exported results together. These make a
rerun possible.

``` r

packageVersion("exmort")
#> [1] '0.1.0'
```

## Prepare your own workbook

On **Data**, choose a template and click **Download template**. Keep its
header layout, replace the counts and event dates, and upload the
workbook. Select the mortality and event sheets, then click **Merge
Mortality/Events Data**. Use [Preparing mortality data and event
calendars](https://shanlong-who.github.io/exmort/articles/data-preparation.md)
for the workbook layout and checks.

The package also contains the templates locally:

``` r

template_dir <- system.file("app", "XLSX", package = "exmort")
list.files(template_dir, pattern = "^Data_Entry_Template_.*\\.xlsx$")
#> [1] "Data_Entry_Template_monthly.xlsx" "Data_Entry_Template_weekly.xlsx"
```

## Report requirements

All report formats need Pandoc. RStudio includes Pandoc; if R cannot
find it, check
[`rmarkdown::pandoc_available()`](https://pkgs.rstudio.com/rmarkdown/reference/pandoc_available.html)
in your RStudio session.

| Format       | Additional requirement                |
|--------------|---------------------------------------|
| HTML         | No LaTeX installation                 |
| Word (.docx) | The `officedown` package              |
| PDF          | A LaTeX installation, such as TinyTeX |

``` r

install.packages("officedown")
install.packages("tinytex")
tinytex::install_tinytex()
```

Install the extra tools only for the formats you plan to use. HTML is a
useful first check when a Word or PDF download fails.

## Troubleshooting

| Symptom | First check |
|----|----|
| App does not open | Confirm installation, inspect the R console, and try an unused port |
| Upload validation fails | Check the template layout, selected sheets, dates and numeric counts |
| No model results | Check group selection, non-event baseline coverage and the run messages |
| Report generation fails | Try HTML first; then check Pandoc and the chosen format’s extra tools |

Report reproducible problems at the [exmort issue
tracker](https://github.com/shanlong-who/exmort/issues). Include the
package version and error message, with a small non-sensitive example
when the problem depends on an input workbook.
