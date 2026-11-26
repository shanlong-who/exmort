# Compare observed and *expected deaths.*

[English](https://shanlong-who.github.io/exmort/)[简体中文](https://shanlong-who.github.io/exmort/zh/)

![](reference/figures/logo.png)exmort All-cause and excess mortality
calculator

How different was mortality during a heatwave, outbreak or other event?
Use weekly or monthly death counts to estimate a baseline and see how
far recorded deaths were above or below it.

[Open the
calculator](https://shanlong-who.github.io/excess-mortality/)[Run in
RStudio](#run-in-r)

The browser version downloads R on your first visit. Start with a
built-in example or open your own workbook.

[![Current CRAN
version](https://www.r-pkg.org/badges/version/exmort)](https://cran.r-project.org/package=exmort)
[![Total CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/exmort)](https://cran.r-project.org/package=exmort)
[![R CMD check
status](https://github.com/shanlong-who/exmort/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/shanlong-who/exmort/actions/workflows/R-CMD-check.yaml)

Observed deathsExpected deaths

![Monthly observed deaths rise from 980 in January to 1,230 in April,
then fall to 1,060 in June. The expected baseline stays at
1,000.](monthly-example.png)

April excess deaths**+230**

April P-score**+23%**

In April, 1,230 deaths were observed and 1,000 were expected. The
difference was 23% of the expected count.

[See how the P-score is
calculated](https://shanlong-who.github.io/exmort/articles/models-and-interpretation.html#observed-expected-and-excess-deaths)

Worked example · Synthetic data**Six months of mortality**

## From a workbook to a report

### [Prepare the data](https://shanlong-who.github.io/exmort/articles/data-preparation.md)

Match weekly or monthly death counts to an event calendar. Check dates,
missing counts and which periods will form the baseline.

### [Compare baselines](https://shanlong-who.github.io/exmort/articles/models-and-interpretation.md)

Explore seven all-cause models, from historical averages to seasonal
regression. Read the uncertainty alongside the estimated excess.

### [Run your first analysis](https://shanlong-who.github.io/exmort/articles/exmort.md)

Follow a built-in country example from the first import to a results
table, a plot and a downloadable report.

## Run exmort in RStudio

Install the CRAN release once. Then launch the calculator from the
RStudio console whenever you need it.

[CRAN package](https://cran.r-project.org/package=exmort)[Function
reference](https://shanlong-who.github.io/exmort/reference/run_app.md)

``` r

install.packages("exmort")

library(exmort)
run_app()
```

New in exmort 0.1.2

## Explore mortality by cause

Select a country and cause from the Australian or Philippine monthly
workbooks. Compare historical-average and negative-binomial baselines.
The Philippine example compares 2024 with 2019–2023, a period that
includes pandemic years.

The historical-average prediction interval included 55% of held-out
monthly counts in Australia and 53% in the Philippines.
Negative-binomial coverage was 97% and 87%, respectively. Check local
performance before using intervals for alerts.

[Read the cause-specific
guide](https://github.com/shanlong-who/exmort/blob/main/inst/app/CAUSE_SPECIFIC.md)[Download
the model audit
(CSV)](https://shanlong-who.github.io/exmort/cause-model-audit.csv)

[Cite
exmort](https://shanlong-who.github.io/exmort/authors.html#citation)[Release
notes](https://shanlong-who.github.io/exmort/news/index.md)[Source on
GitHub](https://github.com/shanlong-who/exmort)
