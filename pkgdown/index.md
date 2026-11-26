<div class="em-home">

<nav class="em-language" aria-label="Homepage language"><a href="./" hreflang="en" lang="en" aria-current="page">English</a><a href="zh/" hreflang="zh-CN" lang="zh-CN">简体中文</a></nav>

<section class="em-hero" aria-labelledby="em-title">
<div class="em-intro">
<div class="em-brand"><img src="man/figures/logo.png" width="60" height="69" alt=""><span>exmort <small>All-cause and excess mortality calculator</small></span></div>
<h1 id="em-title">Compare observed and <em>expected deaths.</em></h1>
<p class="em-lead">How different was mortality during a heatwave, outbreak or other event? Use weekly or monthly death counts to estimate a baseline and see how far recorded deaths were above or below it.</p>
<div class="em-actions"><a class="em-primary" href="https://shanlong-who.github.io/excess-mortality/">Open the calculator</a><a class="em-secondary" href="#run-in-r">Run in RStudio</a></div>
<p class="em-browser">The browser version downloads R on your first visit. Start with a built-in example or open your own workbook.</p>
<div class="em-badges" role="group" aria-label="R package status">
<a href="https://cran.r-project.org/package=exmort"><img src="https://www.r-pkg.org/badges/version/exmort" alt="Current CRAN version" height="20"></a>
<a href="https://cran.r-project.org/package=exmort"><img src="https://cranlogs.r-pkg.org/badges/grand-total/exmort" alt="Total CRAN downloads" height="20"></a>
<a href="https://github.com/shanlong-who/exmort/actions/workflows/R-CMD-check.yaml"><img src="https://github.com/shanlong-who/exmort/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main" alt="R CMD check status" height="20"></a>
</div>
</div>

<figure class="em-example">
<figcaption><span class="em-kicker">Worked example · Synthetic data</span><strong>Six months of mortality</strong></figcaption>
<div class="em-legend"><span><i class="em-observed" aria-hidden="true"></i>Observed deaths</span><span><i class="em-expected" aria-hidden="true"></i>Expected deaths</span></div>
<img class="em-chart" src="monthly-example.png" width="1200" height="620" alt="Monthly observed deaths rise from 980 in January to 1,230 in April, then fall to 1,060 in June. The expected baseline stays at 1,000.">
<div class="em-reading"><div><span>April excess deaths</span><strong>+230</strong></div><div><span>April P-score</span><strong>+23%</strong></div></div>
<p>In April, 1,230 deaths were observed and 1,000 were expected. The difference was 23% of the expected count.</p>
<a href="articles/models-and-interpretation.html#observed-expected-and-excess-deaths">See how the P-score is calculated</a>
</figure>
</section>

<section class="em-guides" aria-labelledby="analysis-guides">
<h2 id="analysis-guides">From a workbook to a report</h2>
<div class="em-guide-grid">
<div><h3><a href="articles/data-preparation.html">Prepare the data</a></h3><p>Match weekly or monthly death counts to an event calendar. Check dates, missing counts and which periods will form the baseline.</p></div>
<div><h3><a href="articles/models-and-interpretation.html">Compare baselines</a></h3><p>Explore seven all-cause models, from historical averages to seasonal regression. Read the uncertainty alongside the estimated excess.</p></div>
<div><h3><a href="articles/exmort.html">Run your first analysis</a></h3><p>Follow a built-in country example from the first import to a results table, a plot and a downloadable report.</p></div>
</div>
</section>

<section class="em-install" aria-labelledby="run-in-r">
<div><h2 id="run-in-r">Run exmort in RStudio</h2>
<p>Install the CRAN release once. Then launch the calculator from the RStudio console whenever you need it.</p>
<div class="em-links"><a href="https://cran.r-project.org/package=exmort">CRAN package</a><a href="reference/run_app.html">Function reference</a></div></div>
<div class="em-code">

```r
install.packages("exmort")

library(exmort)
run_app()
```

</div>
</section>

<section class="em-cause" aria-labelledby="by-cause">
<div><p class="em-kicker">New in exmort 0.1.2</p>
<h2 id="by-cause">Explore mortality by cause</h2></div>
<div><p>Select a country and cause from the Australian or Philippine monthly workbooks. Compare historical-average and negative-binomial baselines. The Philippine example compares 2024 with 2019–2023, a period that includes pandemic years.</p>
<p>The historical-average prediction interval included 55% of held-out monthly counts in Australia and 53% in the Philippines. Negative-binomial coverage was 97% and 87%, respectively. Check local performance before using intervals for alerts.</p>
<div class="em-links"><a href="https://github.com/shanlong-who/exmort/blob/main/inst/app/CAUSE_SPECIFIC.md">Read the cause-specific guide</a><a href="cause-model-audit.csv">Download the model audit (CSV)</a></div></div>
</section>

<div class="em-footlinks"><a href="authors.html#citation">Cite exmort</a><a href="news/index.html">Release notes</a><a href="https://github.com/shanlong-who/exmort">Source on GitHub</a></div>

</div>
