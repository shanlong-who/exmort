This is an update of exmort from 0.1.1 to 0.1.2.

The bundled Shiny app adds monthly cause-specific mortality analysis, two example workbooks, a workbook template, and an English and Simplified Chinese interface. The exported launch function, maintainer and licence are unchanged.

The release workflow builds the source tarball with R release and checks the same archive with R release and R-devel using R CMD check --as-cran. The tests exercise the Shiny modules and generate a report from bundled data. The larger audit of 128 cause categories and the held-out year comparisons run separately from CRAN checks.
