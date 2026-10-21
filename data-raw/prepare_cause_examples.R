# Run from inst/app/. Official source snapshots are retained in data-raw/.
if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "English_United States.utf8")
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(openxlsx)
library(xml2)
library(rvest)
library(lubridate)

source("modules/cause_data.R", encoding = "UTF-8")
source("../../data-raw/philippines_history.R", encoding = "UTF-8")
cache_dir <- "data-raw/official_causes"
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

download_official <- function(url, filename) {
  path <- file.path(cache_dir, filename)
  if (!file.exists(path)) request(url) |> req_timeout(60) |> req_perform(path = path)
  path
}

fetch_philippine_causes <- function() {
  root <- "https://openstat.psa.gov.ph/PXWeb/api/v1/en/DB/1A/VS/DE/"
  tables <- c(`2023` = "0491A1ADEB3.px", `2024` = "0141A1BDEE8.px")
  parts <- lapply(names(tables), function(year) {
    url <- paste0(root, tables[[year]])
    meta_path <- download_official(url, paste0("psa_", year, "_metadata.json"))
    metadata <- fromJSON(meta_path, simplifyVector = FALSE)
    causes <- metadata$variables[[1]]
    months <- metadata$variables[[2]]
    cause_codes <- unlist(causes$values)
    month_codes <- unlist(months$values)[-1]
    query <- list(query = list(
      list(code = causes$code, selection = list(filter = "item", values = as.list(cause_codes))),
      list(code = months$code, selection = list(filter = "item", values = as.list(month_codes)))
    ), response = list(format = "json"))
    data_path <- file.path(cache_dir, paste0("psa_", year, "_monthly_causes.json"))
    if (!file.exists(data_path)) request(url) |> req_body_json(query, auto_unbox = TRUE) |>
      req_timeout(60) |> req_perform(path = data_path)
    payload <- fromJSON(data_path, simplifyVector = FALSE)
    labels <- unlist(causes$valueTexts)
    labels <- sub("^[^0-9A-Za-z]+", "", labels)
    # Keep the published cause number. The same number identifies the series across years.
    cause_names <- sub(" [A-Z][0-9][0-9].*$", "", labels)
    cause_names <- sub("findings no elsewhere classified", "findings not elsewhere classified", cause_names, fixed = TRUE)
    cause_names[1] <- "All causes (registered deaths)"
    rows <- lapply(payload$data, function(cell) {
      key <- unlist(cell$key)
      count <- unlist(cell$values)[1]
      numeric_count <- suppressWarnings(as.numeric(count))
      if (is.na(numeric_count) && !count %in% c("..", "...", ":", "")) stop(paste("Unrecognised PSA count:", count))
      data.frame(COUNTRY = "Philippines", ISO3 = "PHL", CAUSE = cause_names[match(key[1], cause_codes)],
                 YEAR = as.integer(year), PERIOD = match(key[2], month_codes), NO_DEATHS = numeric_count,
                 CAUSE_CODE = key[1], SOURCE_URL = url, DATA_BASIS = "Month of occurrence; registered deaths",
                 DATA_STATUS = "Published PSA counts; not adjusted for under-registration")
    })
    out <- bind_rows(rows)
    stopifnot(nrow(out) == length(cause_codes) * 12L)
    # Published annual totals are independent controls, not extra monthly observations.
    total_query <- query
    total_query$query[[2]]$selection$values <- list(unlist(months$values)[1])
    total_path <- file.path(cache_dir, paste0("psa_", year, "_annual_controls.json"))
    if (!file.exists(total_path)) request(url) |> req_body_json(total_query, auto_unbox = TRUE) |>
      req_timeout(60) |> req_perform(path = total_path)
    annual <- fromJSON(total_path, simplifyVector = FALSE)
    for (cell in annual$data) {
      code <- unlist(cell$key)[1]
      expected <- suppressWarnings(as.numeric(unlist(cell$values)[1]))
      actual <- out$NO_DEATHS[out$CAUSE_CODE == code]
      if (all(is.finite(actual)) && is.finite(expected)) stopifnot(sum(actual) == expected)
    }
    out
  })
  normalize_mortality_data(bind_rows(parts))
}

download_australian_causes <- function() {
  page_url <- "https://www.abs.gov.au/statistics/health/causes-death/provisional-mortality-statistics/latest-release"
  page_path <- download_official(page_url, "abs_release.html")
  links <- read_html(page_path) |> html_elements("a[href]") |> html_attr("href")
  decoded <- utils::URLdecode(links)
  history <- unique(links[grepl("Deaths by month of occurrence.*[.]xlsx$", decoded)])
  current <- unique(links[grepl("Monthly Dashboard.*[.]xlsx$", decoded)])
  stopifnot(length(history) == 1L, length(current) == 1L)
  paths <- c(history = download_official(url_absolute(history, page_url), "abs_monthly_history.xlsx"),
             current = download_official(url_absolute(current, page_url), "abs_monthly_current.xlsx"))
  for (name in names(paths)) {
    cat("ABS", name, "sheets:", paste(getSheetNames(paths[[name]]), collapse = " | "), "\n")
  }
  list(paths = paths, urls = c(history = url_absolute(history, page_url), current = url_absolute(current, page_url)))
}

parse_australian_causes <- function(files) {
  parse_file <- function(path, sheet, url, current = FALSE) {
    raw <- read.xlsx(path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE)
    stopifnot(grepl("Doctor certified deaths, Number of deaths", raw[2, 1], fixed = TRUE))
    stopifnot(identical(as.character(unlist(raw[4, 3:14])), month.name))
    years <- if (current) suppressWarnings(as.integer(sub("^.*?, ([0-9]{4})$", "\\1", trimws(raw[[1]])))) else suppressWarnings(as.integer(trimws(raw[[1]])))
    valid_rows <- which(is.finite(years) & trimws(raw[[2]]) == "no.")
    labels <- if (current) trimws(sub(", [0-9]{4}$", "", raw[[1]])) else raw[[1]]
    labels[is.finite(years) & !current] <- NA_character_
    cause <- NA_character_
    rows <- list()
    for (i in seq_len(nrow(raw))) {
      if (!current && !is.na(labels[i]) && !is.finite(years[i])) cause <- trimws(labels[i])
      if (!i %in% valid_rows) next
      label <- if (current) labels[i] else cause
      if (grepl("doctor certified deaths", label, ignore.case = TRUE)) label <- "All causes (doctor certified)"
      label <- sub("Cerebrovascular diseases", "Cerebrovascular disease", label, fixed = TRUE)
      label <- sub("Dementia including Alzheimers", "Dementia", label, fixed = TRUE)
      total_row <- valid_rows[years[valid_rows] == years[i]][1]
      total_values <- suppressWarnings(as.numeric(unlist(raw[total_row, 3:14])))
      published <- seq_len(max(which(is.finite(total_values))))
      values <- trimws(as.character(unlist(raw[i, published + 2L])))
      # ABS defines the dash as nil. Apply it only within published months.
      values[values %in% c("-", "\u2014", "\u2013")] <- "0"
      counts <- suppressWarnings(as.numeric(values))
      if (anyNA(counts)) stop(paste("Unrecognised ABS count in", sheet, label, years[i]))
      rows[[length(rows) + 1L]] <- data.frame(COUNTRY = "Australia", ISO3 = "AUS", CAUSE = label,
        YEAR = years[i], PERIOD = published, NO_DEATHS = counts, SOURCE_URL = url,
        DATA_BASIS = "Month of occurrence; doctor-certified deaths only", DATA_STATUS = "Provisional and subject to revision")
    }
    bind_rows(rows)
  }
  history <- parse_file(files$paths[["history"]], "Table 4.3", files$urls[["history"]])
  current <- parse_file(files$paths[["current"]], "Table 2.2", files$urls[["current"]], current = TRUE)
  keys <- c("CAUSE", "YEAR", "PERIOD")
  # The current release owns overlapping values; do not average revisions.
  history <- anti_join(history, current, by = keys)
  normalize_mortality_data(bind_rows(history, current))
}

write_cause_workbook <- function(data, path, note, analysis_start = 2020L, event_label = "Analysis period (example)") {
  wb <- createWorkbook()
  addWorksheet(wb, "National level", gridLines = TRUE)
  addWorksheet(wb, "events")
  addWorksheet(wb, "Sources")
  dates <- seq(as.Date(sprintf("%04d-%02d-01", min(data$YEAR), min(data$PERIOD[data$YEAR == min(data$YEAR)]))),
               as.Date(sprintf("%04d-%02d-01", max(data$YEAR), max(data$PERIOD[data$YEAR == max(data$YEAR)]))), by = "month")
  years <- as.integer(format(dates, "%Y"))
  months <- as.integer(format(dates, "%m"))
  labels <- c("AGE GROUP", "SEX", "CAUSE", "COUNTRY", "ISO3", "AREA")
  writeData(wb, 1, matrix(c(labels, "YEARS"), nrow = 1), colNames = FALSE)
  writeData(wb, 1, t(years), startCol = 7, startRow = 2, colNames = FALSE)
  writeData(wb, 1, "MONTHS", startCol = 7, startRow = 3, colNames = FALSE)
  writeData(wb, 1, t(months), startCol = 7, startRow = 4, colNames = FALSE)
  writeData(wb, 1, t(month.abb[months]), startCol = 7, startRow = 5, colNames = FALSE)
  causes <- unique(data$CAUSE)
  causes <- c(causes[grepl("^All causes", causes)], causes[!grepl("^All causes", causes)])
  for (i in seq_along(causes)) {
    selected <- data[data$CAUSE == causes[i], , drop = FALSE]
    key <- selected$YEAR * 12 + selected$PERIOD
    values <- selected$NO_DEATHS[match(years * 12 + months, key)]
    writeData(wb, 1, matrix(c("Total", "Total", causes[i], data$COUNTRY[1], data$ISO3[1], "National level"), nrow = 1),
              startRow = i + 5L, colNames = FALSE)
    writeData(wb, 1, t(values), startRow = i + 5L, startCol = 7, colNames = FALSE, keepNA = FALSE)
  }
  header_style <- createStyle(fgFill = "#D9EAF4", textDecoration = "bold", fontColour = "#17365D")
  addStyle(wb, 1, header_style, rows = 1:5, cols = seq_len(6L + length(dates)), gridExpand = TRUE)
  addStyle(wb, 1, createStyle(numFmt = "#,##0"), rows = seq.int(6L, 5L + length(causes)), cols = seq.int(7L, 6L + length(dates)), gridExpand = TRUE)
  setColWidths(wb, 1, cols = c(1, 2, 4, 5, 6), widths = c(14, 12, 18, 9, 18))
  setColWidths(wb, 1, cols = 3, widths = 85)
  setColWidths(wb, 1, cols = seq.int(7L, 6L + length(dates)), widths = 12)
  freezePane(wb, 1, firstActiveRow = 6, firstActiveCol = 7)
  end_date <- ceiling_date(max(dates), "month") - days(1)
  event <- data.frame(event_name = event_label, start_date = as.Date(paste0(analysis_start, "-01-01")), end_date = end_date)
  writeData(wb, "events", event)
  addStyle(wb, "events", createStyle(numFmt = "yyyy-mm-dd"), rows = 2, cols = 2:3, gridExpand = TRUE)
  setColWidths(wb, "events", cols = 1:3, widths = c(36, 16, 16))
  writeData(wb, "Sources", data.frame(Field = c("Dataset", "Coverage", "Date basis", "Status", "Retrieved", "Use", "Blank cells", "Cause groups", "Baseline", "Symbols"),
    Value = c(paste(data$COUNTRY[1], "monthly deaths by cause"), paste(min(data$YEAR), max(data$YEAR), sep = "-"),
      data$DATA_BASIS[1], data$DATA_STATUS[1], as.character(Sys.Date()), note,
      "Unavailable or outside the published coverage. Blank values are never imputed as zero.",
      "Published categories include totals and overlapping parent/child groups. Analyse each selection separately.",
      "Edit the events sheet before analysis. All baseline months must precede the analysis window. Prefer 3-5 complete baseline years.",
      if (data$ISO3[1] == "PHL") "2019 PDF dashes represent nil counts, reconciled against each published annual total. Later Excel/API zeros are explicit. Missing earlier categories remain blank." else
        "ABS dash values are zero only within published months. Future unpublished months are excluded.")))
  sources <- data |> distinct(across(any_of(c("YEAR", "SOURCE_URL", "SOURCE_TABLE"))))
  writeData(wb, "Sources", sources, startRow = 14)
  setColWidths(wb, "Sources", cols = 1, widths = 22)
  setColWidths(wb, "Sources", cols = 2, widths = 115)
  addStyle(wb, "Sources", createStyle(wrapText = TRUE, valign = "top"), rows = 1:12, cols = 2, gridExpand = TRUE)
  setRowHeights(wb, "Sources", rows = 2:11, heights = 32)
  if (data$ISO3[1] == "PHL") {
    setRowHeights(wb, "Sources", rows = 7, heights = 150)
    setColWidths(wb, "Sources", cols = 3, widths = 50)
    addStyle(wb, "Sources", createStyle(wrapText = TRUE, valign = "top"),
             rows = seq.int(15L, 14L + nrow(sources)), cols = 2:3, gridExpand = TRUE)
    setRowHeights(wb, "Sources", rows = seq.int(15L, 14L + nrow(sources)), heights = 48)
  }
  saveWorkbook(wb, path, overwrite = TRUE)
  round_trip <- read_mortality_sheet(path, "National level")
  expected <- data |> dplyr::select(CAUSE, YEAR, PERIOD, NO_DEATHS)
  checked <- left_join(expected, round_trip, by = c("CAUSE", "YEAR", "PERIOD"), suffix = c("_source", "_file"))
  stopifnot(nrow(checked) == nrow(expected), identical(checked$NO_DEATHS_source, checked$NO_DEATHS_file))
  cat("BUILT:", path, "CAUSES:", length(causes), "SOURCE_ROWS:", nrow(data), "\n")
}

philippines_api <- fetch_philippine_causes()
philippines_history <- normalize_mortality_data(read_philippine_history(cache_dir, unique(philippines_api$CAUSE)))
philippines <- normalize_mortality_data(bind_rows(philippines_history, philippines_api))
saveRDS(philippines, file.path(cache_dir, "philippines_monthly.rds"))
write_cause_workbook(philippines, "XLSX/Philippines_cause_monthly.xlsx",
  paste("Example: analyse 2024 using 2019-2023 as the historical comparison.",
        "This comparison includes the COVID-19 pandemic years and is not a pre-pandemic counterfactual.",
        "Review the baseline before interpreting excess mortality. Causes not published in earlier years remain missing; they are not zeros.",
        "2019 uses the official report's Table 10 text; 2020-2022 use official Excel tables; 2023-2024 use OpenSTAT."),
  2024L, event_label = "2024 historical comparison")
philippines_only <- "--philippines-only" %in% commandArgs(trailingOnly = TRUE)
if (!philippines_only) {
australia_files <- download_australian_causes()
saveRDS(australia_files, file.path(cache_dir, "australia_files.rds"))
australia <- parse_australian_causes(australia_files)
saveRDS(australia, file.path(cache_dir, "australia_monthly.rds"))
write_cause_workbook(australia, "XLSX/Australia_cause_monthly.xlsx",
  "Includes doctor-certified deaths only, consistently in the baseline and analysis periods. COVID-19 has no pre-2020 baseline.")
template_data <- transform(australia[australia$CAUSE == "Ischaemic heart diseases" & australia$YEAR <= 2020, ],
                           NO_DEATHS = NA_real_, CAUSE = "Enter cause of death", COUNTRY = "Enter country", ISO3 = "XXX")
# The empty template deliberately has no round-trip data validation.
template_source <- "XLSX/Australia_cause_monthly.xlsx"
template <- loadWorkbook(template_source)
removeWorksheet(template, "National level")
addWorksheet(template, "National level")
years <- rep(2015:2020, each = 12)
months <- rep(1:12, 6)
writeData(template, "National level", matrix(c("AGE GROUP", "SEX", "CAUSE", "COUNTRY", "ISO3", "AREA", "YEARS"), nrow = 1), colNames = FALSE)
writeData(template, "National level", t(years), startRow = 2, startCol = 7, colNames = FALSE)
writeData(template, "National level", "MONTHS", startRow = 3, startCol = 7, colNames = FALSE)
writeData(template, "National level", t(months), startRow = 4, startCol = 7, colNames = FALSE)
writeData(template, "National level", t(month.abb[months]), startRow = 5, startCol = 7, colNames = FALSE)
writeData(template, "National level", matrix(c("Total", "Total", "Enter cause of death", "Enter country", "XXX", "National level"), nrow = 1), startRow = 6, colNames = FALSE)
removeWorksheet(template, "Sources")
addWorksheet(template, "Instructions")
writeData(template, "Instructions", data.frame(Instruction = c("Enter monthly counts from column G. Keep country, ISO3 and cause labels in columns C-E.",
  "Add one row per cause and subgroup. Missing counts remain blank; real zeros are 0.",
  "Alternatively upload a long table with COUNTRY, ISO3, CAUSE, AGE_GROUP, SEX, AREA, YEAR, PERIOD and NO_DEATHS.",
  "Edit the event dates to define your analysis period. Keep at least two complete baseline years before it.",
  "Do not add overlapping causes together. Document the source and date basis alongside your data.")))
writeData(template, "events", data.frame(event_name = "Analysis period (example)", start_date = as.Date("2020-01-01"), end_date = as.Date("2020-12-31")))
setColWidths(template, "National level", cols = 1:6, widths = c(14, 12, 65, 20, 9, 18))
setColWidths(template, "National level", cols = 7:78, widths = 12)
freezePane(template, "National level", firstActiveRow = 6, firstActiveCol = 7)
setColWidths(template, "Instructions", cols = 1, widths = 120)
worksheetOrder(template) <- c(2, 1, 3)
saveWorkbook(template, "XLSX/Cause_specific_monthly_template.xlsx", overwrite = TRUE)
}
manifest <- data.frame(FILE = c("Australia_cause_monthly.xlsx", "Philippines_cause_monthly.xlsx"),
                       MD5 = unname(tools::md5sum(c("XLSX/Australia_cause_monthly.xlsx", "XLSX/Philippines_cause_monthly.xlsx"))),
                       RETRIEVED = as.character(Sys.Date()), SOURCE_ROWS = c(if (exists("australia")) nrow(australia) else NA_integer_, nrow(philippines)))
if (philippines_only && file.exists(file.path(cache_dir, "manifest.csv"))) {
  previous_manifest <- read.csv(file.path(cache_dir, "manifest.csv"))
  unchanged <- match("Australia_cause_monthly.xlsx", previous_manifest$FILE)
  if (!is.na(unchanged)) manifest[1, ] <- previous_manifest[unchanged, names(manifest)]
}
write.csv(manifest, "data-raw/official_causes/manifest.csv", row.names = FALSE)
cat("CAUSE_EXAMPLES_COMPLETE\n")
