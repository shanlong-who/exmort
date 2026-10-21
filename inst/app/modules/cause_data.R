# Mortality input shared by legacy templates and cause-specific workbooks.
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(ISOweek)

normalize_mortality_data <- function(data, area = "National level") {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  names(data) <- toupper(gsub("[ -]+", "_", trimws(names(data))))
  names(data)[names(data) == "EVENT_INDEX"] <- "event_index"
  names(data)[names(data) == "EVENT_NAME"] <- "event_name"
  required <- c("YEAR", "PERIOD", "NO_DEATHS")
  if (!all(required %in% names(data))) stop("Mortality data need YEAR, PERIOD and NO_DEATHS columns.")
  defaults <- list(COUNTRY = "Unspecified country", ISO3 = "UNK", CAUSE = "Total",
                   AREA = area, AGE_GROUP = "Total", SEX = "Total")
  for (column in names(defaults)) {
    if (!column %in% names(data)) data[[column]] <- defaults[[column]]
    data[[column]] <- trimws(as.character(data[[column]]))
    if (any(is.na(data[[column]]) | data[[column]] == "")) {
      stop(paste("Fill all", column, "labels. Missing labels cannot identify a mortality series."))
    }
  }
  for (column in required) {
    original <- data[[column]]
    converted <- suppressWarnings(as.numeric(original))
    if (any(is.na(converted) & !is.na(original) & trimws(as.character(original)) != "")) {
      stop(paste(column, "contains non-numeric values. Keep missing counts blank; keep real zeros as 0."))
    }
    data[[column]] <- converted
  }
  if (anyNA(data$YEAR) || anyNA(data$PERIOD)) stop("YEAR and PERIOD cannot be missing.")
  if (any(data$YEAR != floor(data$YEAR) | data$YEAR < 1900 | data$YEAR > 2100)) stop("YEAR must contain valid whole years.")
  if (!"WM_IDENTIFIER" %in% names(data)) {
    data$WM_IDENTIFIER <- if (max(data$PERIOD) > 12) "Week" else "Month"
  }
  data$WM_IDENTIFIER <- ifelse(tolower(data$WM_IDENTIFIER) %in% c("month", "monthly"), "Month",
                               ifelse(tolower(data$WM_IDENTIFIER) %in% c("week", "weekly"), "Week", NA_character_))
  if (anyNA(data$WM_IDENTIFIER) || dplyr::n_distinct(data$WM_IDENTIFIER) != 1L) stop("Use one frequency per workbook: Month or Week.")
  limit <- if (data$WM_IDENTIFIER[1] == "Month") 12 else 53
  if (any(data$PERIOD != floor(data$PERIOD) | data$PERIOD < 1 | data$PERIOD > limit)) stop("PERIOD is outside the selected frequency.")
  counts <- data$NO_DEATHS
  if (any(!is.na(counts) & (!is.finite(counts) | counts < 0 | counts != floor(counts)))) {
    stop("NO_DEATHS must contain non-negative whole counts or blank missing values.")
  }
  keys <- c("ISO3", "COUNTRY", "CAUSE", "AREA", "SEX", "AGE_GROUP", "YEAR", "PERIOD")
  if (anyDuplicated(data[keys])) stop("Duplicate country, cause, subgroup and time records were found.")
  if (data$WM_IDENTIFIER[1] == "Month") {
    data$DAYS <- as.numeric(lubridate::days_in_month(as.Date(sprintf("%04d-%02d-01", data$YEAR, data$PERIOD))))
  } else if (!"DAYS" %in% names(data)) {
    data$DAYS <- 7
  }
  data |> arrange(COUNTRY, CAUSE, AREA, SEX, AGE_GROUP, YEAR, PERIOD)
}

read_mortality_sheet <- function(path, sheet) {
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE,
                            skipEmptyRows = FALSE, skipEmptyCols = FALSE)
  if (nrow(raw) < 2L) stop("The selected mortality sheet is empty.")
  header <- toupper(gsub("[ -]+", "_", trimws(as.character(unlist(raw[1, ])))))
  if (all(c("YEAR", "PERIOD", "NO_DEATHS") %in% header)) {
    data <- openxlsx::read.xlsx(path, sheet = sheet, skipEmptyRows = TRUE)
    return(normalize_mortality_data(data, sheet))
  }
  years <- suppressWarnings(as.numeric(unlist(raw[2, ])))
  time_columns <- which(is.finite(years) & years >= 1900 & years <= 2100)
  if (!length(time_columns)) stop("The sheet does not match a monthly or weekly mortality template.")
  weekly <- any(toupper(as.character(as.matrix(raw[seq_len(min(6L, nrow(raw))), ]))) == "WEEKS", na.rm = TRUE)
  period_row <- if (weekly) 6L else 4L
  first_data_row <- if (weekly) 7L else 6L
  if (nrow(raw) < first_data_row) stop("The template has no mortality rows.")
  periods <- suppressWarnings(as.numeric(unlist(raw[period_row, time_columns])))
  valid_time <- is.finite(periods) & periods >= 1 & periods <= if (weekly) 53 else 12
  time_columns <- time_columns[valid_time]
  periods <- periods[valid_time]
  metadata <- intersect(which(header %in% c("AGE_GROUP", "SEX", "CAUSE", "COUNTRY", "ISO3", "AREA")), seq_len(ncol(raw)))
  rows <- seq.int(first_data_row, nrow(raw))
  values <- as.matrix(raw[rows, time_columns, drop = FALSE])
  keep_rows <- rowSums(!is.na(values) & trimws(as.character(values)) != "") > 0
  rows <- rows[keep_rows]
  if (!length(rows)) stop("The selected mortality sheet has no reported counts.")
  parts <- lapply(rows, function(row) {
    labels <- as.list(raw[row, metadata, drop = FALSE])
    names(labels) <- header[metadata]
    data.frame(labels, YEAR = years[time_columns], PERIOD = periods,
               NO_DEATHS = as.character(unlist(raw[row, time_columns])),
               WM_IDENTIFIER = if (weekly) "Week" else "Month", check.names = FALSE)
  })
  data <- bind_rows(parts)
  # Legacy templates merge age labels across the sex rows.
  if ("AGE_GROUP" %in% names(data)) data <- tidyr::fill(data, AGE_GROUP)
  normalize_mortality_data(data, sheet)
}

mortality_sheet_names <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  sheets[!grepl("^(instructions?|sources?|notes?|metadata|events?)([ (].*)?$", sheets, ignore.case = TRUE)]
}

builtin_mortality_registry <- function() {
  c("Australia" = "Australia_built_in_data.xlsx", "Japan" = "Japan_built_in_data.xlsx",
    "Republic of Korea" = "Republic_of_Korea_built_in_data.xlsx", "New Zealand" = "New_Zealand_built_in_data.xlsx",
    "Philippines" = "Philippines_built_in_data.xlsx",
    "Australia: monthly causes of death" = "Australia_cause_monthly.xlsx",
    "Philippines: monthly causes of death" = "Philippines_cause_monthly.xlsx")
}

builtin_mortality_path <- function(name) {
  registry <- builtin_mortality_registry()
  if (!name %in% names(registry)) stop("Choose a built-in dataset.")
  file.path("XLSX", unname(registry[[name]]))
}

clear_mortality_results <- function(rv) {
  for (field in c("total_prediction", "plot_time_data", "summary_by_event", "overall_summary", "model_aic", "best_model")) rv[[field]] <- NULL
}

mortality_source_note <- function(path) {
  if (!"Sources" %in% openxlsx::getSheetNames(path)) return(NULL)
  notes <- openxlsx::read.xlsx(path, sheet = "Sources", rows = 1:11)
  if (!all(c("Field", "Value") %in% names(notes))) return(NULL)
  paste(notes$Value[notes$Field %in% c("Date basis", "Status", "Use")], collapse = " ")
}

prepare_mortality_model_input <- function(data) {
  data <- normalize_mortality_data(data)
  if (n_distinct(data$COUNTRY) != 1L || n_distinct(data$CAUSE) != 1L) stop("Select one country and one cause before fitting models.")
  data$SE_IDENTIFIER <- "END"
  data$DATE_TO_SPECIFY_WEEK <- if (data$WM_IDENTIFIER[1] == "Week") {
    dates <- ISOweek::ISOweek2date(sprintf("%04d-W%02d-1", data$YEAR, data$PERIOD))
    paste0(month.abb[lubridate::month(dates)], "-", format(dates, "%d"))
  } else rep(NA_character_, nrow(data))
  data
}
