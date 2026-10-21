# Official Philippine monthly cause tables and their annual controls.
library(dplyr)
library(tidyr)
library(openxlsx)

philippine_history_sources <- function() {
  data.frame(
    YEAR = 2019:2022,
    FILE = c("psa_2019_table10_web_extract.txt", paste0("psa_", 2020:2022, "_statistical_tables.xlsx")),
    SHEET = c("Table 10, printed pages 79-81", "T10", "T11", "T13"),
    SOURCE_URL = c(
      "https://psa.gov.ph/system/files/main-publication/2019%2520Vital%2520Statistics%2520Report%2520Vol.%25203%2520-%2520Death%2520Statistics.pdf",
      "https://psa.gov.ph/sites/default/files/main-publication/2-2020%20Deaths%20VSR%20Statistical%20Tables_ONS.xlsx",
      "https://psa.gov.ph/system/files/vsd/Statistical%20tables_2021%20Deaths_0.xlsx",
      "https://psa.gov.ph/system/files/vsd/3_2022%20Death%20Statistical%20Tables_2nd%20draft_ONS_0.xlsx"),
    RETRIEVAL = c("Official PDF text obtained through the web reader; original line and page references retained",
                  rep("Official Excel downloaded through the browser", 3)))
}

read_psa_2019_table <- function(path) {
  # Preserve the web reader's raw output. Parse only the identified Table 10 lines.
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  ids <- suppressWarnings(as.integer(sub("^L([0-9]+)@.*", "\\1", lines)))
  selected <- !is.na(ids) & ids >= 6508L & ids <= 6628L
  lines <- lines[selected]
  ids <- ids[selected]
  lines <- sub("^L[0-9]+@P[0-9-]+: *", "", lines[!duplicated(ids)])
  starts <- which(grepl("^(Total |1-[0-9]{3}[a-d]? )", lines))
  counts_pattern <- "((?:[0-9][0-9,]*|-)(?: +(?:[0-9][0-9,]*|-)){12}) *$"
  records <- lapply(seq_along(starts), function(index) {
    start <- starts[index]
    end <- if (index < length(starts)) starts[index + 1L] - 1L else length(lines)
    record <- lines[start]
    next_line <- start + 1L
    while (!grepl(counts_pattern, record, perl = TRUE) && next_line <= end) {
      record <- paste(record, lines[next_line])
      next_line <- next_line + 1L
    }
    if (!grepl(counts_pattern, record, perl = TRUE)) stop("A 2019 Table 10 row does not have an annual total and twelve monthly values.")
    values <- regmatches(record, regexpr(counts_pattern, record, perl = TRUE))
    values <- strsplit(trimws(values), " +")[[1]]
    values[values == "-"] <- "0"
    values <- as.numeric(gsub(",", "", values, fixed = TRUE))
    data.frame(SOURCE_LABEL = trimws(sub(counts_pattern, "", record, perl = TRUE)),
               ANNUAL_TOTAL = values[1], t(values[-1]), check.names = FALSE)
  })
  out <- bind_rows(records)
  names(out)[3:14] <- month.name
  if (nrow(out) != 108L || out$ANNUAL_TOTAL[1] != 620414) stop("The 2019 monthly cause table is incomplete.")
  out
}

read_psa_history_excel <- function(path, sheet, year) {
  raw <- read.xlsx(path, sheet = sheet, colNames = FALSE, skipEmptyRows = FALSE)
  if (!grepl(paste0("Philippines: ", year), raw[1, 1], fixed = TRUE) ||
      !grepl("Month of Occurrence", paste(unlist(raw[1:3, ]), collapse = " "), fixed = TRUE)) {
    stop("The source is not the expected cause-by-month-of-occurrence table.")
  }
  if (!identical(trimws(as.character(unlist(raw[3, 3:14]))), month.name)) stop("The monthly column order has changed.")
  annual <- suppressWarnings(as.numeric(raw[[2]]))
  rows <- which(is.finite(annual) & !is.na(raw[[1]]))
  out <- data.frame(SOURCE_LABEL = trimws(raw[[1]][rows]), ANNUAL_TOTAL = annual[rows],
                    raw[rows, 3:14, drop = FALSE], check.names = FALSE)
  names(out)[3:14] <- month.name
  out
}

map_psa_cause_labels <- function(labels, dictionary) {
  labels <- trimws(gsub("[\u2010-\u2015]", "-", labels))
  published_codes <- sub("^(1-[0-9]{3}[a-d]?).*$", "\\1", labels)
  dictionary_codes <- sub("^(1-[0-9]{3}[a-d]?).*$", "\\1", dictionary)
  mapped <- dictionary[match(published_codes, dictionary_codes)]
  special <- !grepl("^1-[0-9]{3}", labels)
  text <- tolower(gsub("[^a-z0-9]", "", labels, ignore.case = TRUE))
  special_names <- case_when(
    text == "total" ~ "All causes (registered deaths)",
    grepl("vaccin", text) ~ "COVID-19 vaccines causing adverse effects in therapeutic use",
    grepl("multisystem", text) ~ "Multisystem inflammatory syndrome associated with COVID-19",
    grepl("postcovid", text) ~ "Post COVID-19 condition",
    grepl("vaping", text) ~ "Vaping-related disorder",
    grepl("virusnotidentified", text) ~ "COVID-19 Virus not identified",
    grepl("virusidentified", text) ~ "COVID-19 Virus identified",
    grepl("covid19", text) ~ "COVID-19",
    TRUE ~ NA_character_)
  mapped[special] <- special_names[special]
  if (anyNA(mapped) || any(!mapped %in% dictionary)) {
    stop(paste("Unmatched historical PSA causes:", paste(labels[is.na(mapped) | !mapped %in% dictionary], collapse = "; ")))
  }
  if (anyDuplicated(mapped)) stop("Historical PSA labels map to duplicate causes.")
  mapped
}

read_philippine_history <- function(cache_dir, dictionary) {
  sources <- philippine_history_sources()
  controls <- list()
  parts <- lapply(seq_len(nrow(sources)), function(index) {
    source <- sources[index, ]
    path <- file.path(cache_dir, source$FILE)
    if (!file.exists(path)) stop(paste("Download the documented PSA source before rebuilding:", source$SOURCE_URL, "Save as", path))
    wide <- if (source$YEAR == 2019L) read_psa_2019_table(path) else
      read_psa_history_excel(path, source$SHEET, source$YEAR)
    wide$CAUSE <- map_psa_cause_labels(wide$SOURCE_LABEL, dictionary)
    counts <- as.matrix(wide[, month.name])
    storage.mode(counts) <- "numeric"
    if (any(!is.finite(counts)) || any(counts < 0 | counts != floor(counts))) stop("Historical PSA counts contain invalid or missing values.")
    totals <- rowSums(counts)
    if (any(totals != wide$ANNUAL_TOTAL)) stop(paste("Monthly counts do not match annual controls in", source$YEAR))
    controls[[index]] <<- data.frame(YEAR = source$YEAR, SOURCE_LABEL = wide$SOURCE_LABEL,
      CAUSE = wide$CAUSE, ANNUAL_TOTAL = wide$ANNUAL_TOTAL, MONTHLY_SUM = totals,
      SOURCE_URL = source$SOURCE_URL, SHEET = source$SHEET)
    wide[, month.name] <- counts
    wide |> pivot_longer(all_of(month.name), names_to = "MONTH", values_to = "NO_DEATHS") |>
      transmute(COUNTRY = "Philippines", ISO3 = "PHL", CAUSE, YEAR = source$YEAR,
                PERIOD = match(MONTH, month.name), NO_DEATHS, SOURCE_LABEL,
                SOURCE_URL = source$SOURCE_URL, SOURCE_TABLE = source$SHEET,
                DATA_BASIS = "Month of occurrence; registered deaths",
                DATA_STATUS = "Published PSA counts; not adjusted for under-registration")
  })
  write.csv(bind_rows(controls), file.path(cache_dir, "philippines_history_annual_controls.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(sources, file.path(cache_dir, "philippines_history_sources.csv"), row.names = FALSE)
  bind_rows(parts)
}
