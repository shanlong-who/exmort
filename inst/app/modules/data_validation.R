# Schema and content checks run between data load and merge. The goal is
# to catch malformed uploads early with a clear message instead of
# letting the model run blow up on missing columns or non-numeric values.
#
# Each validator returns a character vector of human-readable errors.
# An empty vector means the data passed validation.

library(dplyr)

# Mortality data is expected after process_mortality_data2() has run.
# At that point the columns are: AREA, AGE_GROUP, SEX, YEAR, PERIOD,
# DAYS, NO_DEATHS.
validate_mortality_data <- function(df) {
  errors <- character(0)

  if (is.null(df)) {
    return("Mortality data is missing. The selected sheet may be empty or the wrong format.")
  }
  if (!is.data.frame(df)) {
    return("Mortality data is not a data frame.")
  }
  if (nrow(df) == 0) {
    return("Mortality data is empty after processing.")
  }

  required_cols <- c("AREA", "AGE_GROUP", "SEX", "YEAR", "PERIOD", "NO_DEATHS")
  missing_cols  <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste0(
      "Mortality data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Type checks only when the column is present.
  if ("YEAR" %in% names(df)) {
    yr <- suppressWarnings(as.numeric(df$YEAR))
    if (any(is.na(yr) & !is.na(df$YEAR))) {
      errors <- c(errors, "YEAR has non-numeric values.")
    } else if (any(yr < 1900 | yr > 2100, na.rm = TRUE)) {
      errors <- c(errors, "YEAR has values outside the plausible range 1900-2100.")
    }
  }

  if ("PERIOD" %in% names(df)) {
    pd <- suppressWarnings(as.numeric(df$PERIOD))
    if (any(is.na(pd) & !is.na(df$PERIOD))) {
      errors <- c(errors, "PERIOD has non-numeric values.")
    } else {
      max_pd <- suppressWarnings(max(pd, na.rm = TRUE))
      if (is.finite(max_pd) && max_pd > 53) {
        errors <- c(errors, "PERIOD has values greater than 53. Weekly data should use 1-53; monthly data should use 1-12.")
      }
      if (any(pd < 1, na.rm = TRUE)) {
        errors <- c(errors, "PERIOD has values less than 1.")
      }
    }
  }

  if ("NO_DEATHS" %in% names(df)) {
    nd <- suppressWarnings(as.numeric(df$NO_DEATHS))
    if (any(is.na(nd) & !is.na(df$NO_DEATHS))) {
      errors <- c(errors, "NO_DEATHS has non-numeric values.")
    } else if (any(nd < 0, na.rm = TRUE)) {
      errors <- c(errors, "NO_DEATHS has negative values.")
    }
  }

  errors
}

# Event data has flexible column names: loadEventsData() converts any
# *_date / *_time columns to Date. We only require an event_name and
# at least one start_date / end_date pair.
validate_event_data <- function(df) {
  errors <- character(0)

  if (is.null(df)) {
    return("Event data is missing. The selected events sheet may be empty.")
  }
  if (!is.data.frame(df)) {
    return("Event data is not a data frame.")
  }
  if (nrow(df) == 0) {
    return("Event data is empty.")
  }

  has_event_name <- any(tolower(names(df)) %in% c("event_name", "event"))
  if (!has_event_name) {
    errors <- c(errors, "Event data is missing an 'event_name' column.")
  }

  has_start <- any(grepl("start_date|start_time", names(df), ignore.case = TRUE))
  has_end   <- any(grepl("end_date|end_time",     names(df), ignore.case = TRUE))
  if (!has_start || !has_end) {
    errors <- c(errors, "Event data must include start_date and end_date columns.")
  }

  errors
}

# Convenience wrapper used by the data module: runs both validators and
# returns NULL if everything passed, otherwise a single string suitable
# for a shinyalert modal.
validate_uploaded_data <- function(mortality_df, event_df) {
  errs <- c(
    validate_mortality_data(mortality_df),
    validate_event_data(event_df)
  )
  if (length(errs) == 0) return(NULL)
  paste(paste0("• ", errs), collapse = "\n")
}
