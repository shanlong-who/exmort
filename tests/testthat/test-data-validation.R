# Upload validators in inst/app/modules/data_validation.R. These run between
# data load and merge and are the app's only guard against a malformed
# spreadsheet reaching the models.

valid_mortality <- function() {
  data.frame(
    AREA      = "Total",
    AGE_GROUP = "All",
    SEX       = "Both",
    YEAR      = c(2019, 2019, 2020),
    PERIOD    = c(1, 2, 1),
    DAYS      = 7,
    NO_DEATHS = c(120, 131, 145),
    stringsAsFactors = FALSE
  )
}

valid_events <- function() {
  data.frame(
    event_name = c("COVID-19 wave 1", "Typhoon"),
    start_date = as.Date(c("2020-03-01", "2020-11-01")),
    end_date   = as.Date(c("2020-06-30", "2020-11-15")),
    stringsAsFactors = FALSE
  )
}

test_that("clean mortality data passes validation", {
  validate_mortality_data <- module_fun("data_validation.R", "validate_mortality_data")

  expect_length(validate_mortality_data(valid_mortality()), 0)
})

test_that("missing or empty mortality data is reported", {
  validate_mortality_data <- module_fun("data_validation.R", "validate_mortality_data")

  expect_match(validate_mortality_data(NULL), "missing")
  expect_match(validate_mortality_data("not a data frame"), "not a data frame")
  expect_match(validate_mortality_data(valid_mortality()[0, ]), "empty")
})

test_that("missing required columns are named in the error", {
  validate_mortality_data <- module_fun("data_validation.R", "validate_mortality_data")

  df <- valid_mortality()
  df$NO_DEATHS <- NULL
  df$SEX <- NULL

  errors <- validate_mortality_data(df)
  expect_length(errors, 1)
  expect_match(errors, "SEX")
  expect_match(errors, "NO_DEATHS")
})

test_that("out-of-range and non-numeric values are caught", {
  validate_mortality_data <- module_fun("data_validation.R", "validate_mortality_data")

  bad_year <- valid_mortality()
  bad_year$YEAR <- c(2019, 2019, 1500)
  expect_match(validate_mortality_data(bad_year), "1900-2100", all = FALSE)

  bad_period <- valid_mortality()
  bad_period$PERIOD <- c(1, 2, 54)
  expect_match(validate_mortality_data(bad_period), "greater than 53", all = FALSE)

  zero_period <- valid_mortality()
  zero_period$PERIOD <- c(0, 1, 2)
  expect_match(validate_mortality_data(zero_period), "less than 1", all = FALSE)

  negative_deaths <- valid_mortality()
  negative_deaths$NO_DEATHS <- c(120, -1, 145)
  expect_match(validate_mortality_data(negative_deaths), "negative", all = FALSE)

  text_deaths <- valid_mortality()
  text_deaths$NO_DEATHS <- c("120", "n/a", "145")
  expect_match(validate_mortality_data(text_deaths), "non-numeric", all = FALSE)
})

test_that("week 53 is accepted", {
  validate_mortality_data <- module_fun("data_validation.R", "validate_mortality_data")

  df <- valid_mortality()
  df$PERIOD <- c(1, 52, 53)
  expect_length(validate_mortality_data(df), 0)
})

test_that("event data needs a name and a start/end pair", {
  validate_event_data <- module_fun("data_validation.R", "validate_event_data")

  expect_length(validate_event_data(valid_events()), 0)

  no_name <- valid_events()
  names(no_name)[1] <- "label"
  expect_match(validate_event_data(no_name), "event_name", all = FALSE)

  no_end <- valid_events()
  no_end$end_date <- NULL
  expect_match(validate_event_data(no_end), "start_date and end_date", all = FALSE)

  expect_match(validate_event_data(NULL), "missing")
  expect_match(validate_event_data(valid_events()[0, ]), "empty")
})

test_that("validate_uploaded_data() returns NULL when both inputs are clean", {
  validate_uploaded_data <- module_fun("data_validation.R", "validate_uploaded_data")

  expect_null(validate_uploaded_data(valid_mortality(), valid_events()))
})

test_that("validate_uploaded_data() collapses all problems into one message", {
  validate_uploaded_data <- module_fun("data_validation.R", "validate_uploaded_data")

  bad_mortality <- valid_mortality()
  bad_mortality$NO_DEATHS <- c(120, -1, 145)
  bad_events <- valid_events()
  bad_events$end_date <- NULL

  message <- validate_uploaded_data(bad_mortality, bad_events)
  expect_type(message, "character")
  expect_length(message, 1)
  expect_match(message, "negative")
  expect_match(message, "start_date and end_date")
  # One bullet ("•") per problem found.
  expect_equal(lengths(regmatches(message, gregexpr("•", message))), 2L)
})
