# Shared model helpers in inst/app/modules/common_functions.R. calculate_dates()
# builds the continuous day index that every baseline model regresses on, so
# it is worth pinning down exactly.

test_that("calculate_dates() builds a monthly day index anchored on min(YEAR)", {
  env <- module_env("common_functions.R")
  calculate_dates <- get("calculate_dates", envir = env, mode = "function")

  src <- data.frame(
    YEAR   = c(2020, 2020, 2021),
    PERIOD = c(1, 2, 1)
  )
  out <- calculate_dates(src, max_period = 12, nys = 2,
                         DOM = env$DOM, MOY = env$MOY)

  # Mid-month day numbers: Jan 2020 = 15, Feb 2020 = 31 + 15, Jan 2021 = 365 + 15.
  expect_equal(out$DATE, c(15, 46, 380))
})

test_that("calculate_dates() handles series that do not start in 2015", {
  env <- module_env("common_functions.R")
  calculate_dates <- get("calculate_dates", envir = env, mode = "function")

  # A pre-2015 start used to produce NA or a subscript error because the index
  # was hardcoded to 2015. The result must stay finite for any start year.
  early <- calculate_dates(
    data.frame(YEAR = c(2010, 2011), PERIOD = c(1, 1)),
    max_period = 12, nys = 2, DOM = env$DOM, MOY = env$MOY
  )
  expect_true(all(is.finite(early$DATE)))
  expect_equal(early$DATE, c(15, 380))

  late <- calculate_dates(
    data.frame(YEAR = c(2023, 2024), PERIOD = c(6, 6)),
    max_period = 12, nys = 2, DOM = env$DOM, MOY = env$MOY
  )
  expect_true(all(is.finite(late$DATE)))
})

test_that("calculate_dates() falls back to weekly midpoints without week labels", {
  env <- module_env("common_functions.R")
  calculate_dates <- get("calculate_dates", envir = env, mode = "function")

  src <- data.frame(
    YEAR = c(2020, 2020, 2021),
    PERIOD = c(1, 2, 1),
    DATE_TO_SPECIFY_WEEK = NA_character_
  )
  out <- calculate_dates(src, max_period = 52, nys = 2,
                         DOM = env$DOM, MOY = env$MOY)

  # Week midpoints: 3.5, 10.5, then a full year later.
  expect_equal(out$DATE, c(3.5, 10.5, 368.5))
})

test_that("the month constants are the calendar ones", {
  env <- module_env("common_functions.R")

  expect_equal(sum(env$DOM), 365)
  expect_length(env$MOY, 12)
  expect_equal(env$MOY[1], "Jan")
  expect_equal(env$MOY[12], "Dec")
})
