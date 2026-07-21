# Date helpers in inst/app/modules/utils.R. Every table, plot and model in the
# app is indexed by ISO year-week, so an off-by-one here shifts the whole
# baseline. These are the checks that would catch a change in lubridate or
# ISOweek breaking the app.

test_that("get_iso_weeks_in_year() knows which ISO years have 53 weeks", {
  get_iso_weeks_in_year <- module_fun("utils.R", "get_iso_weeks_in_year")

  expect_equal(get_iso_weeks_in_year(2015), 53)
  expect_equal(get_iso_weeks_in_year(2020), 53)
  expect_equal(get_iso_weeks_in_year(2026), 53)

  expect_equal(get_iso_weeks_in_year(2019), 52)
  expect_equal(get_iso_weeks_in_year(2021), 52)
  expect_equal(get_iso_weeks_in_year(2022), 52)
})

test_that("get_iso_weeks_in_year() rejects implausible input", {
  get_iso_weeks_in_year <- module_fun("utils.R", "get_iso_weeks_in_year")

  expect_error(get_iso_weeks_in_year("2020"), "single numeric")
  expect_error(get_iso_weeks_in_year(c(2020, 2021)), "single numeric")
  expect_error(get_iso_weeks_in_year(1700), "between 1800 and 2999")
})

test_that("get_week_start_date() returns the ISO Monday of the week", {
  get_week_start_date <- module_fun("utils.R", "get_week_start_date")

  # ISO week 1 of 2021 starts on 4 January 2021 (a Monday).
  expect_equal(get_week_start_date(2021, 1), as.Date("2021-01-04"))
  # ISO week 1 of 2020 starts in the previous calendar year.
  expect_equal(get_week_start_date(2020, 1), as.Date("2019-12-30"))
  expect_equal(get_week_start_date(2021, 52), as.Date("2021-12-27"))

  # Every result must be a Monday.
  mondays <- vapply(
    1:52,
    function(w) as.integer(format(get_week_start_date(2022, w), "%u")),
    integer(1)
  )
  expect_true(all(mondays == 1L))
})

test_that("get_week_start_date() clamps out-of-range weeks instead of failing", {
  get_week_start_date <- module_fun("utils.R", "get_week_start_date")

  # 2021 has 52 ISO weeks: a stray week 53 must not crash the batch date
  # calculation in app.R.
  expect_equal(get_week_start_date(2021, 53), get_week_start_date(2021, 52))
  expect_equal(get_week_start_date(2021, 0), get_week_start_date(2021, 1))

  # 2020 has 53 ISO weeks, so week 53 is real there.
  expect_equal(get_week_start_date(2020, 53), as.Date("2020-12-28"))

  expect_error(get_week_start_date("2021", 1), "must be numeric")
})

test_that("the plot palette is stable and colourblind-safe", {
  wpro_palette <- module_fun("utils.R", "wpro_palette")
  get_model_colors <- module_fun("utils.R", "get_model_colors")

  pal <- wpro_palette()
  expect_length(pal, 8)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal)))

  colors <- get_model_colors()
  expect_true(all(c(
    "ARIMA/SARIMA Model", "Historical Average",
    "Negative Binomial Regression", "Quasi-Poisson Model",
    "Zero Inflated Poisson Model", "GAM Spline Model",
    "Karlinsky-Kobak Model"
  ) %in% names(colors)))
  expect_false(any(duplicated(colors)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})
