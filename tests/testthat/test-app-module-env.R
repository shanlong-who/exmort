test_that("the bundled application ships with the package", {
  app <- exmort_app_dir()

  expect_true(dir.exists(app))
  expect_true(file.exists(file.path(app, "app.R")))
  expect_true(dir.exists(file.path(app, "modules")))
})

test_that("app_module_env() loads into a private environment, not .GlobalEnv", {
  before <- ls(envir = globalenv(), all.names = TRUE)
  env <- app_module_env("data_validation.R")

  expect_true(is.environment(env))
  expect_false(identical(env, globalenv()))
  expect_true(exists("validate_mortality_data", envir = env, mode = "function"))
  expect_identical(ls(envir = globalenv(), all.names = TRUE), before)
})

test_that("source_module() loads each file only once unless forced", {
  env <- app_module_env()

  expect_true(env$source_module("modules/data_validation.R"))
  expect_false(env$source_module("modules/data_validation.R"))
  expect_true(env$source_module("modules/data_validation.R", force = TRUE))
})

test_that("an unknown module is reported clearly", {
  env <- app_module_env()

  expect_error(
    env$source_module("modules/no_such_module.R"),
    "Unknown application module"
  )
})

test_that("run_app() takes a launch.browser argument and passes on ...", {
  expect_true(is.function(run_app))
  expect_named(formals(run_app), c("launch.browser", "..."))
})
