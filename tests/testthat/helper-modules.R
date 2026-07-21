# Shared fixtures. The Shiny application lives in inst/app and is loaded into
# a private environment so its helper functions can be tested directly.

.module_cache <- new.env(parent = emptyenv())

# Load a module file once per test run and return the environment it lives in.
module_env <- function(module) {
  if (is.null(.module_cache[[module]])) {
    .module_cache[[module]] <- app_module_env(module)
  }
  .module_cache[[module]]
}

# Fetch a single function from a module file.
module_fun <- function(module, name) {
  get(name, envir = module_env(module), mode = "function")
}
