# Launch the All-Cause and Excess Mortality Calculator

Starts the bundled 'shiny' application in your default web browser. The
app runs entirely on your own machine and does not upload any data.

## Usage

``` r
run_app(launch.browser = interactive(), ...)
```

## Arguments

- launch.browser:

  Logical; whether to open the app in a web browser. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- ...:

  Additional arguments passed on to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (for example
  `port` or `host`).

## Value

Called for its side effect of running the Shiny application; invisibly
returns `NULL` when the app is closed.

## Details

The application is copied to a per-session temporary directory before it
is launched, so it never writes into the (possibly read-only) package
installation directory.

## Examples

``` r
if (interactive()) {
  # Launch the calculator in your browser:
  run_app()
}
```
