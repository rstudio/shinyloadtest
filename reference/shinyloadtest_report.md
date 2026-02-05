# Make shinyloadtest Report

Make shinyloadtest Report

## Usage

``` r
shinyloadtest_report(
  df,
  output = "shinyloadtest_report.html",
  duration_cutoff = c(attr(df, "recording_duration"), 60)[1],
  http_latency_cutoff = 5,
  max_websocket_cutoff = 20,
  open_browser = TRUE,
  self_contained = TRUE,
  verbose = TRUE
)
```

## Arguments

- df:

  data.frame returned from
  [`load_runs()`](https://rstudio.github.io/shinyloadtest/reference/load_runs.md)

- output:

  File where HTML output should be saved

- duration_cutoff:

  Cutoff value for session duration plot. Defaults to the recording
  duration used to simulate `df` or 60 seconds.

- http_latency_cutoff:

  Cutoff value for total http latency plot

- max_websocket_cutoff:

  Cutoff value for max websocket latency plot

- open_browser:

  Whether to open the created output in the browser

- self_contained:

  Boolean that determines if the final output should be a self contained
  html file

- verbose:

  Boolean that determines if progress output is displayed

## Value

The path to the report, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
shinyloadtest_report(slt_demo_data_1)
} # }
```
