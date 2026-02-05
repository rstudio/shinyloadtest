# Example metrics for a 1-user load test

An example dataset like that returned by
[`load_runs`](https://rstudio.github.io/shinyloadtest/reference/load_runs.md),
but without the `json` variable for portability. Contains latency data
for 1813 `shinycannon` events suitable for passing to
[`shinyloadtest_report`](https://rstudio.github.io/shinyloadtest/reference/shinyloadtest_report.md).

## Usage

``` r
slt_demo_data_1
```

## Format

A data frame with 1813 rows and 12 variables:

- run:

  Name of the run

- session_id:

  simulated session identifier, 0-based

- user_id:

  simulated user identifier, 0-based

- iteration:

  user session identifier, 0-based

- input_line_number:

  recording line number associated with event

- event:

  type of the event

- start:

  time the event started, in seconds, relative to the time at which all
  simulated users were running.

- end:

  time the event ended, in seconds, relative to the time at which all
  simulated users were running

- time:

  event duration, in seconds

- concurrency:

  number of events that happened at the same time as this one

- maintenance:

  whether this event occurred before or after all simulated users were
  running

- label:

  event-specific text label
