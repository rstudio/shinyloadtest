# Changelog

## shinyloadtest (development version)

#### Bug Fixes

- R CMD check NOTE about undefined global variable `id` in
  [dplyr](https://dplyr.tidyverse.org) code
  ([\#182](https://github.com/rstudio/shinyloadtest/issues/182)).

- Fixed [\#179](https://github.com/rstudio/shinyloadtest/issues/179):
  Waterfall plot labels now support all message types as of {shiny}
  v1.9.1. This will remove the empty `Set:` and `Updated:` labels from
  the waterfall plot and replace them with an appropriate label.

## shinyloadtest 1.2.0

CRAN release: 2024-07-19

#### Bug Fixes

- Fixed [\#168](https://github.com/rstudio/shinyloadtest/issues/168):
  Remove deprecated usage of
  [`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
  during `shinyloadtest_report`.

- Fixed [\#163](https://github.com/rstudio/shinyloadtest/issues/163):
  `gtable_trim` error during `shinyloadtest_report` with newer versions
  of ggplot2.

## shinyloadtest 1.1.0

CRAN release: 2021-02-11

- [`record_session()`](https://rstudio.github.io/shinyloadtest/reference/record_session.md)
  gained a new variable `connect_api_key` to be able to record a session
  against RStudio Connect using a Connect API key. Please see [Load
  Testing Authenticated
  Apps](https://rstudio.github.io/shinyloadtest/articles/load-testing-authenticated-apps.html)
  for more details. Using a Connect API key also requires `shinycannon`
  \>= 1.1.0
  ([\#133](https://github.com/rstudio/shinyloadtest/issues/133))

- [`load_runs()`](https://rstudio.github.io/shinyloadtest/reference/load_runs.md)
  now uses vroom instead of `read_csv()`; this substantially improves
  its performance.

- The homepage has been rewritten to get to the big picture more
  quickly. Most of the details about shinycannon have moved to a new
  article.

- The analysis vignette has been overhauled to get into the plots more
  quickly, and provide more advice about interpretation.

- Plots have be thoroughly overhauled:

  - Waterfall plot now only shows plots in maintenance period, and times
    are session relative.

  - Sessions in warmup and cooldown are no longer shown in muted
    colours, because that unfortunately relied on a ggplot2 bug. Instead
    they’re simply omitted from the plot.

  - The line of best fit in the latency plots has been made less
    visually prominent, and no longer shows standard errors.

## shinyloadtest 1.0.1

CRAN release: 2020-01-09

This is the initial release of shinyloadtest to CRAN.

#### Bug fixes

- Server type detection is now tolerant of invalid HTML/XML markup
  produced by the target application
  ([\#115](https://github.com/rstudio/shinyloadtest/issues/115),
  [rstudio/shinycannon#38](https://github.com/rstudio/shinycannon/issues/38))
- Fixed a bug that prevented runs from being loaded
  ([\#124](https://github.com/rstudio/shinyloadtest/issues/124),
  [\#125](https://github.com/rstudio/shinyloadtest/issues/125))

#### Improvements

- The application server type is now captured in recording files. A
  corresponding enhancement was made to `shinycannon` that will produce
  a warning if the recording server type differs from the target
  ([\#107](https://github.com/rstudio/shinyloadtest/issues/107)),
  [rstudio/shinycannon#36](https://github.com/rstudio/shinycannon/pull/36))
- The way URLs are dealt with internally is now more robust, and should
  be tolerant of a wider variety of Shiny app deployment scenarios
  ([\#119](https://github.com/rstudio/shinyloadtest/issues/119)).
