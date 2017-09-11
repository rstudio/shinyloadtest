# shinyloadtest
[![Build Status](https://travis-ci.org/rstudio/shinyloadtest.svg?branch=master)](https://travis-ci.org/rstudio/shinyloadtest)


This package helps analyze logs created by a [proxyrec](https://github.com/rstudio/proxyrec) load test. Follow the instructions in [proxyrec](https://github.com/rstudio/proxyrec) for creating and running a load test for a [shiny](https://shiny.rstudio.com) application.

## Installation

Currently, `shinyloadtest` is available on Github:

```r
devtools::install_github("rstudio/shinyloadtest")
```

## Use

[proxyrec](https://github.com/rstudio/proxyrec) will create a directory of log files from a load test. Given a directory, this package includes functions for:

- creating a data frame from the log files (`createLog`)

- analyzing the number of concurrent connections over time (`getConncurrentOverTest` & `getMaxConcurrent`)

- analyzing latencies for page load and application use (`getPageLoadTimes` & `getSetInputtimes `)

- trimming a log to remove burnin and final tests (`trimRampUp` & `TODO`)

![](inst/sample_concurrency_plot.png)

The package also includes a sample R Markdown report:

```r
createLoadTestReport()
rmarkdown::render('load_test_template.Rmd', params = list(dirctory = '/path/to/proxyrec/results'))
```


## Use Cases

We recommend using [proxyrec](https://github.com/rstudio/proxyrec) and `shinyloadtest` to experiment with application performance and tuning. For more details see: [Performance Tuning for Shiny Server Pro](https://support.rstudio.com/hc/en-us/articles/220546267-Scaling-and-Performance-Tuning-Applications-in-Shiny-Server-Pro), [Performance Tuning for RStudio Connect](https://support.rstudio.com/hc/en-us/articles/231874748), and [Performance Tuning for shinyapps.io](http://shiny.rstudio.com/articles/scaling-and-tuning.html).


