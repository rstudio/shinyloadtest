# Load Testing Shiny Applications

> This is a WIP, do not use yet. Please follow community.rstudio.com for a
soft launch of the tools.

The `shinyloadtest` package and the accompanying `shinycannon` software enable load testing
deployed shiny applications.

Load tests ensure that deployed Shiny applications are ready for mission critical tasks. Load testing
helps identify performance bottlenecks (before users do!), test [asynchronous Shiny code](https://rstudio.github.io/promises), and identify optimal scaling settings or server specs (e.g. the necessary cpu and RAM to support an expected audience).

Just as importantly, load testing can squash the commnon concern that Shiny doesn't scale. In fact, we used these load testing tools to prove that more than 10,000 users could access a Shiny application at same time!

<p><a href="https://www.rstudio.com/resources/videos/scaling-shiny/?wvideo=mx214xmcqw"><img src="https://embedwistia-a.akamaihd.net/deliveries/97390a2a0512c2f959b37e3e91b0d9c81e3023c5.jpg?image_play_button_size=2x&amp;image_crop_resized=960x540&amp;image_play_button=1&amp;image_play_button_color=4287c7e0" width="400" height="225" style="width: 400px; height: 225px;"></a></p><p><a href="https://www.rstudio.com/resources/videos/scaling-shiny/?wvideo=mx214xmcqw">Scaling Shiny – RStudio</a></p>

To get started with `shinyloadtest`, read through the quick start guide below. Additional resources
include details of `shinyloadtest`'s log formats, a case study showing how to interpret load test results, an overview of using `shinyloadtest` with authenticated apps, and a description of how to use `shinyloadtest` to determine runtime settings in RStudio Connect or Shiny Server Pro.

## Installation

To perform a load test you'll need two pieces of software: `shinyloadtest` and `shinycannon`.

To install `shinyloadtest`:

```R
devtools::install_github('rstudio/shinyloadtest')
```

`shinycannon` is a standalone executable:

Platform | Download URL | Install Command
-- | -- | --
Max OS | |
Ubuntu | [download](https://s3.amazonaws.com/rstudio-shinycannon-build/2018-06-14-06%3A15%3A50_1.0.0-31565b0/deb/shinycannon_1.0.0-31565b0_amd64.deb) | `sudo gdebi shinycannon_1.0.0-31565b0_amd64.db`
Windows | |


## Quick Start

The process for load testing a Shiny application consists of three steps:

1. Record a typical user session for the app

2. Replay the session in parallel, simulating many simultaneous users accessing the app

3. Analyze the results of the load test

Rinse and repeat as necessary. Each step is described below.

### Step 1: Recording

Record a session using the `recordSession` function, which takes the URL of the **deployed** application:


```r
shinyloadtest::record_session('https://beta.rstudioconnect.com/content/3239/')
```

Running the function will open a browser displaying the app. Once open, interact with the application as a normal user would and then close the browser. After closing the browser, a file will be created that contains a recording of the session. This recording, written to `recording.log` by default, will serve as the basis for the load test.


### Step 2: Run the Load Test

With the recording in hand, we're ready to run the load test. The actual test is conducted outside of R using the stand alone `shinycannon` tool.

`shinycannon` can be used from the command line. An easy option is to use `shinycannon` from the RStudio terminal. The main arguments to `shinycannon` are:

- **sessions**: The number of concurrent users to simulate. `shinycannon` uses separate parallel threads to represent each user.

- **loaded-duration-minutes**: The duration of the load test. `shinycannon` will re-run the recording as necessary to meet the specified duration. For example, if the recording is 5 minutes long, but the desired test duration is 18 minutes, `shinycannon` will

As an example, to run a load test simulating 100 concurrent users for 20 minutes:

```bash
shinycannon recording.log https://beta.rstudioconnect.com/content/3239/ --sessions=100 --loaded-duration-minutes=20s

Logging at INFO level to output_dir/detail.log
2018-06-15T11:22:29.465037Z - Running: 0, Failed: 0, Done: 0
2018-06-15T11:22:29.466399Z - Worker thread 1 warming up
2018-06-15T11:22:29.476858Z - Worker thread 2 warming up
2018-06-15T11:22:29.477245Z - Worker thread 3 warming up
                           ...
2018-06-15T11:22:29.499708Z - Waiting for warmup to complete
                           ...
2018-06-15T11:22:29.516885Z - Maintaining for 20 minutes
2018-06-15T11:28:34.465847Z - Running: 100, Failed: 0, Done: 0
                           ...
2018-06-15T11:44:29.517080Z - Stopped maintaining, waiting for workers to stop
2018-06-15T11:48:34.465847Z - Running: 60, Failed: 0, Done: 40
                           ...
```

`shinycannon` includes detailed help documentation explaining the other arguments:

```bash
shinycannon --help
```

During the test, `shinycannon` reports the progress and number of simulated users. The result of the test is an output directory which includes timing information for each session.

### Step 3: Analyze the Results

The output directory created in step 2 is the focus of step 3. The `shinyloadtest` R package provides
functions for analyzing the output files from the test.

```r
library(shinyloadtest)
results <- tidy_loadtest(
      `20 users` = 'load-test-results',
      `1 user` = 'baseline-results'
)
results
```

![](inst/img/results.png)

```r
results %>%
   hist_loadtimes()
results %>%
   plot_time_boxplot()
```

![](inst/img/hist_loadtime.png)

![](inst/img/boxplot.png)


Load the results into a shiny flex dashboard
```r
results %>%
  make_report() %>%
  rmarkdown::run()
```
