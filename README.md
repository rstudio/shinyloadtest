# Load Testing Shiny Applications - WIP DO NOT USE (YET!)

The `shinyloadtest` package and the accompanying `shinycannon` software enable load testing 
deployed shiny applications.

Load tests ensure that deployed Shiny applications are ready for mission critical tasks. Load testing
helps identify performance bottlenecks (before users do!), test [asynchronous Shiny code](https://rstudio.github.io/promises), and identify optimal scaling settings or server specs (e.g. the necessary cpu and RAM to support an expected audience).

Just as importantly, load testing can squash the commnon concern that Shiny doesn't scale. In fact, we used these load testing tools to prove that more than 10,000 users could access a Shiny application at same time!

<p><a href="https://www.rstudio.com/resources/videos/scaling-shiny/?wvideo=mx214xmcqw"><img src="https://embedwistia-a.akamaihd.net/deliveries/97390a2a0512c2f959b37e3e91b0d9c81e3023c5.jpg?image_play_button_size=2x&amp;image_crop_resized=960x540&amp;image_play_button=1&amp;image_play_button_color=4287c7e0" width="400" height="225" style="width: 400px; height: 225px;"></a></p><p><a href="https://www.rstudio.com/resources/videos/scaling-shiny/?wvideo=mx214xmcqw">Scaling Shiny – RStudio</a></p>

To get started with `shinyloadtest`, read through the quick start guide below. Additional resources 
include details of `shinyloadtest`'s log format, a case study showing how to interpret load test results, an overview of using `shinyloadtest` with authenticated apps, and a description of how to use `shinyloadtest` to determine runtime settings in RStudio Connect or Shiny Server Pro.

## Quick Start

The process for load testing a Shiny application consists of three steps:

1. Record a typical user session for the app

2. Replay the session in parallel, simulating many simultaneous users accessing the app

3. Analyze the results of the load test

Rinse and repeat as necessary. Each step is described below.

### Step 1: Recording

First, install the `shinyloadtest` package. The package is not currently on CRAN, but can 
be installed with `devtools`:

```
devtools::install_github('shinyloadtest')
```

The `shinyloadtest` package contains one essential function, `recordSession`. The primary argument
to `recordSession` is the URL of the **deployed** application, 

//TODO
```r
shinyloadtest::recordSession('https://beta.rstudioconnect.com/)
```

Running the function will open a browser displaying the app. Once open, interact with the application as a normal user would and then close the browser. After closing the browser, a file will be created that contains a recording of the session. This recording, written to `recording.log` will serve as the basis for the load test.


### Step 2: Run the Load Test

With the recording in hand, we're almost ready to run the load test. The actual test is conducted outside of R using a standalone executable called `shinycannon`. `shinycannon` can be installed for [Mac](), [Windows](), or [Ubuntu](). `shinycannon` requires Java, see these [instructions]() for details.

Once installed, `shinycannon` can be used from the command line to execute a load test using the
recording file created in step 1. The main arguments to `shinycannon` are:

- ** **: The number of concurrent users to simulate. `shinycannon` uses separate parallel threads to represent each user.

- ** **: The duration of the load test. `shinycannon` will re-run the recording as necessary to meet the specified duration. For example, if the recording is 5 minutes long, but the desired test duration is 18 minutes, `shinycannon` will have each "user" repeat the session 4 times.

As an example, to run a load test simulating 100 concurrent users:

//TODO
```bash
shinycannon 
```

`shinycannon` includes detailed help documentation explaining the other arguments, available by running:

//TODO
```bash

```

During the test, `shinycannon` will report the progress and number of simulated users. The result of the test is an output directory, `//TODO` by default, which includes timing information for each session.

### Step 3: Analyze the Results

The output directory created in step 2 is the focus of step 3. The `shinyloadtest` R package provides 
functions for analyzing the output files from the test.

As a simple example, we can create a tidy dataframe with the test results and look at a few sample plots.

```r
//TODO
```

## Limitations

The load testing tool will not be able to record and load test applications if they:

