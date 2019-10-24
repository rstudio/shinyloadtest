Load Testing Shiny Applications
===============================

The `shinyloadtest` package and the accompanying
[`shinycannon`](https://github.com/rstudio/shinycannon) software enable
load testing deployed Shiny applications.

Load testing helps developers and administrators estimate how many users
their application can support. If an application requires tuning, load
testing and load test result analysis can be used to identify
performance bottlenecks and to guide changes to infrastructure,
configuration, or code.

It’s a common misconception that **“Shiny doesn’t scale.”** In
actuality, properly-architected Shiny applications can be scaled
horizontally, a fact which Sean Lopp was recently able to demonstrate at
rstudio::conf 2018. We used `shinycannon` to simulate 10,000 concurrent
users interacting with an application deployed to AWS. You can see a
recording of Sean’s talk and the load test demonstration here: [Scaling
Shiny](https://resources.rstudio.com/rstudio-conf-2018/scaling-shiny-sean-lopp)

To get started with `shinyloadtest`, read through the quick start guide
below.

![Analysis Example](./reference/figures/slt_report_screenshot.png)

Installation
------------

To perform a load test you’ll need two pieces of software:
`shinyloadtest` and `shinycannon`.

-   `shinyloadtest` is an R package used to generate recordings and
    analyze results. You should install it on your development machine.
    If you are installing `shinyloadtest` within a Linux environment,
    you will need to install `OpenSSL >= 1.0.1` and its associated
    development libraries as those are required by the
    [`websocket`](https://github.com/rstudio/websocket) package. On
    Debian/Ubuntu systems, install the `libssl1.0` and `libssl1.0-dev`
    software packages. On Redhat/Fedora/CentOS systems, install the
    `openssl` and `openssl-devel` software packages.
-   `shinycannon` is the command-line replay tool. You can install it on
    your development machine for testing, but for best results we
    recommend installing it on a server, and preferably not the one the
    application under test is also on.

### `shinyloadtest`

    devtools::install_github('rstudio/shinyloadtest')

### `shinycannon`

As opposed to the `shinyloadtest` R package, `shinycannon` is installed
and run differently depending on platform.

#### Linux

Depending on your distribution, `shinycannon` can be installed using one
of the following packages.

<table>
<caption>Linux Packages</caption>
<thead>
<tr class="header">
<th style="text-align: left;">Distribution</th>
<th style="text-align: left;">Download Link</th>
<th style="text-align: left;">Install Command</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Ubuntu/Debian</td>
<td style="text-align: left;"><a href='https://s3.amazonaws.com/rstudio-shinycannon-build/2019-10-23-19:28:20_1.0.0-38be139/deb/shinycannon_1.0.0-38be139_amd64.deb'>shinycannon_1.0.0-38be139_amd64.deb</a></td>
<td style="text-align: left;"><code>sudo dpkg -i shinycannon_1.0.0-38be139_amd64.deb</code></td>
</tr>
<tr class="even">
<td style="text-align: left;">Redhat/Fedora/CentOS</td>
<td style="text-align: left;"><a href='https://s3.amazonaws.com/rstudio-shinycannon-build/2019-10-23-19:28:20_1.0.0-38be139/rpm/shinycannon-1.0.0-38be139.x86_64.rpm'>shinycannon-1.0.0-38be139.x86_64.rpm</a></td>
<td style="text-align: left;"><code>sudo yum install -y shinycannon-1.0.0-38be139.x86_64.rpm</code></td>
</tr>
<tr class="odd">
<td style="text-align: left;">SUSE</td>
<td style="text-align: left;"><a href='https://s3.amazonaws.com/rstudio-shinycannon-build/2019-10-23-19:28:20_1.0.0-38be139/rpm/shinycannon-1.0.0-suse-38be139.x86_64.rpm'>shinycannon-1.0.0-suse-38be139.x86_64.rpm</a></td>
<td style="text-align: left;"><code>zypper —-no-gpg-checks install -y shinycannon-1.0.0-suse-38be139.x86_64.rpm</code></td>
</tr>
</tbody>
</table>

#### macOS

1.  Install [Java](https://www.java.com/en/download/)
2.  Download
    <a href='https://s3.amazonaws.com/rstudio-shinycannon-build/2019-10-23-19:28:20_1.0.0-38be139/bin/shinycannon-1.0.0-38be139.sh'>shinycannon-1.0.0-38be139.sh</a>
3.  Install with
    `sudo cp shinycannon-1.0.0-38be139.sh /usr/local/bin/shinycannon; sudo chmod +x /usr/local/bin/shinycannon`
4.  Run `shinycannon -h` to see help output, or
    `shinycannon [RECORDING-PATH] [APP-URL]` to generate load.

#### Windows

1.  Install [Java](https://www.java.com/en/download/)
2.  Download
    <a href='https://s3.amazonaws.com/rstudio-shinycannon-build/2019-10-23-19:28:20_1.0.0-38be139/jar/shinycannon-1.0.0-38be139.jar'>shinycannon-1.0.0-38be139.jar</a>
    to the directory you wish to run it in
3.  Run `java -jar shinycannon-1.0.0-38be139.jar -h` to see help output,
    or
    `java -jar shinycannon-1.0.0-38be139.jar [RECORDING-PATH] [APP-URL]`
    to generate load.

Quick Start
-----------

The process for load testing a Shiny application consists of three
steps:

1.  **Record** a typical user session for the app
2.  **Replay** the session in parallel, simulating many simultaneous
    users accessing the app
3.  **Analyze** the results of the load test and determine if the app
    performed well enough

Rinse and repeat as necessary. Each step is described below.

### Step 1: Recording

Record a session using the `record_session` function in the
`shinyloadtest` package, which takes the URL of the **deployed**
application as an argument:

    shinyloadtest::record_session('https://shinyapp.example.com/')

You should substitute `https://shinyapp.example.com/` for the URL of the
Shiny app you’d like to record. If your application requires
authentication you should consult the [authentication
article](articles/load-testing-authenticated-apps.html). You should also
be aware that [certain Shiny
features](articles/limitations-of-shinyloadtest.html) are not compatible
with shinyloadtest.

Running the function will open a browser displaying the app. Once open,
interact with the application as a normal user would and then close the
browser. After closing the browser window or tab, a file will be created
that contains a recording of the session. This recording, written to
`recording.log` by default, will serve as the basis for the load test.

### Step 2: Run the Load Test

With the recording in hand, we’re ready to run the load test. The actual
test is conducted outside of R using the `shinycannon` command-line
tool. You can run it using your system’s terminal or console program, or
you can run it from the RStudio terminal tab.

`shinycannon` accepts two required positional arguments:

1.  **\[RECORDING-PATH\]**: path to the file produced by
    `shinyloadtest::record_session`
2.  **\[APP-URL\]**: URL of the target Shiny application

In addition to these two required arguments, `shinycannon` accepts a
number of optional arguments that can be specified with flags. Of these,
the most interesting are:

1.  **–workers**: The number of concurrent users to simulate.
    `shinycannon` uses threads to represent each user. It defaults to 1.
2.  **–loaded-duration-minutes**: The duration of the load test, in
    minutes. This does not include “warmup time”, which is the time
    shinycannon spends gradually increasing the number of workers, or
    “cooldown time”, which is the time spent decreasing the number of
    workers. It defaults to 0, meaning that after all workers have
    “warmed up”, they will immediately begin to “cool down”. Generally,
    you’ll want to set this to a duration greater than 0. `shinycannon`
    will re-run the recording as necessary to fill the time.
3.  **–output-dir**: Name of the directory to create and store timing
    information in.

As an example, to run a load test simulating 5 concurrent users for at
least 2 minutes, outputting to the directory `run1`:

    $ shinycannon recording.log https://shinyapp.example.com/ --workers 5 --loaded-duration-minutes 2 --output-dir run1
    2018-08-29 15:06:14.191 INFO [progress] - Running: 0, Failed: 0, Done: 0
    2018-08-29 15:06:14.193 INFO [thread01] - Warming up
    2018-08-29 15:06:14.195 INFO [thread00] - Waiting for warmup to complete
    2018-08-29 15:06:19.193 INFO [progress] - Running: 1, Failed: 0, Done: 0
    2018-08-29 15:06:24.194 INFO [progress] - Running: 1, Failed: 0, Done: 0
    2018-08-29 15:06:29.083 INFO [thread02] - Warming up
    2018-08-29 15:06:29.195 INFO [progress] - Running: 1, Failed: 0, Done: 0
    2018-08-29 15:06:34.195 INFO [progress] - Running: 2, Failed: 0, Done: 0
    2018-08-29 15:06:39.196 INFO [progress] - Running: 2, Failed: 0, Done: 0
    2018-08-29 15:06:43.973 INFO [thread03] - Warming up
    2018-08-29 15:06:44.196 INFO [progress] - Running: 2, Failed: 0, Done: 0
    2018-08-29 15:06:49.196 INFO [progress] - Running: 3, Failed: 0, Done: 0
    2018-08-29 15:06:54.201 INFO [progress] - Running: 3, Failed: 0, Done: 0
    2018-08-29 15:06:58.862 INFO [thread04] - Warming up
    2018-08-29 15:06:59.201 INFO [progress] - Running: 3, Failed: 0, Done: 0
    2018-08-29 15:07:04.201 INFO [progress] - Running: 4, Failed: 0, Done: 0
    2018-08-29 15:07:09.202 INFO [progress] - Running: 4, Failed: 0, Done: 0
    2018-08-29 15:07:13.751 INFO [thread05] - Warming up
    2018-08-29 15:07:13.752 INFO [thread00] - Maintaining for 2 minutes (120000 ms)
    2018-08-29 15:07:14.202 INFO [progress] - Running: 4, Failed: 0, Done: 0
    2018-08-29 15:07:19.202 INFO [progress] - Running: 5, Failed: 0, Done: 0
    2018-08-29 15:07:24.202 INFO [progress] - Running: 5, Failed: 0, Done: 0
    ...

`shinycannon` includes detailed help documentation explaining the other
arguments:

    shinycannon -h

During the test, `shinycannon` reports the progress and number of
simulated users. The result of the test is an output directory (`run1`
in the example above) which includes timing information for each
session.

### Step 3: Analyze the Results

The output directory created in step 2 (`run1`) is the focus of step 3.
The `shinyloadtest` R package provides functions for analyzing the
output files from the test.

A tidy data frame of run simulation information can be created using
`shinyloadtest::load_runs`.

    df <- shinyloadtest::load_runs("5 workers" = "./run1")

Once the data is loaded into R, a report can be generated using:

    shinyloadtest::shinyloadtest_report(df, "run1.html")

This self contained html report will be opened in your browser for
inspection. For further analysis explaination, please visit [‘Analysing
load test logs’](articles/analyzing-load-test-logs.html).

Helpful Links
=============

-   [RStudio Community](https://community.rstudio.com)
-   [shinycannon](https://github.com/rstudio/shinycannon)
-   Articles:
    -   [Analysing load test
        logs](articles/analyzing-load-test-logs.html)
    -   [Load test case study](articles/case-study-scaling.html)
    -   [Limitations of
        `shinyloadtest`](articles/limitations-of-shinyloadtest.html)
    -   [Load testing authenticated
        apps](articles/load-testing-authenticated-apps.html)
        <!--  * [FAQ](articles/faq.html) -->
