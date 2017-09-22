# shinyloadtest
[![Build Status](https://travis-ci.org/rstudio/shinyloadtest.svg?branch=master)](https://travis-ci.org/rstudio/shinyloadtest)

[Shiny](https://shiny.rstudio.com) is a web framework for R. `shinyloadtest`
adds support for load testing Shiny applications using a the
[proxyrec](https://github.com/rstudio/proxyrec) node js package. 

**Why `shinyloadtest` ?**

Load testing has many uses:
-  ensure application performance during real-world use 
-  determine server specs
-  test and tune different server settings

For more details see:
[Performance Tuning for Shiny Server
Pro](https://support.rstudio.com/hc/en-us/articles/220546267-Scaling-and-Performance-Tuning-Applications-in-Shiny-Server-Pro),
[Performance Tuning for RStudio
Connect](https://support.rstudio.com/hc/en-us/articles/231874748), and
[Performance Tuning for
shinyapps.io](http://shiny.rstudio.com/articles/scaling-and-tuning.html).

**Why do I need another load testing tool?**

Shiny is not a good fit for traditional HTTP load testing tools like
ApacheBench, as they are designed for the stateless HTTP request/response model.
Shiny apps, on the other hand, do much of their communication over stateful
WebSockets, using Shiny's own application-level protocol.

`shinyloadtest` provides tools to interactively record a test and then rerun the
test at scale, mimicing hundreds or thousands of concurrent users.
`shinyloadtest` also captures and analyzes metrics to understand latency under
load.  

![](img/sample_concurrency_plot.png)

## Installation

Load testing shiny applications will require 2 tools: the `shinyloadtest` R
package and the `proxyrec` node js package. 

**Installing `proxyrec`**

`proxyrec` can be installed from pre-compiled executables or froms source.

*Executable*

Use the following links to download pre-compiled executables. The executable is just a bundling of `node js 8.4`'s executable and the `proxyrec` package.

|Operating System|
|----------------|
|[Mac OS](https://s3-us-west-2.amazonaws.com/rstudio-proxyrec-execs/rstudio/proxyrec/36/36.1/main-macos)|
|[Linux](https://s3-us-west-2.amazonaws.com/rstudio-proxyrec-execs/rstudio/proxyrec/36/36.1/main-linux)|


We recommend renaming the executable to `proxyrec`. To confirm a successful
download, run: 

```bash
./proxyrec --help
Commands:
  record	   Record a run
  playback <file>  Playback a run

Options:
  --help Show help
```

*From Source*

To install `proxyrec` and use the source package:

1. Install [node js 8](https://nodejs.org/en/download/current/)

2. Clone the `shinyloadtest` repository: `git clone https://github.com/rstudio/proxyrec.git`

3. Within the repository, run: `npm install`

4. For the remaining usage instructions, replace `./proxyrec` with `node
lib/main.js`. Commands must be run from within the repo.

**Installing `shinyloadtest`**

Next, install `shinyloadtest` by running the following in R:

```r
devtools::install_github("rstudio/shinyloadtest")
```

## Use

Load testing requires 3 steps: 

1. Recording user interactions with an app to a test file
2. Playing back the recorded test with a specified # of concurrent users for a
given duration
3. Analzying the playback results

Each step is described in a detailed article.

## Known limitations 

`shinyloadtest` does not work for all classes of shiny apps. Please submit an
issue with a [reprex](https://github.com/tidyverse/reprex) for applications that are not able to be tested.
 
- Subapps are not supported (or any page with Shiny apps in an iframe) 
- File uploads are not supported
- Applications that require authentication or use `session$user` information are not supported

## License

The R package and the `proxyrec` tool are both GPLv3.
