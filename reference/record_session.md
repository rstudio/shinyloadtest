# Record a Session for Load Test

This function creates a [reverse
proxy](https://en.wikipedia.org/wiki/Reverse_proxy) at
`http://host:port` (http://127.0.0.1:8600 by default) that intercepts
and records activity between your web browser and the Shiny application
at `target_app_url`.

## Usage

``` r
record_session(
  target_app_url,
  host = "127.0.0.1",
  port = 8600,
  output_file = "recording.log",
  open_browser = TRUE,
  connect_api_key = NULL
)
```

## Arguments

- target_app_url:

  The URL of the deployed application.

- host:

  The host where the proxy will run. Usually localhost is used.

- port:

  The port for the reverse proxy. Default is 8600. Change this default
  if port 8600 is used by another service.

- output_file:

  The name of the generated recording file.

- open_browser:

  Whether to open a browser on the proxy (default=`TRUE`) or not
  (`FALSE`).

- connect_api_key:

  An Posit Connect api key. It may be useful to use
  `Sys.getenv("CONNECT_API_KEY")`.

## Value

Creates a recording file that can be used as input to the `shinycannon`
command-line load generation tool.

## Details

By default, after creating the reverse proxy, a web browser is opened
automatically. As you interact with the application in the web browser,
activity is written to the `output_file` (`recording.log` by default).

To shut down the reverse proxy and complete the recording, close the web
browser tab or window.

Recordings are used as input to the `shinycannon` command-line
load-generation tool which can be obtained from the [shinyloadtest
documentation site](https://rstudio.github.io/shinyloadtest/index.html).

## `fileInput`/`DT`/`HTTP POST` support

Shiny's
[`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html)
input for uploading files, the `DT` package, and potentially other
packages make HTTP POST requests to the target application. Because POST
requests can be large, they are not stored directly in the recording
file. Instead, new files adjacent to the recording are created for each
HTTP POST request intercepted.

The adjacent files are named after the recording with the pattern
`<output_file>.post.<N>`, where `<output_file>` is the chosen recording
file name and `<N>` is the number of the request.

If present, these adjacent files must be kept alongside the recording
file when the recording is played back with the `shinycannon` tool.

## See also

[`shinyloadtest` articles](https://rstudio.github.io/shinyloadtest/)

## Examples

``` r
if (FALSE) { # \dontrun{
record_session("https://example.com/your-shiny-app/")
} # }
```
