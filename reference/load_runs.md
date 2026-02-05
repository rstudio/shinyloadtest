# Create Tidy Load Test Results

The `shinycannon` tool creates a directory of log files for each load
test. This function translates one or more test result directories into
a tidy data frame.

## Usage

``` r
load_runs(..., verbose = vroom::vroom_progress())
```

## Arguments

- ...:

  Key-value pairs where the key is the desired name for the test and the
  value is a path to the test result directory.

- verbose:

  Whether or not to print progress for reading loadtest directories

## Value

A tidy data frame with the test result data. Each row is an event.
Columns include identifiers and timing information for the event. The
variables are as follows

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

- json:

  raw message JSON and parsed JSON of the event

## Output variables

- `run`: The name of the recording session.

- `session_id`: An incrementing integer value for every session within a
  `run`. Starts at 0.

- `user_id`: Which simulated user is performing the work within a `run`.
  Starts at 0.

- `iteration`: an incrementing integer value of the session iteration
  for the \#' matching `user_id`. Starts at 0.

- `input_line_number`: The line number corresponding to the event in the
  `recording.log` file.

- `event`: the web event being performed. One of the following values:

  - `REQ_HOME`: initial request for to load the homepage

  - `REQ_GET`: Request a supporting file (JavaScript / CSS)

  - `REQ_TOK`: Request a Shiny token

  - `REQ_SINF`: Request SockJS information

  - `REQ_POST`: Perform a POST query, such as uploading part of a file

  - `WS_RECV_BEGIN_UPLOAD`: A file upload is being requested

  - `WS_OPEN`: Open a new SockJS connection

  - `WS_RECV_INIT`: Initialize a new SockJS

  - `WS_SEND`: Send information from the Shiny server to the browser

  - `WS_RECV`: Send information from the browser to the Shiny server

  - `WS_CLOSE`: Close the SockJS connection

- `start`: Start time of the event relative to the beginning of the
  `run`'s maintenance period

- `end`: End time of the event relative to the beginning of the `run`'s
  maintenance period

- `time`: Total elapsed time of the event

- `concurrency`: A number of events that are being processed
  concurrently

- `maintenance`: A boolean determining whether or not all simulated
  users are executing a session

- `label`: A human readable event name

- `json`: The parsed JSON provided in the `recording.log` file. If the
  field `message` exists, a `message_parsed` field is added containing a
  parsed form of the SockJS's JSON message content.

## Examples

``` r
if (FALSE) { # \dontrun{
load_runs(
  `1 core` = "results/run-1/",
  `2 cores` = "results/run-2/"
)
} # }
```
