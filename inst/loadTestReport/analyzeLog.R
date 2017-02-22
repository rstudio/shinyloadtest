library(dplyr)
library(lubridate)
library(dplyr)
library(assertthat)
library(tidyr)

## returns the successes from the eventLog as a data frame
getSuccesses <- function(eventLog) {
  assert_that(is.list(eventLog))

  successes <- lapply(seq_along(eventLog), function(i) {
    df <- NULL
    if (is.data.frame(eventLog[[i]])) {
      df <- eventLog[[i]]
    }
    df
  })
  successes <- do.call(rbind, successes)
  successes
}

## Returns the errors from eventLog
getErrors <- function(eventLog){
  assert_that(is.list(eventLog))

  errors <- lapply(seq_along(eventLog), function(i) {
    df <- NULL
    if (!is.data.frame(eventLog[[i]])) {
      df <- eventLog[[i]]
    }
    df
  })
  errors <- errors[!vapply(errors,is.null, logical(1))]
  errors
}

## Helper function
getEventInterval <- function(eventLog, startEvent, endEvent) {

  assert_that(startEvent %in% eventLog$event)
  assert_that(endEvent %in% eventLog$event)

  start <- eventLog %>%
    filter(event == startEvent) %>%
    select(start_time = time, connection, input)

  stop <- eventLog %>%
    filter(event == endEvent) %>%
    select(stop_time = time, connection, timedout)

  suppressMessages({
    result <- inner_join(start, stop) %>%
      mutate(start_time  = ymd_hms(start_time),
             stop_time = ymd_hms(stop_time),
             interval = interval(start_time, stop_time))
  })

  result
}


## returns the actual number of concurrent connections
## received by the server
getMaxConcurrent <- function(eventLog) {

  result <- getEventInterval(eventLog, "Shiny app started",
              "Closing PhantomJS session")

  maxCon <- 0

  for (i in 1:nrow(result)) {
    cur <- result[i,]
    othr <- result[-i,]
    curCon <- sum(int_overlaps(cur$interval, othr$interval)) + 1
    if (curCon > maxCon)
      maxCon <- curCon
  }

  maxCon
}

## returns the page load time
getPageLoadTime <- function(eventLog) {
  result <- getEventInterval(eventLog, "Navigating to Shiny app", "Shiny app started")
  result <- result %>%
    mutate(loadTime_s = as.numeric(as.duration(interval))) %>%
    select(connection, loadTime_s)
  result
}

## returns a dataframe with information on the duration for
## setInput events
getSetInputTime <- function(eventLog) {


  ## These come in pairs, so we need to do more work
  ## Start by adding a pair id
  result <- eventLog %>%
    filter(event == "Setting inputs" | event == "Finished setting inputs") %>%
    group_by(connection) %>%
    mutate(inputId = cumsum(!is.na(input)))

  ## Pull out the timing info and spread
  timing <- result %>%
    select(-input, -timedout) %>%
    spread(key = event, value = time)

  ## Pull out the input name and timedout result
  ## Collapse into one row per input
  info <- result %>%
    select(input, timedout, connection, inputId) %>%
    fill(input, .direction = "down") %>%
    fill(timedout, .direction = "up") %>%
    unique()

  ## Join the result and calculate duration
  result <- inner_join(timing, info, by = c("connection" = "connection", "inputId" = "inputId")) %>%
    rename(start_time = `Setting inputs`, end_time = `Finished setting inputs`) %>%
    mutate(time_s = as.numeric(as.duration(interval(start = start_time, end = end_time))))

  result
}


