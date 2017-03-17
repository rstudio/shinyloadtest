#' Get successes from event log
#'
#' @param eventLog Returned from \code{\link{loadTest}}. The \code{eventLog} is
#'   a list composed of data frames (from successful tests) and potnetially errors.
#'
#' @details Given an \code{eventLog} containing successesful test results (data
#'   frames) and error messages, this function returns the successful results in
#'   a collapsed data frame. See \code{\link{getErrors}}
#' @importFrom assertthat assert_that
#' @export
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

#' Get errors from event log
#'
#' @param eventLog Returned from \code{\link{loadTest}}. The \code{eventLog} is
#'   a list composed of data frames (from successful tests) and potnetially errors.
#'
#' @details Given an \code{eventLog} containing successesful test results (data
#'   frames) and error messages, this function returns the error messages in a
#'   list. See \code{\link{getSuccesses}}
#'
#' @export
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


#' Get duration for simple event pairs
#'
#' @param eventLog Data frame of events returned from either
#'   \code{\link{loadTest}} or \code{\link{getSuccesses}}
#' @param startEvent The first event in the pair, i.e. "Navigating to Shiny app"
#' @param endEvent The second event in the pair, i.e. "Shiny app started"
#' @param workerId Should the returned data frame include the worker id
#'
#' @return  A data frame containing the duration of the event pair per connection
#' @importFrom assertthat assert_that
#' @export
getEventInterval <- function(eventLog, startEvent, endEvent, workerId = TRUE) {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }

  assert_that(startEvent %in% eventLog$event)
  assert_that(endEvent %in% eventLog$event)

  start <- eventLog[which(eventLog$event == startEvent),
                    c("time", "connection")]
  colnames(start) <- c("start_time", "connection")

  end <- eventLog[which(eventLog$event == endEvent),
                    c("time", "connection")]
  colnames(end) <- c("end_time", "connection")

  suppressMessages({
    result <- merge(start, end)
    result$interval <-
      lubridate::interval(start = lubridate::ymd_hms(result$start_time),
        end = lubridate::ymd_hms(result$end_time)
      )
  })

  if (workerId) {
    worker <- eventLog[which(eventLog$event == startEvent),
                       c("workerid", "connection")]
    result <- merge(worker, result)
  }

  result
}


#' Return the number of concurrent connections
#'
#' @param eventLog Data frame of events returned from either
#'   \code{\link{loadTest}} or \code{\link{getSuccesses}}
#'
#' @details The actual number of concurrent connections for a load test can vary
#'   from the target. This function uses the conenct and disconnect events from
#'   the log to determine the interval when each connection was open. The
#'   maximum number of overlapping intervals is returned as the maxiumum number
#'   of concurrent connections achieved during the test.
#'
#' @export
getMaxConcurrent <- function(eventLog) {

  result <- getEventInterval(eventLog, "Shiny app started",
                             "Closing PhantomJS session")

  maxCon <- 0

  for (i in 1:nrow(result)) {
    cur <- result[i,]
    othr <- result[-i,]
    curCon <- sum(lubridate::int_overlaps(cur$interval, othr$interval)) + 1
    if (curCon > maxCon)
      maxCon <- curCon
  }

  maxCon
}


#' Get page load times
#'
#' @param eventLog Data frame of events returned from either
#'   \code{\link{loadTest}} or \code{\link{getSuccesses}}
#' @return Data frame containing the page load time per connection
#' @export
getPageLoadTimes <- function(eventLog) {
  result <- getEventInterval(eventLog, "Navigating to Shiny app", "Shiny app started")
  result$load_time_sec <- suppressMessages({as.numeric(result$interval)})
  result$interval <- NULL
  result[, names(result) %in% c("connection", "load_time_sec")]
}

#' Get test duration
#'
#' @param eventLog Data frame of events returned from either
#'   \code{\link{loadTest}} or \code{\link{getSuccesses}}
#' @return Data frame containing the test duration per connection.
#' @export
getTestDurations <- function(eventLog) {
  result <- getEventInterval(eventLog, "Shiny app started", "Closing PhantomJS session")
  result$test_duration_sec <- suppressMessages({as.numeric(result$interval)})
  result$interval <- NULL
  result[, names(result) %in% c("connection", "test_duration_sec")]
}


#' Get event duration for user interactions with inputs
#'
#' @param eventLog eventLog Data frame of events returned from either
#'   \code{\link{loadTest}} or \code{\link{getSuccesses}}
#'
#' @return Data frame containing a unique input id and event duration
#' @export
getSetInputTimes <- function(eventLog) {

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate needed for this function to work. Please install it.",
         call. = FALSE)
  }


  ## These come in pairs, so we need to do more work
  ## Start by adding a unique id for each input
  result <- eventLog[which(eventLog$event == "Setting inputs" | eventLog$event == "Finished setting inputs"),]
  numInputs <- length(result[which(result$connection == result$connection[1] & !is.na(result$input)), "input"])
  result$inputId <- cumsum(!is.na(result$input)) %% numInputs

  ## Pull out the timing info and spread
  start <- result[which(result$event == "Setting inputs"), names(result) != "timedout"]
  end <- result[which(result$event == "Finished setting inputs"), names(result) != "input"]
  merge_by <- c("inputId", "connection")
  result <- merge(start, end, by.x = merge_by, by.y = merge_by)

  result$interval <- lubridate::interval(start = lubridate::ymd_hms(result$time.x),
                       end = lubridate::ymd_hms(result$time.y))

  result$event_time_sec <- suppressMessages({as.numeric(result$interval)})

  result$input_id <- paste0("Action ", ifelse(result$inputId == 0, numInputs, result$inputId), ": ", result$input)

  desired_col <- c("connection", "input_id", "event_time_sec", "interval", "timedout")

  result[, names(result) %in% desired_col]

}


#' Get connections per R process
#'
#' @param eventLog A data frame with event information as returned from
#'   \code{getSuccesses}
#'
#' @return A data frame with 2 columns: \code{connection} and
#'   \code{other_connections}, the number of other connections sharing the R
#'   process along with \code{connection}.
#'
#' @details This function is useful to help determine if connections per R
#'   process is contributing to latency which is an indicator the utilization
#'   scheduler should be updated.
#' @export
getConnectionsPerR <- function(eventLog) {
  ids <- unique(eventLog$workerid)
  result <- getEventInterval(eventLog, "Shiny app started",
                             "Closing PhantomJS session", workerId = TRUE)

  result$other_connections <- 0
  for (i in 1:nrow(result)) {
    cur <- result[i,]
    othr <- result[-i,]
    othr_same_proc <- othr[othr$workerid == cur$workerid,]
    othr_same_proc_interval <- sum(lubridate::int_overlaps(cur$interval, othr_same_proc$interval))
    result$other_connections[i] <- othr_same_proc_interval
  }
  desired_cols <- c("connection", "other_connections")
  result <- result[, names(result) %in% desired_cols]
  result
}
