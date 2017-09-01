#' Create log file from directory containing run logs
#'
#' @param logsDir directory containing run logs, see \code{loadtest}
#'
#' @return data frame with raw event logs from successful tests
#' @export
createLog <- function(logsDir) {
  assertthat::assert_that(dir_exists(logsDir))

  files <- list.files(logsDir)
  log <- list()

  for (i in seq_along(files)) {
    fname <- file.path(logsDir, files[i])
    f <- read.table(fname, stringsAsFactors = FALSE)
    colnames(f) <- c('eventid', 'time', 'elapsed_ms', 'event')

    # check for unique REQ_HOME and WS_CLOSE
    checkUniqueEvent(f, "REQ_HOME", fname)
    checkUniqueEvent(f, "WS_CLOSE", fname)

    parsed <- parseLogName(fname)
    f$process <- parsed$process
    f$connection <- i

    log[[i]] <- f
  }

  logdf <- do.call(rbind, log)
  logdf$time <- lubridate::ymd_hms(logdf$time)

  logdf
}

# we need to be sure that we have unique events for some actions
# if we don't, it was likely because the user visited more than one
# page (not just the shiny app) while recording
checkUniqueEvent <- function(logfile, event, fname) {
  if (sum(logfile$event == event) != 1)
    stop(paste0("Error: Playback file ", fname,
                " should have 1 instance of `", event,
                "`. Perhaps you recorded a bad test?"))
}

parseLogName <- function(logName) {
  parseNum <- function(x, pat) {
    pat_match <- regmatches(x, regexpr(pat, x))
    pat_match_strip <- gsub("_", "", pat_match, fixed = TRUE)
    as.numeric(pat_match_strip)
  }

  process <- parseNum(logName, "\\_\\d+\\_")
  con <- parseNum(logName, "\\_\\d+\\.")

  return(list(con = con, process = process))
}


#' Get duration for simple event pairs
#'
#' @param eventLog Data frame of events returned from \code{\link{createLog}}
#' @param startEvent The first event in the pair, i.e. "REQ_HOME"
#' @param endEvent The second event in the pair, i.e. "WS_CLOSE"
#'
#' @return  A data frame containing the duration of the event pair per connection
#' @importFrom assertthat assert_that
#' @export
getEventInterval <- function(eventLog, startEvent, endEvent) {

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


  result
}


#' Return the number of concurrent connections
#'
#' @param eventLog Data frame of events returned from e\code{\link{createLog}}
#'
#' @details The actual number of concurrent connections for a load test can vary
#'   from the target. This function uses the connect and disconnect events from
#'   the log to determine the interval when each connection was open. The
#'   maximum number of overlapping intervals is returned as the maxiumum number
#'   of concurrent connections achieved during the test.
#'
#' @export
getMaxConcurrent <- function(eventLog) {

  result <- getEventInterval(eventLog, "REQ_HOME",
                             "WS_CLOSE")

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



#' Get concurrent connections over the test duration
#'
#' @param eventLog Data frame of events returned from \code{\link{createLog}}
#'
#' @return A data frame with time stamps and the number of concurrent
#'   connections for each time stamp
#' @importFrom lubridate %within%
#' @export
getConcurrentOverTest <- function(eventLog) {

  result <- getEventInterval(eventLog, "REQ_HOME",
                             "WS_CLOSE")



  interval <- seq(min(result$start_time), max(result$end_time), by = 5)

  # fix timezones before comparisons
  interval <- lubridate::force_tz(interval, tzone = "UTC")


  cons <- vector(length = length(interval), mode = "numeric")

  for (i in seq_along(interval)) {
    for (j in seq_along(result$interval)) {
      if (interval[i] %within% result$interval[j])
        cons[i] <- cons[i] + 1
    }
  }

  data.frame(time = interval, connections = cons)

}

#' Get page load times
#'
#' @param eventLog Data frame of events returned from \code{\link{createLog}}
#' @return Data frame containing the page load time per connection
#' @export
getPageLoadTimes <- function(eventLog) {
  result <- getEventInterval(eventLog, "REQ_HOME", "WS_OPEN")
  result$load_time_sec <- suppressMessages({as.numeric(result$interval)})
  result$interval <- NULL
  result[, names(result) %in% c("connection", "load_time_sec")]
}

#' Get test duration
#'
#' @param eventLog Data frame of events returned from \code{\link{createLog}}
#' @return Data frame containing the test duration per connection.
#' @export
getTestDurations <- function(eventLog) {
  result <- getEventInterval(eventLog, "REQ_HOME", "WS_CLOSE")
  result$test_duration_sec <- suppressMessages({as.numeric(result$interval)})
  result$interval <- NULL
  result[, names(result) %in% c("connection", "test_duration_sec")]
}


#' Get event duration for user interactions with inputs
#'
#' @param eventLog eventLog Data frame of events returned from \code{\link{createLog}}
#'
#' @return Data frame containing a unique input id and event duration
#' @export
getSetInputTimes <- function(eventLog) {

  ## These come in pairs, so we need to do more work
  events <- list()
  for (i in seq_along(unique(eventLog$connection))) {
    con <- eventLog[eventLog$connection == i,]
    con_events <- con[which(con$event == "WS_RECV" | con$event == "WS_SEND"),]

    # remove the first two that correspond to initial page load
    con_events <- con_events[-(1:2),]

    # add an ID
    con_events$inputId <- cumsum(con_events$event == "WS_SEND")

    # save to list and reset
    events[[i]] <- con_events
    rm(con, con_events)
  }
  result <- do.call(rbind, events)

  ## Pull out the timing info and spread
  start <- result[which(result$event == "WS_SEND"),]
  end <- result[which(result$event == "WS_RECV"),]
  merge_by <- c("inputId", "connection")
  result <- merge(start, end, by.x = merge_by, by.y = merge_by)

  result$interval <- lubridate::interval(start = lubridate::ymd_hms(result$time.x),
                                         end = lubridate::ymd_hms(result$time.y))

  result$event_time_sec <- suppressMessages({as.numeric(result$interval)})

  desired_col <- c("connection", "inputId", "event_time_sec", "interval")

  result[, names(result) %in% desired_col]

}


#' Remove connections during ramp up from log
#'
#' @param eventLog  A data frame with event information as returned from
#'   \code{\link{createLog}}
#' @param rampUpConns Number of connections to remove.
#'
#' @details \code{rampUpConns} connections are removed from the beginning of the
#'   event log. Trimming these connections can help stabalize the results of the
#'   load test.
#'
#' @return Trimmed \code{eventLog}
#' @export
trimRampUp <- function(eventLog, rampUpConns) {
  # check that eventLog is sorted
  eventLog <- eventLog[ with(eventLog, order(connection, time)), ]

  connsToRemove <- unique(eventLog$connection)[1:rampUpConns]
  trimmedLog <- eventLog[which(!(eventLog$connection %in% connsToRemove)),]
  trimmedLog
}
