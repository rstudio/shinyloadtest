context('Log Utilities')

test_that('bad logs fail correctly', {
  expect_error(createLog("fakedir"), "dir_exists(path = logsDir) is not TRUE", fixed = TRUE)
  expect_error(createLog("../sample_logs/profile_no_ws_close"), "Error: Playback file ../sample_logs/profile_no_ws_close/profile_0_0.txt should have 1 instance of `WS_CLOSE`. Perhaps you recorded a bad test?")
  # TODO: Add tests for other bad log types
    # also add tests for parser ...
})

test_that('good profiles create a log with correct dims', {
  log <- createLog("../sample_logs/good_profiles")
  expect_equal(class(log), "data.frame")
  expect_equal(colnames(log), c("eventid", "time", "elapsed_ms", "event", NA ,"process", "connection"))
  expect_equal(nrow(log), 2560)
})

# The following tests just check that we get the expected results for the
# sample log. This is not ideal
#
# TODO: Add actual unit tests!
log <- createLog('../sample_logs/good_profiles')

test_that('getEventInterval returns expected event times', {
    res <- getEventInterval(log, "REQ_HOME", "WS_CLOSE")
    expect_equal(class(res), 'data.frame')
    expect_equal(nrow(res), 80)
    expect_equal(colnames(res), c('connection', 'start_time', 'end_time', 'interval'))
    expect_equal(res$start_time[1],  as.POSIXct("2017-09-11 17:36:31.06599998",tz = "UTC"))
})


test_that('getMaxConcurrent returns expected #', {
    expect_equal(getMaxConcurrent(log), 26)
})

test_that('getConcurrentOverTest returns expected data frame', {
   res <- getConcurrentOverTest(log)
   expect_equal(class(res), 'data.frame')
   expect_equal(nrow(res), 29)
   expect_equal(class(res$time), c('POSIXct', 'POSIXt'))
})

test_that('getPageLoadTimes returns expected page load time', {
  res <- getPageLoadTimes(log)
  expect_equal(class(res), 'data.frame')
  expect_equal(colnames(res), c('connection', 'load_time_sec'))
  expect_equal(nrow(res), 80)
  expect_equal(res$load_time_sec[1], 7)
})

test_that('getTestDuration returns expected test duration', {
  res <- getTestDurations(log)
  expect_equal(class(res), 'data.frame')
  expect_equal(colnames(res), c('connection', 'test_duration_sec'))
  expect_equal(nrow(res), 80)
  expect_equal(res$test_duration_sec[1], 15)
})

test_that('getSetInputTimes returns expected times', {
  res <- getSetInputTimes(log)
  expect_equal(class(res), 'data.frame')
  expect_equal(colnames(res), c('inputId', 'connection', 'interval', 'event_time_sec'))
  expect_equal(res$event_time_sec[1], 0)
})

test_that('trimRampUp returns expected log', {
  res <- trimRampUp(log, 10)
  expect_equal(nrow(res), 2240)
})
