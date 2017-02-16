#' @importFrom assertthat assert_that
sld_addTimeline <- function(self, private, event){

  ## Validate event
  assert_that(is.data.frame(event))
  e_names <- colnames(event)
  t_names <- c("event", "input", "inputValue", "start" ,"end", "duration", "timedout")

  assert_that( e_names %in% t_names && t_names %in% e_names)

  private$timeline[[length(private$timeline) + 1]] <- event
  invisible(self)
}

sld_initialize <- function(self, private, super, path,
                           loadTimeout, checkNames, debug,
                           phantomTimeout, connectionId) {

  private$connection_id <- connectionId

  time_start <- Sys.time()

  super$initialize(path = path, loadTimeout = loadTimeout,
    checkNames = checkNames, debug = debug, phantomTimeout = phantomTimeout)

  time_end <- Sys.time()

  timing <- data.frame(event      = "pageLoad",
                       input      = NA,
                       inputValue = path,
                       start      = time_start,
                       end        = time_end,
                       duration   = as.numeric(time_end - time_start, units = "secs"),
                       timedout   = FALSE,
                       stringsAsFactors = FALSE)

  private$addTimeline(timing)

  invisible(self)
}

sld_snapshot <- function(self, private) {

  private$snapshotCount <- private$snapshotCount + 1

  current_dir  <- paste0(self$getSnapshotDir(), "-results")

  filename <- sprintf("%03d_%03d.png", private$connection_id,
                private$snapshotCount)

  if (private$snapshotCount == 1) {
    if (dir_exists(current_dir)) {
      unlink(current_dir, recursive = TRUE)
    }
    dir.create(current_dir, recursive = TRUE)
  }

  self$takeScreenshot(file.path(current_dir, filename))

  invisible(self)

}


sld_setInputs = function(self, private, super, ..., wait_, values_,
                         timeout_, timing_) {

  ## for realistic user testing, setInputs should only accept
  ## one input being changed at a time
  inputs = list(...)

  if (length(inputs) > 1)
    stop("Only one input should be changed at a time during a load test")

  timing <- super$setInputs(..., wait_ = wait_, values_ = values_,
              timeout_ = timeout_, timing_ = timing_)$timing

  ## log additional info about input
  timing$input <- names(inputs)[1]
  timing$inputValue <- list(inputs[[1]])

  ## append timing data to the private variable
  private$addTimeline(timing)

  invisible(self)

}

