sld_initialize <- function(self, private, super, path,
                           loadTimeout, checkNames, debug,
                           phantomTimeout, connectionId) {

  ## TODO: Check that path and connectionId are valid

  private$connection_id <- connectionId

  super$initialize(path = path, loadTimeout = loadTimeout,
    checkNames = checkNames, debug = debug, phantomTimeout = phantomTimeout)

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
                         timeout_) {

  ## for realistic user testing, setInputs should only accept
  ## one input being changed at a time
  if (length(list(...)) > 1)
    stop("Only one input should be changed at a time during a load test")

  super$setInputs(..., wait_ = wait_, values_ = values_, timeout_ = timeout_)

  invisible(self)
}
