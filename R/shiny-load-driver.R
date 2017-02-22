#' Class that extends ShinyDriver from \code{shinytes}t package for load testing.
#'
#' @section Usage:
#' app <- ShinyLoadDriver$new(url, connectionId)
#' app$snapshotInit("myloadtest")
#' app$setInputs(...)
#' app$snapshot()
#' app$stop()
#' app$getEventLog()
#'
#' @section Arguments:
#' \describe{
#'   \item{path}{The url of a deployed Shiny app starting
#'    with 'http://' or https://'}
#'   \item{connectionId}{Identifies the current connection,
#'   typically a sequential id}
#'   \item{...}{\code{ShinyLoadDriver} inherits the other
#'   arguments from \code{ShinyDriver}}
#'  }
#'
#' @section Details:
#'
#' \code{app$new()} creates a \code{ShinyLoadDriver} object. It opens
#' a session in phantomJS connected to the Shiny app deployed at \code{path}.
#' Because ShinyLoadDriver connects to a deployed application, not all of the
#' methods from \code{ShinyDriver} are available.
#'
#' \code{app$setInputs()} sets an input value in the Shiny app and
#' records the amount of time before either an output is updated or a timeout
#' occurs. The timeout value, in milliseconds, can be specified using
#' \code{setInputs(..., timeout_ = 3000)}. \code{setInputs()} accepts at most 1
#' input at a time.
#'
#' \code{app$snapshot()} takes a screenshot of the application and saves it to
#' a sub-directory of the current working directory. The subdirectory is named
#' based on \code{app$snapshotInit("directoryName")}. Screenshots are named
#' \code{connectionId_snapshotCount.png} where \code{snapshotCount} is a counter
#' incrementing with each screenshot taken by the \code{ShinyLoadDriver}.
#'
#' \code{app$getEventLog()} returns an event log with information on the test.
#'
#'
#' @importFrom shinytest ShinyDriver
#' @importFrom R6 R6Class
#' @export
ShinyLoadDriver <- R6Class("ShinyLoadDriver",

  inherit = ShinyDriver,

  public = list(

    ## define a new initializer that accepts a
    ## connection id and assigns it to the object
    initialize = function(path = getOption("target.url"),
      loadTimeout = getOption("load.timeout", 5000), checkNames = TRUE,
      debug = c("none", "all", shinytest::ShinyDriver$debugLogTypes),
      phantomTimeout = getOption("phantom.timeout", 10000),
      connectionId = getOption("connection.id", 1))
    {
      sld_initialize(self, private, super, path = path,
        loadTimeout = loadTimeout, checkNames = checkNames,
        debug = debug, phantomTimeout = phantomTimeout,
        connectionId = connectionId
      )
    },

    ## add stubs for non-supported functions

    getAllValues = function(...) {
      stop("Exporting all values is not supported for deployed apps")
    },

    snapshotUpdate = function(...) {
      stop("Snapshots are not supported for deployed apps")
    },

    snapshotCompare = function(...) {
      stop("Snapshots are not supported for deployed apps")
    },

    ## Overload some of the functions

    ## over writes choices for values_
    ## and handles timing info
    setInputs = function(..., wait_ = TRUE, values_ = FALSE, timeout_ = 3000,
      timing_ = TRUE)
    {
      sld_setInputs(self, private, super, ..., wait_ = TRUE, values_ = FALSE,
        timeout_ = timeout_)
    },

    ## takes a screenshot instead of a true snapshot
    snapshot = function(){
      sld_snapshot(self, private)
    },

    ## adds the connection id
    getEventLog = function(){
      log <- super$getEventLog()
      log$connection <- private$connection_id
      log$process <- Sys.getpid()
      log
    },

    ## new functions

    ## returns a directory with the same name
    ## as the test file in the current working dir
    getSnapshotDir = function(){
      private$snapshotDir
    }

  ),

  private = list(
    connection_id = NULL          # connection id used for screenshot filename
  )

)

