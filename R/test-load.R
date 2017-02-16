#' Run load tests for a Shiny application
#'
#' @param testFile The file containing a test script to run. Test script can be
#'   generated from \code{recordTest(load_mode = TRUE)}.
#' @param url Web address of the deployed Shiny application.
#' @param numConcurrent Number of concurrent connections to simulate.
#' @param numTotal Total number of connections to simulation.
#' @param stagger  Concurrent connections are staggered by pausing for a random,
#'   uniformally distributed period of time before starting the connection. This
#'   parameter is the maximum amount of time (sec) to wait. A value of 0 means
#'   that no staggering will occur.
#'
#' @details This function simulates load against a deployed Shiny app. The
#'   function creates a cluster of workers using the parallel function
#'   \code{makePSOCKcluster}. The number of works is equal to the number of
#'   desired concurrent connections. Each worker launches a phantomJS process
#'   that calls the URL and drives the app through the test. Tests should be
#'   generated using the \code{recordTest} function with \code{load_mode =
#'   TRUE}. Timing information is aggregated and returned as a data frame. If
#'   the number of total tests > number of concurrent tests, then finished workers
#'    will be recycled to complete remaining tests.The phantomJS process is
#'   reused, but a new browser session is started for each test.
#'
#' @return If all of the child processes complete successfully this function
#'   returns a dataframe containing timing and event information. If not all of
#'   the child processes suceed, a list is returned containing timing for
#'   successful tests and error messages for failed tests.
#'
#' @importFrom foreach %dopar% foreach
#' @importFrom assertthat assert_that
#'
#' @export
loadTest <- function(testFile = "./tests/myloadtest.R",
                     url = NULL,
                     numConcurrent = 4,
                     numTotal = numConcurrent,
                     stagger = 5
                     ) {

  assert_that(file.exists(testFile))

  if (!grepl("^http(s?)://", url))
    stop(paste0("URL ", url," does not appear to be for a deployed Shiny app"))


  ## Validate Inputs
  assert_that(is_count(numConcurrent))
  assert_that(is_count(numTotal))
  if (numTotal == 0 || numConcurrent == 0)
    stop("numTotal and numConcurrent must be >= 1")

  if (numTotal < numConcurrent)
    stop("numTotal must be >= numConcurrent")


  ## Create Workers
  message(paste0('====== Initializing PSOCK Cluster with ',
                 numConcurrent, ' Workers ======'))

  cl <- parallel::makePSOCKcluster(numConcurrent)
  on.exit(parallel::stopCluster(cl))
  doParallel::registerDoParallel(cl)


  ## Loop Through Connections

  message(paste0('====== Beginning Load Test ======'))

  results <- foreach::foreach(i = 1:numTotal,
    .errorhandling = 'pass',
    .packages = 'shinytest') %dopar% {
      withr::with_options(
        list(target.url = url, connection.id = i), {
          env <- new.env(parent = .GlobalEnv)

          Sys.sleep(runif(1, 0, stagger))

          ## The test file will return a data frame
          ## with the relevant timing information.
          source(testFile, local = env)
        })
    }

  ## If there was not an error, we can flatten the results
  ## If there was an error, return the results as a list
  ## that includes the error message AND issue a warning
  tryCatch(
    {
      results <- listToDF(results)
    },
    error = function(e) { results;
      warning("One or more child processes encountered an error.", call. = FALSE)
    })

  results
}
