#' Create load test report from template
#'
#' @param dir Destination directory for the load test template
#' @param name New name for the load test template. Must include the .Rmd extension.
#' @importFrom assertthat assert_that
#' @details Copies the load test template from \code{shinyloadtest} to \code{dir/name}
#' @export
createLoadTestReport <- function(dir = getwd(), name = "load_test_template.Rmd"){

  ## check destinationd directory
  dir <- normalizePath(dir)
  assert_that(assertthat::is.dir(dir) && assertthat::is.writeable(dir))

  ## check file name
  is_valid_rmd_name(name)

  path <- system.file("/loadTestReport/load_test_template.Rmd", package = "shinyloadtest")
  file.copy(path, dir)

  new_name <- paste0(dir, "/", name)
  file.rename(paste0(dir,"/load_test_template.Rmd"), to = new_name)
  new_name

}
