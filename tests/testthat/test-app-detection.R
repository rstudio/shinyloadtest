context("test Shiny app type detection")

slurp <- function(file) {
  readChar(file, file.info(file)$size, useBytes = TRUE)
}

test_that("Shiny example app detected", {
  testthat::expect_true(hasShinyJs(slurp("01_hello.html")))
})

test_that("Non-Shiny app not considered a Shiny app", {
  testthat::expect_false(hasShinyJs(slurp("r_project.html")))
})

