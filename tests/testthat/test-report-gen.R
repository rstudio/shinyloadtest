context("report generation")

test_that("reports are generated", {
  skip_on_cran()
  html_file <- sprintf("%s.html", tempfile("report"))
  shinyloadtest_report(slt_demo_data_1, output = html_file, open_browser = FALSE)
  expect_true(file.exists(html_file))
})
