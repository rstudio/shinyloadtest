test_that("reports are generated", {
  skip_on_cran()
  html_file <- sprintf("%s.html", tempfile("report"))
  shinyloadtest_report(
    slt_demo_data_1,
    output = html_file,
    open_browser = FALSE
  )
  expect_true(file.exists(html_file))
})

test_that("legend extraction works", {
  df <- data.frame(x = 1:3, y = 1:3, c = LETTERS[1:3], stringsAsFactors = FALSE)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, color = c)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(fill = "", color = "")

  testthat::expect_no_error(
    extract_legend(p)
  )
})
