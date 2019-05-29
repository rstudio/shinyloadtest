library(shinyloadtest)
library(magrittr)

open_browser <- if (length(Sys.getenv("HEADLESS")) > 0) FALSE else TRUE

(function() {
  withr::with_dir("vignettes/test_sessions", {

    zips <- dir(".", pattern = ".zip")

    folders_exist <- zips %>%
      sub("\\.zip$", "", .) %>%
      dir.exists()

    # unzip all missing folders
    if (any(!folders_exist)) {
      lapply(zips[!folders_exist], unzip)
    }
  })



  # compile all documents
  load_runs(
    "1 user" = "vignettes/test_sessions/demo1",
    "4 users" = "vignettes/test_sessions/demo4",
    "16 users" = "vignettes/test_sessions/demo16"
  ) %>%
    shinyloadtest_report("output/demo-local.html", open_browser = open_browser, self_contained = FALSE)

  load_runs("sliders" = "vignettes/test_sessions/sliders") %>%
    shinyloadtest_report("output/sliders.html", open_browser = open_browser)

  load_runs("upload-dt" = "vignettes/test_sessions/upload-dt") %>%
    shinyloadtest_report("output/upload-dt.html", open_browser = open_browser)

  widgets <- load_runs("widgets" = "vignettes/test_sessions/widgets")
  widgets %>%
    shinyloadtest_report("output/widgets.html", open_browser = open_browser)
  widgets %>%
    shinyloadtest_report("output/widgets-local.html", self_contained = FALSE, open_browser = open_browser)

  # First report mentioned in scaling case study
  load_runs(
    "Run 1" = "vignettes/test_sessions/scaling_case_study_run1/",
    "Run 5" = "vignettes/test_sessions/scaling_case_study_run5/"
  ) %>%
    shinyloadtest_report("vignettes/case_studies/case_study_report1.html", open_browser = open_browser)

  # Second report mentioned in scaling case study
  load_runs(
    "Run 5" = "vignettes/test_sessions/scaling_case_study_run5/",
    "Run 5 new" = "vignettes/test_sessions/scaling_case_study_run5_new_settings/"
  ) %>%
    shinyloadtest_report("vignettes/case_studies/case_study_report2.html", open_browser = open_browser)

})()
