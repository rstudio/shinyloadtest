library(shinyloadtest)
library(usethis)
library(stringr)

# Loads demo session .zip files from vignettes/test_sessions and assigns them to
# the variables slt_demo_data_{1,4,16} in the calling environment.
load_demo_data <- function() {
  demo_pat  <- "demo(\\d+)\\.zip"
  zip_files <- list.files("vignettes/test_sessions", pattern = demo_pat, full.names = TRUE)
  for (zip_file in zip_files) {
    n      <- unlist(str_match_all(zip_file, demo_pat))[[2]]
    demo_n <- paste0("demo", n)
    tmpd   <- tempdir()
    unzip(zip_file, exdir = tmpd)
    df <- load_runs(file.path(tmpd, demo_n))
    # We remove the 'json' column here to save space in .rda
    df[["json"]] <- NULL
    assign(paste0("slt_demo_data_", n), df, envir = parent.env(environment()))
  }
}

load_demo_data()

use_data(slt_demo_data_1, overwrite = TRUE, version = 2)
use_data(slt_demo_data_4, overwrite = TRUE, version = 2)
use_data(slt_demo_data_16, overwrite = TRUE, version = 2)
