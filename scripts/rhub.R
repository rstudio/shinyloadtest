

email <- NULL
# email <- "barret@rstudio.com"

check_output <- devtools::check_rhub(email = email, interactive = FALSE)

# check_output$web()
for (i in seq_len(nrow(check_output$urls()))) {
  check_output$livelog(i)
}

check_output$cran_summary()
