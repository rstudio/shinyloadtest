
# build vignettes like normal
devtools::build_vignettes()

# remove all non html files to not bloat the submission
files <- dir(file.path("inst", "doc"), all.files = TRUE, full.names = TRUE, include.dirs = TRUE)
unlink(files[!grepl("\\.html", files)], recursive = TRUE)
