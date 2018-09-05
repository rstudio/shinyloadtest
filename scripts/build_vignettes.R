
## to update the vignettes in the
## Must be run to remove warning about "Files in the 'vignettes' directory newer than ll files in ..."
## MUST BE RUN BEFORE SUBMISSION!!!


# build vignettes like normal
devtools::build_vignettes()

# remove all non html files to not bloat the submission
files <- dir(file.path("inst", "doc"), full.names = TRUE, include.dirs = TRUE)
unlink(files[!grepl("\\.html", files)], recursive = TRUE)

gitignore <- file.path("inst", "doc", ".gitignore")
if (file.exists(gitignore)) {
  unlink(gitignore)
}
