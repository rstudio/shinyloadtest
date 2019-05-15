
## to update the vignettes in the
## Must be run to remove warning about "Files in the 'vignettes' directory newer than ll files in ..."
## MUST BE RUN BEFORE SUBMISSION!!!


# build vignettes like normal
devtools::build_vignettes()

# We do this manual vignette management because "we are the 1%": we pre-generate
# our vignettes and keep them in source control. We do this because generating
# our vignettes requires a lot of data (shinycannon output) that we don't
# want to include in the built package.
# This change to devtools::build_vignettes() motivated a more hands-on approach:
# https://github.com/r-lib/devtools/commit/57b6958f7992e47274b49814b616367b4a2dd836
file.rename("doc", "inst/doc")
file.remove("Meta/vignette.rds")

# remove all non html files to not bloat the submission
files <- dir(file.path("inst", "doc"), full.names = TRUE, include.dirs = TRUE)
unlink(files[!grepl("\\.html", files)], recursive = TRUE)

gitignore <- file.path("inst", "doc", ".gitignore")
if (file.exists(gitignore)) {
  unlink(gitignore)
}
