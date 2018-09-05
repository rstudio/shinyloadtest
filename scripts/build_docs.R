

## to update the package down website in the ./docs folder

unlink("./docs", recursive = TRUE)
pkgdown::build_site()
