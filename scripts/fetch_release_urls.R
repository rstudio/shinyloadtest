#!/usr/bin/Rscript

library(httr)
library(jsonlite)
library(readr)

release_version <- function(release) {
  sub(".", "", release$name)
}

release_sha <- function(release) {
  tag <- release$tag_name
  tags <- jsonlite::parse_json(httr::GET('https://api.github.com/repos/rstudio/shinycannon/tags'))
  sha <- Find(function(x) x$name == tag, tags)$commit$sha
  gsub("(.{7}).*", "\\1", sha)
}

asset_platform <- function(asset) {
  if (grepl("\\.deb$", asset$name)) {
    "deb"
  } else if (grepl("-suse-", asset$name)) {
    "rpm_suse"
  } else if (grepl("\\.rpm$", asset$name)) {
    "rpm_rh"
  } else {
    tools::file_ext(asset$name)
  }
}

asset_df <- function(release) {
  version <- release_version(release)
  sha <- release_sha(release)
  do.call(rbind, lapply(latest$assets, function(asset) {
    data.frame(
      version = version,
      sha = sha,
      platform = asset_platform(asset),
      file = asset$name,
      url = asset$browser_download_url
    )
  }))
}

url <- 'https://api.github.com/repos/rstudio/shinycannon/releases'
json <- jsonlite::parse_json(httr::GET(url))
latest <- json[[1]]
df <- asset_df(latest)

cat(readr::format_csv(df))
