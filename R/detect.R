SERVER_TYPE <- enum(
  RSC,
  SSP,
  SAI,
  SHN
)

format_server_type <- function(field) {
  enum_case(field,
    RSC = "RStudio Server Connect",
    SSP = "Shiny Server or Shiny Server Pro",
    SAI = "shinyapps.io",
    SHN = "R/Shiny"
  )
}

hasShinyJs <- function(body) {
  grepl("/shiny(\\.min)?\\.js['\"]", body)
}

servedBy <- function(appUrl) {
  if (grepl("\\.shinyapps\\.io$", appUrl$host)) {
    return(SERVER_TYPE$SAI)
  }

  h <- curl::new_handle()
  curl::handle_setopt(
    h,
    ssl_verifyhost = FALSE,
    ssl_verifypeer = FALSE,
    followlocation = TRUE
  )
  resp <- curl::curl_fetch_memory(appUrl$build(), handle = h)
  df <- curl::handle_cookies(h)
  headers <- curl::parse_headers_list(resp$headers)

  if (nrow(df[df$name == "SSP-XSRF", ]) == 1 ||
    isTRUE(headers[["x-powered-by"]] %in% c("Express", "Shiny Server", "Shiny Server Pro"))) {
    return(SERVER_TYPE$SSP)
  } else if (nrow(df[which(df$name == "rscid"), ]) == 1) {
    return(SERVER_TYPE$RSC)
  } else if (hasShinyJs(rawToChar(resp$content))) {
    return(SERVER_TYPE$SHN)
  } else {
    cli::cli_abort(paste("Target URL", appUrl$build(), "does not appear to be a Shiny application."))
  }
}
