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

servedBy <- function(appUrl) {

  if (grepl("\\.shinyapps\\.io$", appUrl$host)) return(SERVER_TYPE$SAI)

  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0)
  resp <- curl::curl_fetch_memory(appUrl$build(), handle = h)
  df <- curl::handle_cookies(h)
  headers <- curl::parse_headers_list(resp$headers)

  hasShinyJS <- tryCatch({
    html <- xml2::read_html(rawToChar(resp$content))
    scripts <- xml2::xml_find_all(html, "/html/head/script")
    srcs <- unlist(lapply(scripts, function(script) xml2::xml_attr(script, "src")))
    srcs <- srcs[!is.na(srcs)]
    any(grepl("/shiny.min.js$", srcs))
  }, error = function(e) FALSE)

  if (nrow(df[which(df$name == "SSP-XSRF"),]) == 1
    || isTRUE(headers[["x-powered-by"]] %in% c("Express", "Shiny Server", "Shiny Server Pro"))) {
    return(SERVER_TYPE$SSP)
  } else if (nrow(df[which(df$name == "rscid"),]) == 1) {
    return(SERVER_TYPE$RSC)
  } else if (hasShinyJS) {
    return(SERVER_TYPE$SHN)
  } else {
    stop(paste("Target URL", appUrl$build(), "does not appear to be a Shiny application."))
  }
}
