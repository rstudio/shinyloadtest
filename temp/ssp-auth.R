
install.packages("xml2")
library(xml2)

getHiddenInputs <- function(url = "http://localhost:3838/sample-apps/hello/") {
  login <- httr::GET(url)
  login_html <- xml2::read_html(login)
  inputs <- xml2::xml_find_all(login_html, "//input[@type='hidden']")
  attrs <- xml2::xml_attrs(inputs)
  lst <- Map(function(input) c(input["name"], input["value"]), attrs)
  df <- Reduce(rbind, Map(function(v) data.frame(as.list(v)), lst))
  df$name <- as.character(df$name)
  df$value <- as.character(df$value)
  df
}

sendPostReq <- function(url = "http://localhost:3838/sample-apps/hello/") {
  req_url <- paste0(url, "__login__")
  httr::POST(req_url, config = list(
    add_headers(Accept = ""),
    set_cookies(a = 1, b = 2)
  ))
}
