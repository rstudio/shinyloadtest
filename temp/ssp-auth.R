
install.packages("xml2")
library(xml2)

resLoginForm <- httr::GET("http://localhost:3838/sample-apps/hello/")
login <- "/Users/barbaraborges/Desktop/login.html"
login_html <- xml2::read_html(readChar(login, file.info(login)$size))

getHiddenInputs <- function(login_html) {
  inputs <- xml2::xml_find_all(login_html, "//input[@type='hidden']")
  attrs <- xml2::xml_attrs(inputs)
  lst <- Map(function(input) c(input["name"], input["value"]), attrs)
  df <- Reduce(rbind, Map(function(v) data.frame(as.list(v)), lst))
  df$name <- as.character(df$name)
  df$value <- as.character(df$value)
  df
}

sendPostReq <- function() {
httr::POST()
}
