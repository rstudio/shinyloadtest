
install.packages("xml2")
library(xml2)

getHiddenInputs <- function(login_html) {
  inputs <- xml_find_all(login_html, "//input[@type='hidden']")
  attrs <- xml_attrs(inputs)
  Map(function(input) c(input["name"], input["value"]), attrs)
}


login <- "/Users/barbaraborges/Desktop/login.html"
login_html <- readChar(login, file.info(login)$size) %>% read_html()

login_html %>%
  xml_find_all("//input[@type='hidden']")

attrs <- login_xml %>%
  xml_attrs() %>%
  Map(function(x) c(x["name"], x["value"]), .)

getLoginForm <- function(url = "http://localhost:3838/sample-apps/hello/") {
  res <- httr::GET(url)
}
