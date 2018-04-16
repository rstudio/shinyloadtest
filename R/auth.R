makePair <- function(k, v) {
  pair <- list(v)
  names(pair) <- k
  pair
}

getHiddenInputs <- function(url = "http://localhost:3838/sample-apps/hello/") {
  h <- curl::new_handle()
  resp <- curl::curl_fetch_memory(url, handle = h)
  content <- rawToChar(resp$content)
  login_html <- xml2::read_html(content)
  inputs <- xml2::xml_find_all(login_html, "//input[@type='hidden']")
  attrs <- xml2::xml_attrs(inputs)

  collectNamesValues <- function(rows) {
    Reduce(function(pairs, input) {
      append(pairs, makePair(input[["name"]], input[["value"]]))
    }, rows, init = list())
  }

  list(
    hidden_inputs = collectNamesValues(attrs),
    cookies = collectNamesValues(apply(curl::handle_cookies(h)[,c("name", "value")], 1, as.list))
  )
}

makeParamString <- function(params, sep) {
  kvs <- Map(function (key) paste0(key, "=", params[[key]]), names(params))
  do.call(paste, c(kvs, sep = sep))
}

# TODO factor httr into curl
postLogin <- function(hidden_inputs, cookies, username, password, url = "http://localhost:3838/sample-apps/hello/__login__") {
  params <- append(list(username = username, password = password), hidden_inputs)
  postfields <- URLencode(makeParamString(params, "&"))
  h <- curl::new_handle()
  curl::handle_setopt(h, postfields = postfields, url = url, post = TRUE)
  curl::handle_setheaders(h, accept = "application/json, text/xml, application/xml, */*")
  curl::curl_fetch_memory(url, handle = h)
}

postLoginHttr <- function(hiddenInputs, username, password, url = "http://localhost:3838/sample-apps/hello/__login__") {
  httr::POST(url,
    config = list(),
    body = append(list(
      username = username,
      password = password
    ), hiddenInputs),
    encode = "form"
  )
}

isProtected <- function(appUrl) {

}
