makePair <- function(k, v) {
  pair <- list(v)
  names(pair) <- k
  pair
}

collectNamesValues <- function(rows) {
  Reduce(function(pairs, input) {
    append(pairs, makePair(input[["name"]], input[["value"]]))
  }, rows, init = list())
}

getLoginTokens <- function(url) {
  h <- curl::new_handle()
  resp <- curl::curl_fetch_memory(url, handle = h)
  content <- rawToChar(resp$content)
  login_html <- xml2::read_html(content)
  inputs <- xml2::xml_find_all(login_html, "//input[@type='hidden']")
  attrs <- xml2::xml_attrs(inputs)
  list(
    hidden_inputs = collectNamesValues(attrs),
    cookies = collectNamesValues(apply(curl::handle_cookies(h)[,c("name", "value")], 1, as.list))
  )
}

makeParamString <- function(params, sep) {
  kvs <- Map(function (key) paste0(key, "=", params[[key]]), names(params))
  do.call(paste, c(kvs, sep = sep))
}

postLogin <- function(username, password, appUrl, loginUrl) {
  tokens <- getLoginTokens(appUrl)
  params <- append(list(username = username, password = password), tokens$hidden_inputs)
  h <- curl::new_handle()
  curl::handle_setopt(h,
    postfields = URLencode(makeParamString(params, "&")),
    cookie = makeParamString(tokens$cookies, "; "),
    url = loginUrl,
    post = TRUE,
    followlocation = FALSE
  )
  curl::curl_fetch_memory(loginUrl, handle = h)
  df <- curl::handle_cookies(h)
  df[which(df$name == "session_state"), "value"]
}

# TODO
protectedBy <- function(appUrl) {
  # Returns string "none", "ssp", "rsc", "shinyapps"
}
