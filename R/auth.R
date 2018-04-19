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
  # Maybe this should be a dataframe with a 'type' column of 'cookie'/'input'
  list(
    # TODO These should both be dataframe values?
    hidden_inputs = collectNamesValues(attrs),
    cookies = collectNamesValues(apply(curl::handle_cookies(h)[,c("name", "value")], 1, as.list))
  )
}

makeParamString <- function(params, sep) {
  kvs <- Map(function (key) paste0(key, "=", params[[key]]), names(params))
  do.call(paste, c(kvs, sep = sep))
}

makeParamString2 <- function(name, value, sep) {
  paste0(name, "=", value, collapse = sep)
}

# TODO
# postLoginSSP
# postLoginRSC

# Returns a named vector representing a cookie named `name` with value `value`
# that should be attached to all subsequent HTTP requests, including the initial
# websocket request.
postLogin <- function(username, password, appUrl, loginUrl) {
  tokens <- getLoginTokens(appUrl)
  params <- append(list(username = username, password = password), tokens$hidden_inputs)
  h <- curl::new_handle()
  curl::handle_setopt(h,
    postfields = URLencode(makeParamString(params, "&")),
    # TODO how should the cookies be encoding?
    cookie = makeParamString(tokens$cookies, "; "),
    url = loginUrl,
    post = TRUE,
    followlocation = FALSE
  )
  curl::curl_fetch_memory(loginUrl, handle = h)
  df <- curl::handle_cookies(h)
  unlist(df[which(df$name == "session_state"), c("name", "value")])
}

protectedBy <- function(appUrl) {
  # Returns string "none", "ssp", "rsc", "shinyapps"
  h <- curl::new_handle()
  resp <- curl::curl_fetch_memory(appUrl, handle = h)
  df <- curl::handle_cookies(h)
  if (resp$status_code == 403) {
    if (nrow(df[which(df$name == "SSP-XSRF"),]) == 1) return("ssp")
  }
  "none"
}
