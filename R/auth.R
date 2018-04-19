
getInputs <- function(html) {
  inputs <- xml2::xml_find_all(html, "//input[@type='hidden']")
  attrs <- xml2::xml_attrs(inputs)
  as.data.frame(do.call(rbind, attrs), stringsAsFactors = FALSE)[,c("name", "value")]
}

# TODO
# postLoginSSP
# postLoginRSC

# Returns a named vector representing a cookie named `name` with value `value`
# that should be attached to all subsequent HTTP requests, including the initial
# websocket request.
postLogin <- function(appUrl, loginUrl) {
  username <- getPass::getPass("Enter your username: ")
  password <- getPass::getPass("Enter your password: ")

  h <- curl::new_handle()

  resp <- curl::curl_fetch_memory(appUrl, handle = h)
  content <- rawToChar(resp$content)
  login_html <- xml2::read_html(content)
  inputs <- getInputs(login_html)
  cookies <- curl::handle_cookies(h)[,c("name", "value")]
  formInputs <- rbind(inputs, data.frame(
    name = c("username", "password"), value = c(username, password))
  )

  curl::handle_setopt(h,
    postfields = URLencode(paste0(formInputs[["name"]], "=", formInputs[["value"]], collapse = "&")),
    # postfields = URLencode(makeParamString(params, "&")),
    # TODO how should the cookies be encoding?
    cookie = paste0(cookies[["name"]], "=", cookies[["value"]], collapse = "; "),
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
