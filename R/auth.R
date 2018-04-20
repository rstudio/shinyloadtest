
getInputs <- function(html, server) {
  if (server == "ssp") {
    inputs <- xml2::xml_find_all(html, "//input[@type='hidden']")
    attrs <- xml2::xml_attrs(inputs)
    as.data.frame(do.call(rbind, attrs), stringsAsFactors = FALSE)[,c("name", "value")]
  }
}

# Returns string "none", "ssp", "rsc", "shinyapps"
protectedBy <- function(appUrl) {
  h <- curl::new_handle()
  resp <- curl::curl_fetch_memory(appUrl, handle = h)
  df <- curl::handle_cookies(h)
  if (resp$status_code == 403) {
    if (nrow(df[which(df$name == "SSP-XSRF"),]) == 1) return("ssp")
  }
  else if (nrow(df[which(df$name == "rscid"),]) == 1) return("rsc")
  "none"
}

# Returns the cookie to be persisted in all future HTTP requests in the session
handlePost <- function(handle, loginUrl, postfields, cookies, cookieName) {
  curl::handle_setopt(handle,
    postfields = postfields,
    # TODO how should the cookies be encoding?
    cookie = paste0(cookies[["name"]], "=", cookies[["value"]], collapse = "; "),
    post = TRUE,
    followlocation = FALSE,
    ssl_verifyhost = 0, ssl_verifypeer = 0
  )
  curl::curl_fetch_memory(loginUrl, handle = handle)
  cookies <- curl::handle_cookies(handle)
  unlist(cookies[which(cookies$name == cookieName), c("name", "value")])
}

# Returns a named vector representing a cookie named `name` with value `value`
# that should be attached to all subsequent HTTP requests, including the initial
# websocket request.
# Currently implemented for RSC and SSP
postLogin <- function(appUrl, loginUrl = NULL) {
  username <- getPass::getPass("Enter your username: ")
  password <- getPass::getPass("Enter your password: ")

  appServer <- protectedBy(appUrl)
  if (appServer == "none") stop("No server detected")

  if (is.null(loginUrl)) {
    if (appServer == "rsc") {
      p <- xml2::url_parse(appUrl)
      baseUrl <- paste0(p[1,"scheme"], "://", p[1,"server"], ":", p[1,"port"], "/", collapse = "")
      loginUrl <- paste0(baseUrl, "__login__")
    } else if (appServer == "ssp") {
      loginUrl <- paste0(appUrl, "__login__")
    }
  }

  h <- curl::new_handle()
  resp <- curl::curl_fetch_memory(appUrl, handle = h)
  login_html <- xml2::read_html(rawToChar(resp$content))
  inputs <- rbind(getInputs(login_html, appServer), data.frame(
    name = c("username", "password"), value = c(username, password))
  )
  cookies <- curl::handle_cookies(h)[,c("name", "value")]
  cookie <- if (appServer == "rsc") {
    handlePost(handle = curl::new_handle(), loginUrl = loginUrl,
      postfields = paste0('{"username": "', username, '", "password": "', password, '"}'),
      cookies = cookies, cookieName = "rsconnect"
    )
  } else if (appServer == "ssp") {
    handlePost(handle = h, loginUrl = loginUrl,
      postfields = URLencode(paste0(inputs[["name"]], "=", inputs[["value"]], collapse = "&")),
      cookies = cookies, cookieName = "session_state"
    )
  }
  return(cookie)
}

getApp <- function(appUrl, cookie) {
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0,
    cookie = paste0(cookie[["name"]], "=", cookie[["value"]], collapse = "; ")
  )
  curl::curl_fetch_memory(appUrl, handle = h)
}

# --- EXAMPLE USAGE ---
# --- 1. RSC
# ---   rsc_hosted_app_url <- "http://10.211.55.6:3939/content/3/"
# ---   cookie <- postLogin(rsc_hosted_app_url)
# ---   app <- getApp(rsc_hosted_app_url, cookie)
# ---
# ---   # checking it's the right body (there should be `shiny-input` stuff, etc)
# ---   rawToChar(app[["content"]])
# ---
# --- 2. SSP
# ---   ssp_hosted_app_url <- "http://localhost:3838/sample-apps/hello/"
# ---   cookie <- postLogin(ssp_hosted_app_url)
# ---   app <- getApp(ssp_hosted_app_url, cookie)
# ---
# ---   # checking it's the right body
# ---   rawToChar(app[["content"]])
