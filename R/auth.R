
getInputs <- function(html, server) {
  if (server == SERVER_TYPE$SSP) {
    inputs <- xml2::xml_find_all(html, "//input[@type='hidden']")
    attrs <- xml2::xml_attrs(inputs)
    attrs <- lapply(attrs, function(vec) c(name = vec[["name"]], value = vec[["value"]]))
    as.data.frame(do.call(rbind, attrs), stringsAsFactors = FALSE)
  } else data.frame()
}

pasteParams <- function(df, collapse) {
  if (nrow(df) == 0) {
    ""
  } else {
    paste0(df$name, "=", df$value, collapse = collapse)
  }
}

isProtected <- function(appUrl) {
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0)
  resp <- curl::curl_fetch_memory(appUrl$build(), handle = h)
  # NOTE: Connect returns a 404 if the app exists but requires authentication.
  # So we don't have a way to distinguish between an appUrl that doesn't exist
  # and an app that's protected.
  # SSP returns a 403.
  resp$status_code %in% c(403, 404)
}

loginUrlFor <- function(appUrl, appServer) {
  if (appServer %in% c(SERVER_TYPE$RSC, SERVER_TYPE$SSP)) {
    appUrl$appendPath("__login__")
  } else {
    stop(paste0("Unknown appServer:", appServer))
  }
}

# Returns the cookies to be persisted in all future HTTP requests in the session
handlePost <- function(handle, loginUrl, postfields, cookies, cookieName) {
  curl::handle_setopt(handle,
    postfields = postfields,
    cookie = pasteParams(cookies, "; "),
    post = TRUE,
    followlocation = FALSE,
    ssl_verifyhost = 0,
    ssl_verifypeer = 0
  )

  resp <- curl::curl_fetch_memory(loginUrl$build(), handle = handle)
  if (!(resp$status_code %in% c(200, 302))) stop("Authentication failed")

  curl::handle_cookies(handle)[,c("name", "value")]
}

# Returns the cookies that should be attached to all subsequent HTTP requests,
# including the initial websocket request. Currently implemented for RSC and
# SSP.
postLogin <- function(appUrl, appServer, username, password) {

  loginUrl <- loginUrlFor(appUrl, appServer)

  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0)
  resp <- curl::curl_fetch_memory(appUrl$build(), handle = h)
  login_html <- xml2::read_html(rawToChar(resp$content))
  inputs <- rbind(getInputs(login_html, appServer), data.frame(
    name = c("username", "password"), value = c(username, password))
  )
  cookies <- curl::handle_cookies(h)[,c("name", "value")]

  enum_case(appServer,
    RSC = handlePost(
            handle = curl::new_handle(),
            loginUrl = loginUrl,
            postfields = jsonlite::toJSON(
              list(
                username = username,
                password = password
              ),
              auto_unbox = TRUE
            ),
            cookies = cookies,
            cookieName = "rsconnect"
          ),
    SSP = handlePost(
            handle = curl::new_handle(),
            loginUrl = loginUrl,
            postfields = utils::URLencode(pasteParams(inputs, "&")),
            cookies = cookies,
            cookieName = "session_state"
          ),
    SAI = stop("Logging in to shinyapps.io is unsupported"),
    SHN = stop("Plain Shiny apps don't support authentication")
  )
}

getApp <- function(appUrl, cookie) {
  h <- curl::new_handle()
  curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0,
    cookie = pasteParams(cookie, "; ")
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
