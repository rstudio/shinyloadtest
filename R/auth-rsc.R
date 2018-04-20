source("R/auth.R")

username <- "barbara"
password <- "password"
appUrl <- "http://10.211.55.6:3939/content/3/"
loginUrl <- "http://10.211.55.6:3939/__login__"

# "http://10.211.55.6:3939/__api__/me"
#   <- 401 means unauthorized
#   <- 200 means authenticated

# many cookies and xsrf stuffies

# Returns a named vector representing a cookie named `name` with value `value`
# that should be attached to all subsequent HTTP requests, including the initial
# websocket request.
#
# e.g.
# postLoginRSC("barbara", "nc09brib",
#   "http://10.211.55.6:3939/content/3/",
#   "http://10.211.55.6:3939/connect/#/login")
postLoginRSC <- function(username, password, appUrl, loginUrl) {
  tokens <- getLoginTokens(appUrl)
  params <- append(list(username = username, password = password), tokens$hidden_inputs)
  h <- curl::new_handle()
  curl::handle_setopt(h,
    postfields = URLencode(makeParamString(params, "&")),
    cookie = makeParamString(tokens$cookies, "; "),
    url = "http://10.211.55.6:3939",
    post = TRUE
  )
  res <- curl::curl_fetch_memory("http://10.211.55.6:3939", handle = h)
  # df <- curl::handle_cookies(h)
  # unlist(df[which(df$name == "session_state"), c("name", "value")])
  # res
  # apime <- curl::curl_fetch_memory(appUrl, handle = h)
}

t <- postLoginRSCHttr(
"barbara",
"password",
  appUrl = "http://10.211.55.6:3939/connect/#/login?url=http:%2F%2F10.211.55.6:3939%2Fcontent%2F3%2F",
  loginUrl = "http://10.211.55.6:3939/connect/#/login?url=http:%2F%2F10.211.55.6:3939%2Fcontent%2F3%2F"
)

# 1. obtain rscid, rsconnect, and RSC-XSRF cookies by visiting app url
# 2. post to http://10.211.55.6:3939/__login__ with 3 cookies and an additional
#    header (X-RSC-XSRF: aISSTqRLUZ88S20dMzwis6dqNBNu53jc3nIXASJx2UY=, same as cookie)
#    post body: request payload: {username: "barbara", password: "nc09brib"}
# 3. use the rsconnect cookie in all future HTTP requests
#
#
# 1. send post with body - request payload: {username: "barbara", password: "nc09brib"}
# 2. use the rsconnect cookie in all future HTTP requests
postLoginRSCHttr <- function(username = "barbara", password = "nc09brib", appUrl, loginUrl) {
  tokens <- getLoginTokens(appUrl)
  httr::POST(loginUrl, body = list(username = username, password = password), encode = "json")
}


postLoginRsc <- function(appUrl, username = "barbara", password = "nc09brib") {
  h1 <- curl::new_handle()
  resp <- curl::curl_fetch_memory(appUrl, handle = h1)
  cookies <- curl::handle_cookies(h1)[,c("name", "value")]
  h2 <- curl::new_handle()
  curl::handle_setopt(h2,
    postfields = '{"username": "barbara", "password": "nc09brib"}',
    cookie = paste0(cookies[["name"]], "=", cookies[["value"]], collapse = "; "),
    post = TRUE, followlocation = FALSE
  )
  curl::curl_fetch_memory("http://10.211.55.6:3939/__login__", handle = h2)
  curl::handle_cookies(h2)[,c("name", "value")]
}


getApp <- function(appUrl, cookies) {
  h <- curl::new_handle()
  curl::handle_setopt(h,
    cookie = paste0(cookies[["name"]], "=", cookies[["value"]], collapse = "; ")
  )
  curl::curl_fetch_memory(appUrl, handle = h)
}


protectedBy <- function(appUrl) {
  # Returns string "none", "ssp", "rsc", "shinyapps"
  cookies <- getLoginTokens(appUrl)$cookies
  if (length(cookies$rscid) == 1) return("rsc")
  "none"
}


