source("R/auth.R")

username <- "barbara"
password <- "nc09brib"
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


+postLogin <- function(hiddenInputs, username, password, url = "http://localhost:3838/sample-apps/hello/__login__") {
  +  httr::POST(url,
    +    config = list(),
    +    body = append(list(
      +      username = username,
      +      password = password
      +    ), hiddenInputs),
    +    encode = "form"
    +  )
  +}

"http://10.211.55.6:3939/__login__"
postLoginRSCHttr <- function(username, password, appUrl, loginUrl) {
  tokens <- getLoginTokens(appUrl)
  params <- append(list(username = username, password = password), tokens$hidden_inputs)
  httr::POST(loginUrl, body = params, encode = "form")
}


protectedBy <- function(appUrl) {
  # Returns string "none", "ssp", "rsc", "shinyapps"
  cookies <- getLoginTokens(appUrl)$cookies
  if (length(cookies$rscid) == 1) return("rsc")
  "none"
}


