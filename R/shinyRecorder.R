req_rook_to_curl <- function(req, domain, port) {
  # Rename headers. Example: HTTP_CACHE_CONTROL => Cache-Control
  r <- as.list(req)

  # Log request headers
  logging::logdebug("== Original ==\n")
  logging::logdebug(capture.output(print(str(r))), sep = "\n")

  r <- r[grepl("^HTTP_", names(r))]
  nms <- names(r)
  nms <- sub("^HTTP_", "", nms)
  nms <- tolower(nms)
  nms <- gsub("_", "-", nms, fixed = TRUE)
  nms <- gsub("\\b([a-z])", "\\U\\1", nms, perl = TRUE)
  names(r) <- nms

  # Overwrite host field
  if (port == 80) {
    r$Host <- domain
  } else {
    r$Host <- paste0(domain, ":", port)
  }

  # Log modified request headers
  logging::logdebug("== Modified ==\n")
  logging::logdebug(capture.output(print(str(r))), sep = "\n")
  r
}

resp_httr_to_rook <- function(resp) {
  status <- as.integer(sub("^HTTP\\S+ (\\d+).*", "\\1", curl::parse_headers(resp$headers)[1]))
  headers <- curl::parse_headers_list(resp$headers)
  headers[["transfer-encoding"]] <- NULL
  headers[["content-encoding"]] <- "identity"
  list(
    status = status,
    headers = headers,
    body = resp$content
  )
}

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

# TODO
# Intercept, parse, and record regular HTTP traffic:
# REQ, REQ_HOME, REQ_TOK, REQ_SINF
# Intercept and parse WS traffic:
# WS_OPEN, WS_CLOSE, WS_SEND, WS_RECV, WS_RECV_INIT

makeTimestamp <- function(time = Sys.time()) {
  withr::with_options(
    list(digits.secs = 3),
    format(time, "%Y-%m-%dT%H:%M:%OSZ", tz = "GMT")
  )
}

# Returns NA if workerid not found. This either indicates an error state of some
# kind, or more likely, the Shiny session is running locally.
getWorkerId <- function(page) {
  pat <- ".*<base href=\"_w_([0-9a-z]+)/.*"
  stringr::str_match(page, pat)[[2]]
}

messagePattern <- '^(a\\[")([0-9A-F*]+#)?(0\\|m\\|)(.*)("\\])$'

# Parses a JSON message from the server; returns the object from the nested JSON, if any
parseMessage <- function(msg) {
  res <- stringr::str_match(msg, messagePattern)
  encodedMsg <- res[1,5]
  # If the regex failed, then msg is probably a bare JSON string that can be
  # decoded directly.
  if (is.na(encodedMsg)) {
    jsonlite::fromJSON(msg)
  # If the regex succeeded, we have the payload as an almost-double-JSON-encoded
  # object - it just needs to be wrapped in a set of double-quotes.
  } else {
    wrappedMsg <- paste0('"', encodedMsg, '"')
    jsonlite::fromJSON(jsonlite::fromJSON(wrappedMsg))
  }
}

# Generate a new message based on originalMessage but with new contents
# {"type":"WS_RECV_INIT","created":"2018-03-12T19:51:59.675Z","message":"a[\"1#0|m|{\\\\\"config\\\\\":{\\\\\"workerId\\\\\":[\\\\\"${WORKER}\\\\\"],\\\\\"sessionId\\\\\":[\\\\\"${SESSION}\\\\\"],\\\\\"user\\\\\":null}}\"]"}
# {"type":"WS_RECV_INIT","created":"2018-03-12T20:21:27.086Z","message":"a[\"1#0|m|[\"{\\\"config\\\":{\\\"workerId\\\":[\\\"${WORKER}\\\"],\\\"sessionId\\\":[\\\"${SESSION}\\\"],\\\"user\\\":null}}\"]\"]"}
spliceMessage <- function(originalMessage, newMessageObject) {
  newMsg <- jsonlite::toJSON(jsonlite::unbox(jsonlite::toJSON(newMessageObject, null = 'null')))
  group <- stringr::str_match(originalMessage, messagePattern)
  paste0(group[1,2], group[1,3], group[1,4], newMsg, group[1,6])
}

makeHTTPEvent <- function(server, req, resp_curl, created = Sys.time()) {
  if (req$REQUEST_METHOD != "GET") stop("Unsupported method, only handle GET:", req$REQUEST_METHOD)

  # ShinyHomeRequestEvent,
  if (grepl("(\\/|\\.rmd)($|\\?)", req$PATH_INFO, ignore.case = TRUE)) {
    page <- rawToChar(resp_curl$content)
    workerId <- getWorkerId(page)
    structure(list(
      type = "REQ_HOME",
      created = makeTimestamp(created),
      method = "GET",
      server = if (is.na(workerId)) "local" else "hosted",
      url = req$PATH_INFO,
      statusCode = 200
    ), class = "REQ")

  # ShinyTokenRequestEvent,
  } else if (grepl("__token__", req$PATH_INFO, fixed = TRUE)) {
    structure(list(
      type = "REQ_TOK",
      created = makeTimestamp(created),
      method = "GET",
      server = server,
      url = gsub("_w_[a-z0-9]+", "_w_${WORKER}", req$PATH_INFO),
      statusCode = 200
    ), class = "REQ")

  # ShinySINFRequestEvent
  } else if (grepl("__sockjs__/", req$PATH_INFO, fixed = TRUE)) {
    structure(list(
      type = "REQ_SINF",
      created = makeTimestamp(created),
      method = "GET",
      server = server,
      url = stringr::str_replace_all(req$PATH_INFO, c(
        "n=\\w+" = "n=${ROBUST_ID}",
        "t=\\w+" = "t=${TOKEN}",
        "w=\\w+" = "w=${WORKER}")),
      statusCode = 200
    ), class = "REQ")

  # ShinyRequestEvent
  } else {
    if (server == "hosted") print(req$PATH_INFO)
    structure(list(
      type = "REQ",
      created = makeTimestamp(created),
      method = "GET",
      server = server,
      url = if (server == "local") req$PATH_INFO else gsub("_w_\\w+", "_w_${WORKER}", req$PATH_INFO),
      statusCode = 200
    ), class = "REQ")
  }
}

makeWSEvent <- function(type, created = Sys.time(), ...) {
  structure(list(type = type, created = makeTimestamp(created), ...), class = "WS")
}

format.REQ = function(httpEvt) {
  jsonlite::toJSON(unclass(httpEvt), auto_unbox = TRUE)
}

format.WS = function(wsEvt) {
  jsonlite::toJSON(unclass(wsEvt), auto_unbox = TRUE)
}

trimslash <- function(urlPath, which = c("both", "left", "right")) {
  if (which %in% c("both", "left") && substr(urlPath, 1, 1) == "/") {
    urlPath <- substr(urlPath, 2, nchar(urlPath))
  }
  if (which %in% c("both", "right") && substr(urlPath, nchar(urlPath), nchar(urlPath)) == "/") {
    urlPath <- substr(urlPath, 1, nchar(urlPath)-1)
  }
  urlPath
}

shouldIgnore <- function(msgFromServer) {
  canIgnore <- c('^a\\["ACK.*$', '^\\["ACK.*$', '^h$')
  if (length(unlist(stringr::str_match_all(msgFromServer, canIgnore))) > 0) return(TRUE)
  parsed <- parseMessage(msgFromServer)
  if (length(intersect(names(parsed), c("busy", "progress", "recalculating"))) > 0) return(TRUE)
  if (identical(names(parsed), c("custom"))) {
    customKeys <- names(parsed[["custom"]])
    if (isTRUE(customKeys == "reactlog")) return(TRUE)
  }
  noop <- list(errors = list(), values = list(), inputMessages = list())
  if (identical(parsed, noop)) return(TRUE)
  return(FALSE)
}

RecordingSession <- R6::R6Class("RecordingSession",
  public = list(
    initialize = function(targetAppUrl, host, port, outputFileName, sessionCookie) {
      parsedUrl <- urltools::url_parse(targetAppUrl)
      private$targetScheme <- parsedUrl$scheme
      private$targetHost <- parsedUrl$domain
      private$targetPort <- parsedUrl$port %OR% 80
      private$targetPath <- if (is.na(parsedUrl$path)) "" else parsedUrl$path

      private$localHost <- host
      private$localPort <- port
      private$outputFile <- file(outputFileName, "w")
      private$sessionCookie <- sessionCookie

      private$startServer()
    },
    stop = function() {
      if (!(is.null(private$localServer))) {
        cat("Stopping server\n")
        httpuv::stopServer(private$localServer)
        httpuv::interrupt()
        private$localServer <- NULL
        close(private$outputFile)
      }
    }
  ),
  private = list(
    targetScheme = NULL,
    targetHost = NULL,
    targetPort = NULL,
    targetPath = NULL,
    localHost = NULL,
    localPort = NULL,
    localServer = NULL,
    outputFile = NULL,
    server = NULL,
    sessionCookie = NULL,
    clientWsState = NULL,
    writeEvent = function(evt) {
      writeLines(format(evt), private$outputFile)
      flush(private$outputFile)
    },
    makeUrl = function(req) {
      httpUrl <- paste0(private$targetScheme, "://", private$targetHost, ":", private$targetPort, "/", private$targetPath, "/", req$PATH_INFO, req$QUERY_STRING)
      # This is a hack around the fact that somehow there's three forward slashes in one of the separators
      httpUrl <- gsub("///", "/", httpUrl, fixed = TRUE)
      httpUrl
    },
    makeCurlHandle = function(req) {
      req_curl <- req_rook_to_curl(req, private$targetHost, private$targetPort)
      h <- curl::new_handle()

      if (!is.null(private$sessionCookie)) {
        req_curl[["Cookie"]] <- pasteParams(private$sessionCookie, "; ")
      }

      do.call(curl::handle_setheaders, c(h, req_curl))

      # TODO Accept invalid certificate from upstream. Should we make this an option?
      if (private$targetScheme == "https") {
        curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0)
      }
      h
    },
    handlePOST = function(req) {
      if (req$HTTP_CONTENT_TYPE == "application/octet-stream") {
        h <- private$makeCurlHandle(req)
        url <- private$makeUrl(req)

        len <- as.integer(req$HTTP_CONTENT_LENGTH)
        data <- req$rook.input$read(len)

        curl::handle_setopt(h, postfieldsize = len, postfields = data, post = TRUE)

        resp_curl <- curl::curl_fetch_memory(url, handle = h)
        event <- makeHTTPEvent(private$server, req, resp_curl)
        private$server <- event$server
        private$writeEvent(event)
        resp_httr_to_rook(resp_curl)
      } else {
        stop("Unknown content type: ", req$HTTP_CONTENT_TYPE)
      }
    },
    handleGET = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)
      resp_curl <- curl::curl_fetch_memory(url, handle = h)
      event <- makeHTTPEvent(private$server, req, resp_curl)
      private$server <- event$server
      private$writeEvent(event)
      resp_httr_to_rook(resp_curl)
    },
    handleCall = function(req) private[[paste0("handle", req$REQUEST_METHOD)]](req),
    handleWSOpen = function(clientWS) {
      cat("WS open!")
      private$clientWsState <- "OPEN"
      if (private$server == "local") {
        private$writeEvent(makeWSEvent("WS_OPEN", url = clientWS$request$PATH_INFO))
      } else {
        private$writeEvent(makeWSEvent("WS_OPEN", url =  stringr::str_replace_all(clientWS$request$PATH_INFO, c(
          "n=\\w+" = "n=${ROBUST_ID}",
          "t=\\w+" = "t=${TOKEN}",
          "w=\\w+" = "w=${WORKER}",
          "\\/\\w+\\/\\w+\\/websocket$" = "/${SOCKJSID}/websocket"
        ))))
      }

      wsScheme <- if (private$targetScheme == "https") "wss" else "ws"
      wsUrl <- paste0(wsScheme, "://", private$targetHost, ":", private$targetPort, "/", trimslash(private$targetPath), "/", trimslash(clientWS$request$PATH_INFO))

      serverWS <- websocket::WebsocketClient$new(wsUrl,
        headers = if (!is.null(private$sessionCookie)) c(Cookie = pasteParams(private$sessionCookie, "; ")),
        onMessage = function(msgFromServer) {
          if (private$server == "hosted") {

            if (msgFromServer == "o") {
              private$writeEvent(makeWSEvent("WS_RECV", message = msgFromServer))
              clientWS$send(msgFromServer)
              return(invisible())
            }

            # These kinds of messages are relayed to the browser but are not recorded.
            if (shouldIgnore(msgFromServer)) {
              clientWS$send(msgFromServer)
              return(invisible())
            }

            parsed <- parseMessage(msgFromServer)
            # If the message from the server is an object with a "config" key, fix
            # up some keys with placeholders and record the message as a
            # WS_RECV_INIT
            if ("config" %in% names(parsed)) {
              newMsgObj <- parsed
              newMsgObj$config$workerId <- "${WORKER}"
              newMsgObj$config$sessionId <- "${SESSION}"
              private$writeEvent(makeWSEvent("WS_RECV_INIT", message = spliceMessage(msgFromServer, newMsgObj)))
              clientWS$send(msgFromServer)
              return(invisible())
            }
          }
          # Every other websocket event
          private$writeEvent(makeWSEvent("WS_RECV", message = msgFromServer))
          clientWS$send(msgFromServer)
      }, onClose = function() {
        cat("Server disconnected\n")
        if (private$clientWsState == "OPEN") {
          clientWS$close()
          private$clientWsState <- "CLOSED"
        }
      })
      clientWS$onMessage(function(isBinary, msgFromClient) {
        private$writeEvent(makeWSEvent("WS_SEND", message = msgFromClient))
        serverWS$send(msgFromClient)
      })
      clientWS$onClose(function() {
        cat("Client disconnected\n")
        if (!(serverWS$getState() %in% c("CLOSED", "CLOSING"))) {
          serverWS$close()
        }
        private$writeEvent(makeWSEvent("WS_CLOSE"))
        self$stop()
      })
    },
    startServer = function() {
      private$localServer <- httpuv::startServer(private$localHost, private$localPort,
        list(call = private$handleCall, onWSOpen = private$handleWSOpen))
    }
  )
)

#' Title
#'
#' @param targetAppUrl
#' @param host
#' @param port
#' @param outputFile
#'
#' @return
#' @export
#'
#' @examples
recordSession <- function(targetAppUrl, host = "0.0.0.0", port = 8600,
  outputFile = "recording.log", openBrowser = TRUE) {
    sessionCookie <- if (isProtected(targetAppUrl)) {
      #username <- getPass::getPass("Enter your username: ")
      #password <- getPass::getPass("Enter your password: ")
      username <- "foo"
      password <- "barp"
      postLogin(targetAppUrl, username, password)
    } else NULL
    session <- RecordingSession$new(targetAppUrl, host, port, outputFile, sessionCookie)
    message("Listening on ", host, ":", port)
    if (openBrowser) browseURL(paste0("http://", host, ":", port))
    on.exit(session$stop())
    httpuv::service(Inf)
}
