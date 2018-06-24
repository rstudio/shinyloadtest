req_rook_to_curl <- function(req, domain, port) {
  # Rename headers. Example: HTTP_CACHE_CONTROL => Cache-Control
  r <- as.list(req)

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

# Messages from the server start with a[, messages from the client start with [
# TODO Confirm with Joe if subapp IDs are hex too
messagePattern <- '^(a?\\[")([0-9A-F*]+#)?([0-9A-F]+)(\\|m\\|)(.*)("\\])$'

# Parses a JSON message from the server or client; returns the object from the nested JSON, if any
parseMessage <- function(msg) {
  res <- stringr::str_match(msg, messagePattern)
  encodedMsg <- res[1,6]
  # If the regex failed, then msg is probably a bare JSON string that can be
  # decoded directly.
  if (is.na(encodedMsg)) {
    jsonlite::fromJSON(msg)
  # If the regex succeeded but the subapp id was nonzero, crash with a helpful message.
  } else if (res[1,4] != "0") {
    stop("Subapp id was != 0 and subapp recording is not supported")
  } else {
    # If the regex succeeded subapp id = 0, we have the payload as an almost-double-JSON-encoded
    # object - it just needs to be wrapped in a set of double-quotes.
    wrappedMsg <- paste0('"', encodedMsg, '"')
    jsonlite::fromJSON(jsonlite::fromJSON(wrappedMsg))
  }
}

replaceTokens <- function(str, tokens) stringr::str_replace_all(str, unlist(tokens))

# Generate a new message based on originalMessage but with new contents
# {"type":"WS_RECV_INIT","created":"2018-03-12T19:51:59.675Z","message":"a[\"1#0|m|{\\\\\"config\\\\\":{\\\\\"workerId\\\\\":[\\\\\"${WORKER}\\\\\"],\\\\\"sessionId\\\\\":[\\\\\"${SESSION}\\\\\"],\\\\\"user\\\\\":null}}\"]"}
# {"type":"WS_RECV_INIT","created":"2018-03-12T20:21:27.086Z","message":"a[\"1#0|m|[\"{\\\"config\\\":{\\\"workerId\\\":[\\\"${WORKER}\\\"],\\\"sessionId\\\":[\\\"${SESSION}\\\"],\\\"user\\\":null}}\"]\"]"}
spliceMessage <- function(originalMessage, newMessageObject) {
  newMsg <- jsonlite::toJSON(jsonlite::unbox(jsonlite::toJSON(newMessageObject, null = 'null', auto_unbox = TRUE)))
  unquoted <- gsub("^.|.$", "", newMsg)
  group <- stringr::str_match(originalMessage, messagePattern)
  paste0(group[1,2], group[1,3], group[1,4], group[1,5], unquoted, group[1,7])
}

# TODO look at DT posts
# TODO write to separate files instead of embedding via base64. Files are likely to be huge.
# recording.log
# recording.log.file1
makeHTTPEvent_POST <- function(req, data, resp_curl, created = Sys.time()) {
  if (grepl("/upload/", req$PATH_INFO)) {
    structure(list(
      type = "REQ_POST_UPLOAD",
      created = makeTimestamp(created),
      statusCode = resp_curl$status_code,
      data = if (is.null(data)) NULL else httpuv::rawToBase64(data)
    ), class = "REQ")
  } else {
    stop("Unknown POST flavor")
  }
}

makeHTTPEvent_GET <- function(session, req, resp_curl, created = Sys.time()) {
  makeReq <- function(type) {
    structure(list(
      type = type,
      created = makeTimestamp(created),
      method = "GET",
      statusCode = resp_curl$status_code,
      url = replaceTokens(req$PATH_INFO, session$tokens)
    ), class = "REQ")
  }

  # ShinyHomeRequestEvent
  if (grepl("(\\/|\\.rmd)($|\\?)", req$PATH_INFO, ignore.case = TRUE)) {
    page <- rawToChar(resp_curl$content)
    workerId <- getWorkerId(page)
    if (!is.na(workerId)) session$tokens[[workerId]] <- "${WORKER}"
    return(makeReq("REQ_HOME"))
  }

  # ShinyTokenRequestEvent
  if (grepl("__token__", req$PATH_INFO, fixed = TRUE)) {
    return(makeReq("REQ_TOK"))
  }

  # ShinySINFRequestEvent
  if (grepl("__sockjs__/", req$PATH_INFO, fixed = TRUE)) {
    match <- stringr::str_match(req$PATH_INFO, "n=(\\w+)/t=(\\w+)/")
    if (is.na(match[[1]])) {
      error("Failed to match ROBUSTID and TOKEN strings in path for REQ_SINF")
    } else {
      session$tokens[[match[[2]]]] <- "${ROBUSTID}"
      session$tokens[[match[[3]]]] <- "${TOKEN}"
    }
    return(makeReq("REQ_SINF"))
  }

  # All other requests
  return(makeReq("REQ"))
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

shouldIgnore <- function(msg) {
  sockJSinit <- c('^o$', '^\\["0#0\\|o\\|"\\]$')
  acks <- c('^a\\["ACK.*$', '^\\["ACK.*$', '^h$')
  canIgnore <- c(sockJSinit, acks)
  if (length(unlist(stringr::str_match_all(msg, canIgnore))) > 0) return(TRUE)
  parsed <- parseMessage(msg)
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
    initialize = function(targetAppUrl, host, port, outputFileName, sessionCookies) {
      private$targetURL <- URLBuilder$new(targetAppUrl)
      private$localHost <- host
      private$localPort <- port
      private$outputFile <- file(outputFileName, "w")
      private$sessionCookies <- sessionCookies
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
    },
    # Stores a list of strings to their replacements.
    tokens = list()
  ),
  private = list(
    targetURL = NULL,
    localHost = NULL,
    localPort = NULL,
    localServer = NULL,
    outputFile = NULL,
    sessionCookies = data.frame(),
    clientWsState = NULL,
    uploadUrl = NULL,
    uploadJobId = NULL,
    writeEvent = function(evt) {
      writeLines(format(evt), private$outputFile)
      flush(private$outputFile)
    },
    makeUrl = function(req) {
      private$targetURL$appendPaths(raw = TRUE, paste0(req$PATH_INFO, req$QUERY_STRING))$build()
    },
    makeCurlHandle = function(req) {
      port <- private$targetURL$port %OR% if (private$targetURL$scheme == "https") 443 else 80
      req_curl <- req_rook_to_curl(req, private$targetURL$host, port)
      h <- curl::new_handle()

      if (nrow(private$sessionCookies) > 0) {
        req_curl[["Cookie"]] <- pasteParams(private$sessionCookies, "; ")
      }

      curl::handle_setheaders(h, .list = req_curl)

      # TODO See if there's an easy way to at least show a warning or message
      if (private$targetURL$scheme == "https") {
        curl::handle_setopt(h, ssl_verifyhost = 0, ssl_verifypeer = 0)
      }
      h
    },
    handle_POST = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)
      data <- NULL

      if (!is.null(req$HTTP_CONTENT_LENGTH)) {
        len <- as.integer(req$HTTP_CONTENT_LENGTH)
        if (len > 0) {
          data <- req$rook.input$read(len)
        }
      }

      if (!is.null(data)) {
        curl::handle_setopt(h,
          postfieldsize = length(data),
          postfields = data,
          post = TRUE
        )
      }

      resp_curl <- curl::curl_fetch_memory(url, handle = h)

      # Update sessionCookies in the case of __extendession__ XHR
      cookies_df <- curl::handle_cookies(h)[,c("name", "value")]
      if (nrow(cookies_df) > 0) private$sessionCookies <- cookies_df

      event <- makeHTTPEvent_POST(req, data, resp_curl)
      private$writeEvent(event)

      resp_httr_to_rook(resp_curl)
    },
    handle_GET = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)
      resp_curl <- curl::curl_fetch_memory(url, handle = h)
      event <- makeHTTPEvent_GET(self, req, resp_curl)
      private$writeEvent(event)
      resp_httr_to_rook(resp_curl)
    },
    handleCall = function(req) {
      private[[paste0("handle_", req$REQUEST_METHOD)]](req)
      # TODO Return a result to indicate when no method matches
    },
    handleWSOpen = function(clientWS) {
      cat("WS open!")
      private$clientWsState <- "OPEN"

      match <- stringr::str_match(clientWS$request$PATH_INFO, "/(\\w+/\\w+)/websocket$")
      if (!is.na(match[[1]])) self$tokens[[match[[2]]]] <- "${SOCKJSID}"

      private$writeEvent(makeWSEvent("WS_OPEN", url =  replaceTokens(clientWS$request$PATH_INFO, self$tokens)))

      wsScheme <- if (private$targetURL$scheme == "https") "wss" else "ws"
      wsUrl <- private$targetURL$setScheme(wsScheme)$appendPaths(raw = TRUE, clientWS$request$PATH_INFO)$build()

      serverWS <- websocket::WebsocketClient$new(wsUrl,
        headers = if (!is.null(private$sessionCookie)) c(Cookie = pasteParams(private$sessionCookie, "; ")),
        onMessage = function(msgFromServer) {

          # Relay but don't record ignorable messages
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
            # TODO Only the worker id is meaningful when hosted
            newMsgObj$config$workerId <- "${WORKER}"
            # TODO The session id is in everything (hosted, dev)
            newMsgObj$config$sessionId <- "${SESSION}"
            private$writeEvent(makeWSEvent("WS_RECV_INIT", message = spliceMessage(msgFromServer, newMsgObj)))
            clientWS$send(msgFromServer)
            return(invisible())
          }

          if(!is.null(parsed$response$value$jobId)) {
            # WS_RECV_BEGIN_UPLOAD (upload response)
            private$uploadUrl <- parsed$response$value$uploadUrl
            private$uploadJobId <- parsed$response$value$jobId
            newMsgObj <- parsed
            newMsgObj$response$value$uploadUrl <- "${UPLOAD_URL}"
            newMsgObj$response$value$jobId <- "${UPLOAD_JOB_ID}"
            private$writeEvent(makeWSEvent("WS_RECV_BEGIN_UPLOAD", message = spliceMessage(msgFromServer, newMsgObj)))
            clientWS$send(msgFromServer)
            return(invisible())
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
        if (shouldIgnore(msgFromClient)) {
          serverWS$send(msgFromClient)
          return()
        }
        parsed <- parseMessage(msgFromClient)
        if ("method" %in% names(parsed) && parsed$method == "uploadEnd") {
          newMsgObj <- parsed
          newMsgObj$args[[1]] <- "${UPLOAD_JOB_ID}"
          private$writeEvent(makeWSEvent("WS_SEND", message = spliceMessage(msgFromClient, newMsgObj)))
          serverWS$send(msgFromClient)
        } else {
          private$writeEvent(makeWSEvent("WS_SEND", message = msgFromClient))
          serverWS$send(msgFromClient)
        }
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
    sessionCookies <- if (isProtected(targetAppUrl)) {
      username <- getPass::getPass("Enter your username: ")
      password <- getPass::getPass("Enter your password: ")
      #username <- "foo"
      #password <- "barp"
      postLogin(targetAppUrl, username, password)
    } else data.frame()
    session <- RecordingSession$new(targetAppUrl, host, port, outputFile, sessionCookies)
    message("Listening on ", host, ":", port)
    if (openBrowser) browseURL(paste0("http://", host, ":", port))
    on.exit(session$stop())
    httpuv::service(Inf)
}
