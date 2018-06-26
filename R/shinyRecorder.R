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

# TODO Make tokens an environment
# TODO Use begin/end timestamps for HTTP
# TODO Confirm Sys.time is the right kind of time thing to use with r-lib
makeHTTPEvent_GET <- function(session, req, resp_curl, begin, end) {
  makeReq <- function(type) {
    structure(list(
      type = type,
      begin = makeTimestamp(begin),
      end = makeTimestamp(end),
      statusCode = resp_curl$status_code,
      url = replaceTokens(paste0(req$PATH_INFO, req$QUERY_STRING), session$tokens)
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
  if (grepl("__token__", req$PATH_INFO)) {
    token <- rawToChar(resp_curl$content)
    cat("TOKEN=", token, "\n")
    session$tokens[[rawToChar(resp_curl$content)]] <- "${TOKEN}"
    return(makeReq("REQ_TOK"))
  }

  # ShinySINFRequestEvent
  # TODO Make this work even if n= appears elsewhere after __sockjs__/
  match <- stringr::str_match(req$PATH_INFO, "/__sockjs__/n=(\\w+)")
  if (!is.na(match[[1]])) {
    session$tokens[[match[[2]]]] <- "${ROBUST_ID}"
    return(makeReq("REQ_SINF"))
  }

  # All other requests
  # TODO Detect other non-WS sockjs protocols- URLs that have __sockjs__ but the right-hand side is something else
  return(makeReq("REQ"))
}

makeWSEvent <- function(type, begin = Sys.time(), ...) {
  structure(list(type = type, begin = makeTimestamp(begin), ...), class = "WS")
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

# Regex blacklist of paths to proxy but not record
ignoreGET <- c(".*favicon.ico$")
shouldIgnoreGET <- function(path) {
  length(unlist(stringr::str_match_all(path, ignoreGET))) > 0
}

RecordingSession <- R6::R6Class("RecordingSession",
  public = list(
    initialize = function(targetAppUrl, host, port, outputFileName, sessionCookies) {
      private$targetURL <- URLBuilder$new(targetAppUrl)
      private$localHost <- host
      private$localPort <- port
      private$outputFileName <- outputFileName
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
    outputFileName = NULL,
    outputFile = NULL,
    sessionCookies = data.frame(),
    clientWsState = NULL,
    postCounter = 0,
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
    # TODO Visit whether to store or ignore __extendsession__
    handle_POST = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)

      # If the post body contains data, write the data to a new file named
      # <outputFile>.post.<n> and set dataFileName.
      dataFileName <- NULL
      dataLen <- 0
      if (!is.null(req$HTTP_CONTENT_LENGTH)) {
        dataLen <- as.integer(req$HTTP_CONTENT_LENGTH)
        if (dataLen > 0) {
          sz <- 8192
          dataFileName <- sprintf("%s.post.%d", private$outputFileName, private$postCounter)
          writeCon <- file(dataFileName, "wb")
          repeat {
            data <- req$rook.input$read(sz)
            writeBin(data, writeCon)
            if (length(data) < sz) break
          }
          flush(writeCon)
          close(writeCon)
          private$postCounter <- private$postCounter + 1
        }
      }

      # If a post data file was written, send its contents upstream.
      # TODO Figure out a way to save the file and send it upstream concurrently
      if (!is.null(dataFileName)) {
        # TODO Figure out how to use CURL_INFILESIZE_LARGE to upload files
        # larger than 2GB.
        curl::handle_setopt(h, post = TRUE, infilesize = dataLen)
        readCon <- file(dataFileName, "rb")
        on.exit(close(readCon))
        curl::handle_setopt(h, readfunction = function(n) {
          readBin(readCon, raw(), n = n)
        })
      }

      resp_curl <- curl::curl_fetch_memory(url, handle = h)

      # Update sessionCookies in the case of __extendession__ XHR
      cookies_df <- curl::handle_cookies(h)[,c("name", "value")]
      if (nrow(cookies_df) > 0) private$sessionCookies <- cookies_df

      event <- list(
        type = "REQ_POST",
        created = makeTimestamp(),
        statusCode = resp_curl$status_code,
        url = replaceTokens(paste0(req$PATH_INFO, req$QUERY_STRING), self$tokens)
      )

      event <- append(event, if (!is.null(dataFileName)) list(datafile = basename(dataFileName)))
      private$writeEvent(structure(event, class = "REQ"))
      resp_httr_to_rook(resp_curl)
    },
    handle_GET = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)

      begin <- Sys.time()
      resp_curl <- curl::curl_fetch_memory(url, handle = h)
      end <- Sys.time()

      event <- makeHTTPEvent_GET(self, req, resp_curl, begin, end)

      if (!shouldIgnoreGET(req$PATH_INFO)) private$writeEvent(event)

      resp_httr_to_rook(resp_curl)
    },
    handleCall = function(req) {
      handler <- private[[paste0("handle_", req$REQUEST_METHOD)]]
      if (is.null(handler)) stop("No handler for ", req$REQUEST_METHOD)
      handler(req)
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
            return()
          }

          parsed <- parseMessage(msgFromServer)

          # If the message from the server is an object with a "config" key, fix
          # up some keys with placeholders and record the message as a
          # WS_RECV_INIT
          if ("config" %in% names(parsed)) {
            self$tokens[[parsed$config$sessionId]] <- "${SESSION}"
            private$writeEvent(makeWSEvent("WS_RECV_INIT", message = replaceTokens(msgFromServer, self$tokens)))
            clientWS$send(msgFromServer)
            return()
          }

          # WS_RECV_BEGIN_UPLOAD (upload response)
          if(!is.null(parsed$response$value$jobId)) {
            self$tokens[[parsed$response$value$uploadUrl]] <- "${UPLOAD_URL}"
            self$tokens[[parsed$response$value$jobId]] <- "${UPLOAD_JOB_ID}"
            private$writeEvent(makeWSEvent("WS_RECV_BEGIN_UPLOAD", message = replaceTokens(msgFromServer, self$tokens)))
            clientWS$send(msgFromServer)
            return()
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
        private$writeEvent(makeWSEvent("WS_SEND", message = replaceTokens(msgFromClient, self$tokens)))
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
