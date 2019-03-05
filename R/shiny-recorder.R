# These headers must be removed from requests and responses relayed by proxies.
hop_by_hop_headers <- c(
  "connection",
  "keep-alive",
  "proxy-authenticate",
  "proxy-authorization",
  "te",
  "trailer",
  "transfer-encoding",
  "upgrade"
)

connection_tokens <- function(connection_header) {
  if (!is.null(connection_header)) {
    lower <- stringr::str_to_lower(connection_header)
    stringr::str_trim(stringr::str_split(lower, ";")[[1]])
  } else character(0)
}

headers_to_remove <- function(connection_header) {
  union(hop_by_hop_headers, connection_tokens(connection_header))
}

req_rook_to_curl <- function(req, domain, port) {
  # Rename headers. Example: HTTP_CACHE_CONTROL => cache-control
  r <- as.list(req)

  r <- r[grepl("^HTTP_", names(r))]
  nms <- names(r)
  nms <- sub("^HTTP_", "", nms)
  nms <- tolower(nms)
  nms <- gsub("_", "-", nms, fixed = TRUE)
  nms <- gsub("\\b([a-z])", "\\U\\1", nms, perl = TRUE)
  names(r) <- nms

  names(r) <- tolower(names(r))

  # Overwrite host field
  if (port == 80) {
    r$host <- domain
  } else {
    r$host <- paste0(domain, ":", port)
  }

  r[headers_to_remove(r$connection)] <- NULL

  r
}

resp_httr_to_rook <- function(resp) {
  # TODO Look into HTTP/2.0 support
  status <- as.integer(sub("^HTTP\\S+ (\\d+).*", "\\1", curl::parse_headers(resp$headers)[1]))
  headers <- curl::parse_headers_list(resp$headers)
  headers[headers_to_remove(headers$connection)] <- NULL
  headers[["content-encoding"]] <- NULL
  list(
    status = status,
    headers = headers,
    body = resp$content
  )
}

makeTimestamp <- function(time = Sys.time()) {
  format_string <- "%Y-%m-%dT%H:%M:%OS3Z"
  format(time, format_string, tz = "UTC")
}

# Returns NA if workerid not found. This either indicates an error state of some
# kind, or more likely, the Shiny session is running locally.
getWorkerId <- function(page) {
  pat <- ".*<base href=\"_w_([0-9a-z]+)/.*"
  stringr::str_match(page, pat)[[2]]
}

# Messages from the server start with a[, messages from the client start with [
messagePattern <- '^(a?\\[")([0-9A-F*]+#)?([0-9]+)(\\|[mc]\\|)(.*)("\\])$'
closePattern <- '^c\\[([0-9]+),(".*")\\]$'

# Parses a JSON message from the server or client; returns the object from the nested JSON, if any
parseMessage <- function(msg) {
  res <- stringr::str_match(msg, messagePattern)
  encodedMsg <- res[1,6]
  if (!is.na(encodedMsg)) {
    if (res[1,4] != "0") {
      # If the regex succeeded but the subapp id was nonzero, crash with a helpful message.
      stop("Subapp id was != 0 and subapp recording is not supported")
    } else {
      # If the regex succeeded subapp id = 0, we have the payload as an almost-double-JSON-encoded
      # object - it just needs to be wrapped in a set of double-quotes.
      wrappedMsg <- paste0('"', encodedMsg, '"')
      jsonlite::fromJSON(jsonlite::fromJSON(wrappedMsg))
    }
  } else {
    res2 <- stringr::str_match(msg, closePattern)
    if (!is.na(res2[1,2])) {
      # This is a SockJS-level close message, for now we just ignore these.
      list()
    } else {
      # If the regex failed, then msg is probably a bare JSON string that can be
      # decoded directly. It might also be an older-style SSP message without subapp
      # support (like "[\"0|o|\"]")
      jsonlite::fromJSON(msg)
    }
  }
}

replaceTokens <- function(str, tokens) {
  if (length(tokens) > 0) {
    stringr::str_replace_all(str, unlist(as.list(tokens)))
  } else {
    str
  }
}

makeHTTPEvent_GET <- function(tokens, req, resp_curl, begin, end) {
  makeReq <- function(type) {
    structure(list(
      type = type,
      begin = makeTimestamp(begin),
      end = makeTimestamp(end),
      status = resp_curl$status_code,
      url = replaceTokens(paste0(req$PATH_INFO, req$QUERY_STRING), tokens)
    ), class = "REQ")
  }

  # ShinyHomeRequestEvent
  if (grepl("(\\/|\\.rmd)($|\\?)", req$PATH_INFO, ignore.case = TRUE)) {
    page <- rawToChar(resp_curl$content)
    workerId <- getWorkerId(page)
    if (!is.na(workerId)) tokens[[workerId]] <- "${WORKER}"
    return(makeReq("REQ_HOME"))
  }

  # ShinyTokenRequestEvent
  if (grepl("__token__", req$PATH_INFO)) {
    token <- rawToChar(resp_curl$content)
    tokens[[token]] <- "${TOKEN}"
    return(makeReq("REQ_TOK"))
  }

  # ShinySINFRequestEvent
  match <- stringr::str_match(req$PATH_INFO, "/__sockjs__/(?:.*/)?n=(\\w+)")
  if (!is.na(match[[1]])) {
    tokens[[match[[2]]]] <- "${ROBUST_ID}"
    # Crude workaround for https://github.com/rstudio/shinyloadtest/issues/41
    Sys.sleep(0.75)
    return(makeReq("REQ_SINF"))
  }

  # All other requests
  # TODO Detect other non-WS sockjs protocols- URLs that have __sockjs__ but the right-hand side is something else
  return(makeReq("REQ_GET"))
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
  canIgnore <- c('^a\\["ACK.*$', '^\\["ACK.*$', '^h$')
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
      if (grepl("shinyapps.io$", private$targetURL$host)) {
        stop("Recording shinyapps.io apps is not supported")
      }
      private$localHost <- host
      private$localPort <- port
      private$outputFileName <- outputFileName
      private$outputFile <- file(outputFileName, "w")
      header <- c(
        paste0("# version: ", packageVersion("shinyloadtest")),
        paste0("# target: ", targetAppUrl)
      )
      writeLines(header, private$outputFile)
      private$sessionCookies <- sessionCookies
      private$startServer()
    },
    stop = function() {
      if (!(is.null(private$localServer))) {
        message("Stopping server")
        httpuv::stopServer(private$localServer)
        httpuv::interrupt()
        private$localServer <- NULL
        close(private$outputFile)
      }
    },
    # An environment of session-specific identifier strings to their
    # session-agnostic ${PLACEHOLDER} strings.
    tokens = new.env(parent = emptyenv())
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
    postFileCounter = 0,
    writeEvent = function(evt) {
      writeLines(format(evt), private$outputFile)
      flush(private$outputFile)
    },
    mergeCookies = function(handle) {
      df <- curl::handle_cookies(handle)[,c("name", "value")]
      df <- rbind(private$sessionCookies, df)
      df <- subset(df, !duplicated(df$name, fromLast = TRUE))
      private$sessionCookies <- df
    },
    makeUrl = function(req) {
      query <- gsub("\\?", "", req$QUERY_STRING)
      private$targetURL$appendPaths(req$PATH_INFO)$setQuery(query)$build()
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
      dataFileName <- NULL
      # See if there is data to upload
      # TODO: Detect chunked transfer encoding and error out
      if (!is.null(req$HTTP_CONTENT_LENGTH) && as.integer(req$HTTP_CONTENT_LENGTH) > 0) {
        # TODO Figure out how to use CURL_INFILESIZE_LARGE to upload files
        # larger than 2GB.
        curl::handle_setopt(h, post = TRUE, infilesize = as.integer(req$HTTP_CONTENT_LENGTH))
        dataFileName <- sprintf("%s.post.%d", private$outputFileName, private$postFileCounter)
        writeCon <- file(dataFileName, "wb")
        curl::handle_setopt(h, readfunction = function(n) {
          data <- req$rook.input$read(n)
          writeBin(data, writeCon)
          data
        })
        on.exit({
          flush(writeCon)
          close(writeCon)
          private$postFileCounter <- private$postFileCounter + 1
        })
      }

      begin <- Sys.time()
      resp_curl <- curl::curl_fetch_memory(url, handle = h)
      end <- Sys.time()

      private$mergeCookies(h)

      event <- list(
        type = "REQ_POST",
        begin = makeTimestamp(begin),
        end = makeTimestamp(end),
        status = resp_curl$status_code,
        url = replaceTokens(paste0(req$PATH_INFO, req$QUERY_STRING), self$tokens)
      )

      if (!is.null(dataFileName)) {
        event$datafile <- basename(dataFileName)
      }

      private$writeEvent(structure(event, class = "REQ"))
      resp_httr_to_rook(resp_curl)
    },
    handle_GET = function(req) {
      h <- private$makeCurlHandle(req)
      url <- private$makeUrl(req)
      begin <- Sys.time()
      resp_curl <- curl::curl_fetch_memory(url, handle = h)
      end <- Sys.time()
      private$mergeCookies(h)

      event <- makeHTTPEvent_GET(self$tokens, req, resp_curl, begin, end)

      if (!shouldIgnoreGET(req$PATH_INFO)) private$writeEvent(event)

      resp_httr_to_rook(resp_curl)
    },
    handleCall = function(req) {
      handler <- private[[paste0("handle_", req$REQUEST_METHOD)]]
      if (is.null(handler)) stop("No handler for ", req$REQUEST_METHOD)
      handler(req)
    },
    handleWSOpen = function(clientWS) {
      message("Client connected")
      private$clientWsState <- "OPEN"

      match <- stringr::str_match(clientWS$request$PATH_INFO, "/(\\w+/\\w+)/websocket$")
      if (!is.na(match[[1]])) self$tokens[[match[[2]]]] <- "${SOCKJSID}"

      private$writeEvent(makeWSEvent("WS_OPEN", url =  replaceTokens(clientWS$request$PATH_INFO, self$tokens)))

      wsScheme <- if (private$targetURL$scheme == "https") "wss" else "ws"
      wsUrl <- private$targetURL$setScheme(wsScheme)$appendPaths(clientWS$request$PATH_INFO)$build()

      serverWS <- websocket::WebSocket$new(wsUrl,
        headers = if (nrow(private$sessionCookies) > 0) {
          c(Cookie = pasteParams(private$sessionCookies, "; "))
        } else c())
      serverWS$onMessage(function(event) {
        msgFromServer <- event$data

        # The SockJS init message (doesn't happen with local server) needs to
        # be recorded and results in an acknowledgement message from the
        # client. We handle it specially here because it shouldn't be ignored,
        # but it's also not formatted the way all other messages are. We
        # should receive this message only once per session, immediately after
        # WS open.
        if (msgFromServer == "o") {
          private$writeEvent(makeWSEvent("WS_RECV", message = msgFromServer))
          clientWS$send(msgFromServer)
          return()
        }

        # Relay but don't record ignorable messages
        if (shouldIgnore(msgFromServer)) {
          clientWS$send(msgFromServer)
          return()
        }

        # From here forward, we assumed the message
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
      })
      serverWS$onClose(function(event) {
        message("Server disconnected")
        if (private$clientWsState == "OPEN") {
          clientWS$close()
          private$clientWsState <- "CLOSED"
        }
      })
      serverWS$onOpen(function(event) {
        msgs <- serverSendBuffer
        serverSendBuffer <<- list()
        for (msg in msgs) {
          serverWS$send(msg)
        }
      })

      serverSendBuffer <- list()
      serverSend <- function(msg) {
        if (serverWS$readyState() != 1L || length(serverSendBuffer) > 0) {
          serverSendBuffer <<- c(serverSendBuffer, list(msg))
        } else {
          serverWS$send(msg)
        }
      }

      clientWS$onMessage(function(isBinary, msgFromClient) {
        if (shouldIgnore(msgFromClient)) {
          serverSend(msgFromClient)
          return()
        }
        private$writeEvent(makeWSEvent("WS_SEND", message = replaceTokens(msgFromClient, self$tokens)))
        serverSend(msgFromClient)
      })
      clientWS$onClose(function() {
        message("Client disconnected")
        if (serverWS$readyState() <= 1L) {
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

#' Record a Session for Load Test
#' @details This function creates a reverse proxy listening at \code{host:port}
#'   that stands in front of \code{target_app_url}. As the user interacts with
#'   the app, the actions and responses are recorded in a file that can be
#'   replayed later. By default, a web browser is opened automatically so that
#'   you can begin recording immediately, without manually starting a browser
#'   and navigating to \code{host:port}.
#' @param target_app_url The URL of the deployed application
#' @param host The host where the proxy will run. Usually localhost is used.
#' @param output_file The name for the generated log file.
#' @param open_browser Whether to open a browser on the proxy
#'   (default=\code{TRUE}) or not (\code{FALSE}).
#' @param port The port for the proxy. Default is 8600. Change this default if
#'   port 8600 is used by another service.
#'
#' @return Creates a recording file that can be used to drive a load test.
#' @export
record_session <- function(target_app_url, host = "127.0.0.1", port = 8600,
  output_file = "recording.log", open_browser = TRUE) {
    sessionCookies <- if (isProtected(target_app_url)) {
      username <- getPass::getPass("Enter your username: ")
      if (is.null(username)) {
        return(invisible(FALSE))
      }
      password <- getPass::getPass("Enter your password: ")
      if (is.null(password)) {
        return(invisible(FALSE))
      }
      postLogin(target_app_url, username, password)
    } else data.frame()
    session <- RecordingSession$new(target_app_url, host, port, output_file, sessionCookies)
    on.exit(session$stop())
    message("Listening on ", host, ":", port)

    if (open_browser){
      navUrl <- URLBuilder$new(target_app_url)$setHost(host)$setPort(port)$setScheme("http")$setPaths("")$build()
      message("Navigating to: ", navUrl)
      utils::browseURL(navUrl)
    }

    httpuv::service(Inf)
    invisible(TRUE)
}
