req_rook_to_curl <- function(req, domain, port) {
  # browser()
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
  # browser()
  status <- as.integer(sub("^HTTP\\S+ (\\d+).*", "\\1", curl::parse_headers(resp$headers)[1]))
  list(
    status = status,
    headers = curl::parse_headers_list(resp$headers),
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

# Parses a JSON message from the server; returns the object from the nested JSON, if any
parseMessage <- function(msg) {
  res <- stringr::str_match(msg, '^a\\["([0-9A-F*]+#)?0\\|m\\|(.*)"\\]$')
  encodedMsg <- res[1,3]
  if (is.na(encodedMsg)) stop("Couldn't parse WS message")
  jsonlite::fromJSON(gsub('\\\\\"', '\"', encodedMsg, fixed = TRUE))
}

# val allowedTokens: HashSet<String> = hashSetOf("WORKER", "TOKEN", "ROBUST_ID", "SOCKJSID", "SESSION")
# ShinySockJSInfoRequestEvent,
#
# Make a fake one: newEvent <- makeEvent(empty_env(), NULL, list(PATH_INFO="/"), list(content = charToRaw(page)))
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
  } else if (grepl("__token__?_=", req$PATH_INFO, fixed = TRUE)) {
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
  # {"type":"WS_RECV_INIT","created":"2017-12-14T16:43:34.414Z","message":"a[\"1#0|m|{\\\\\"config\\\\\":{\\\\\"workerId\\\\\":\\\\\"${WORKER}\\\\\",\\\\\"sessionId\\\\\":\\\\\"${SESSION}\\\\\",\\\\\"user\\\\\":null}}\"]"}
}

format.REQ = function(httpEvt) {
  jsonlite::toJSON(unclass(httpEvt), auto_unbox = TRUE)
}

format.WS = function(wsEvt) {
  jsonlite::toJSON(unclass(wsEvt), auto_unbox = TRUE)
}

RecordingSession <- R6::R6Class("RecordingSession",
  public = list(
    initialize = function(targetAppUrl, host, port, outputFileName) {
      parsedUrl <- urltools::url_parse(targetAppUrl)
      private$targetHost <- parsedUrl$domain
      private$targetPort <- parsedUrl$port %OR% 80

      private$localHost <- host
      private$localPort <- port
      private$outputFile <- file(outputFileName, "w")

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
    targetHost = NULL,
    targetPort = NULL,
    localHost = NULL,
    localPort = NULL,
    localServer = NULL,
    outputFile = NULL,
    server = NULL,
    clientWsState = NULL,
    writeEvent = function(evt) {
      writeLines(format(evt), private$outputFile)
      flush(private$outputFile)
    },
    handleCall = function(req) {
      req_curl <- req_rook_to_curl(req, private$targetHost, private$targetPort)
      h <- curl::new_handle()
      do.call(curl::handle_setheaders, c(h, req_curl))
      httpUrl <- paste0("http://", private$targetHost, ":", private$targetPort, req$PATH_INFO)
      resp_curl <- curl::curl_fetch_memory(httpUrl, handle = h)

      event <- makeHTTPEvent(private$server, req, resp_curl)
      private$server <- event$server
      private$writeEvent(event)

      resp_httr_to_rook(resp_curl)
    },
    handleWSOpen = function(clientWS) {
      private$clientWsState <- "OPEN"
      if (private$server == "local") {
        private$writeEvent(makeWSEvent("WS_OPEN", url = clientWS$request$PATH_INFO))
      } else {
        # {"type":"WS_OPEN","created":"2017-12-14T16:43:34.273Z","url":"/__sockjs__/n=${ROBUST_ID}/t=${TOKEN}/w=${WORKER}/s=0/${SOCKJSID}/websocket"}",
        private$writeEvent(makeWSEvent("WS_OPEN", url =  stringr::str_replace_all(clientWS$request$PATH_INFO, c(
          "n=\\w+" = "n=${ROBUST_ID}",
          "t=\\w+" = "t=${TOKEN}",
          "w=\\w+" = "w=${WORKER}",
          "\\/\\w+\\/\\w+\\/websocket$" = "/${SOCKJSID}/websocket"
        ))))
        # !!!!! not here !!!!!! private$writeEvent(makeWSEvent("WS_RECV_INIT", url = clientWS$request$PATH_INFO))
      }
      wsUrl <- paste0("ws://", private$targetHost, ":", private$targetPort)
      serverWS <- websocketClient::WebsocketClient$new(wsUrl,
        onMessage = function(msgFromServer) {
          parsed <- parseMessage(msgFromServer)
          private$writeEvent(makeWSEvent("WS_RECV", message = msgFromServer))

          clientWS$send(msgFromServer)
      }, onDisconnected = function() {
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
recordSession <- function(targetAppUrl, host = "0.0.0.0", port = 8600, outputFile = "recording.log") {
  session <- RecordingSession$new(targetAppUrl, host, port, outputFile)
  message("Listening on ", host, ":", port)
  on.exit(session$stop())
  httpuv::service(Inf)
}
