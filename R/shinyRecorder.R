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

insertTokenPlaceholders <- function(tokens, server, url) {
  if (server == "local") {
    url
  } else if (server == "hosted") {
  } else {
    stop("Unkown server:", server)
  }
}

# Make a fake one: newEvent <- makeEvent(empty_env(), NULL, list(PATH_INFO="/"), list(content = charToRaw(page)))
makeHTTPEvent <- function(tokens, server, req, resp_curl, created = Sys.time()) {
  if (req$REQUEST_METHOD != "GET") stop("Unsupported method, only handle GET:", req$REQUEST_METHOD)
  # ShinyTokenRequestEvent,
  # x ShinyHomeRequestEvent,
  # ShinySockJSInfoRequestEvent,
  # x ShinyRequestEvent
  if (grepl("(\\/|\\.rmd)($|\\?)", req$PATH_INFO, ignore.case = TRUE)) {
    page <- resp_curl$content %>% rawToChar()
    workerId <- getWorkerId(page)
    structure(list(
      newTokens = if (is.na(workerId)) tokens else child_env(tokens, WORKER = workerId),
      type = "REQ_HOME",
      created = makeTimestamp(created),
      method = "GET",
      server = if (is.na(workerId)) "local" else "hosted",
      url = req$PATH_INFO,
      statusCode = 200
    ), class = "REQ")
  } else {
    structure(list(
      newTokens = tokens,
      type = "REQ",
      created = makeTimestamp(created),
      method = "GET",
      server = server,
      url = insertTokenPlaceholders(tokens, server, req$PATH_INFO),
      statusCode = 200
    ), class = "REQ")
  }
}

format.REQ = function(httpEvt) {
  lst <- unclass(httpEvt)
  jsonlite::toJSON(lst[names(lst) != "newTokens"], auto_unbox = TRUE)
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
      if (is.null(private$localServer)) stop("Can't stop, server not running")
      httpuv::stopServer(private$localServer)
      private$localServer <- NULL
      close(private$outputFile)
    }
  ),
  private = list(
    targetHost = NULL,
    targetPort = NULL,
    localHost = NULL,
    localPort = NULL,
    localServer = NULL,
    outputFile = NULL,
    tokens = rlang::child_env(rlang::empty_env()),
    server = NULL,
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

      event <- makeHTTPEvent(private$tokens, private$server, req, resp_curl)
      private$tokens <- event$newTokens
      private$server <- event$server
      private$writeEvent(event)

      resp_httr_to_rook(resp_curl)
    },
    handleWSOpen = function(clientWS) {
      if (private$server == "local") {
        # emit simple WS_OPEN with url: "/websocket/"
        writeLines(jsonlite::toJSON(list(
            type = "WS_OPEN",
            created = makeTimestamp(),
            url = clientWS$request$PATH_INFO),
          auto_unbox = TRUE
          ),
          private$outputFile
        )
      } else {
        # emit WS_OPEN and replace token values in url with token name placeholders
      }
      wsUrl <- paste0("ws://", private$targetHost, ":", private$targetPort)
      serverWS <- websocketClient::WebsocketClient$new(wsUrl, onMessage = function(msgFromServer) {
        cat("Got message from server: ", msgFromServer, "\n")
        clientWS$send(msgFromServer)
      })
      clientWS$onMessage(function (isBinary, msgFromClient) {
        cat("Got message from client: ", msgFromClient, "\n")
        serverWS$send(msgFromClient)
      })
    },
    startServer = function() {
      private$localServer <- httpuv::startServer(private$localHost, private$localPort,
        list(call = private$handleCall, onWSOpen = private$handleWSOpen)
      )
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
