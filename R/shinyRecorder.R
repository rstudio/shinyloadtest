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
# WS_OPEN, WS_CLOSE, WS_SEND, WS_RECV

RecordingSession <- R6::R6Class("RecordingSession",
  public = list(
    initialize = function(targetAppUrl, host, port, outputFile) {
      parsedUrl <- urltools::url_parse(targetAppUrl)
      private$targetHost <- parsedUrl$domain
      private$targetPort <- parsedUrl$port %OR% 80

      private$localHost <- host
      private$localPort <- port
      private$outputFile <- outputFile

      private$startServer()
    },
    stop = function() {
      if (is.null(private$localServer)) stop("Can't stop, server not running")
      httpuv::stopServer(private$localServer)
      private$localServer <- NULL
    }
  ),
  private = list(
    targetHost = NULL,
    targetPort = NULL,
    localHost = NULL,
    localPort = NULL,
    localServer = NULL,
    outputFile = NULL,
    handleCall = function(req) {
      req_curl <- req_rook_to_curl(req, private$targetHost, private$targetPort)
      h <- curl::new_handle()
      do.call(curl::handle_setheaders, c(h, req_curl))
      httpUrl <- paste0("http://", private$targetHost, ":", private$targetPort, req$PATH_INFO)
      resp_curl <- curl::curl_fetch_memory(httpUrl, handle = h)
      # TODO: Pass req and the response code from resp_curl to a function that interprets these values and writes a REQ_* line to the session log.
      resp_httr_to_rook(resp_curl)
    },
    handleWSOpen = function(clientWS) {
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
