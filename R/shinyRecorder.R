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

#' Title
#'
#' @param targetAppUrl
#' @param listenPort
#'
#' @return
#' @export
#'
#' @examples
recordSession <- function(targetAppUrl, listenPort = 8600) {
  parsedUrl <- urltools::url_parse(targetAppUrl)
  targetAppPort <- parsedUrl$port %OR% 80
  httpuv::startServer("0.0.0.0", listenPort,
    list(
      call = function(req) {
        req_curl <- req_rook_to_curl(req, parsedUrl$domain, targetAppPort)
        h <- curl::new_handle()
        do.call(curl::handle_setheaders, c(h, req_curl))
        httpUrl <- paste0("http://", parsedUrl$domain, ":", targetAppPort, req$PATH_INFO)
        resp_curl <- curl::curl_fetch_memory(httpUrl, handle = h)
        resp_httr_to_rook(resp_curl)
      },
      onWSOpen = function(clientWS) {
        wsUrl <- paste0("ws://", parsedUrl$domain, ":", targetAppPort)
        serverWS <- websocketClient::WebsocketClient$new(wsUrl, onMessage = function(msgFromServer) {
          cat("Got message from server: ", msgFromServer, "\n")
          clientWS$send(msgFromServer)
        })
        clientWS$onMessage(function (isBinary, msgFromClient) {
          cat("Got message from client: ", msgFromClient, "\n")
          serverWS$send(msgFromClient)
        })
      }
    )
  )
}
