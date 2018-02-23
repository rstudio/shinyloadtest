


# 
# # library(curl)
# # library(httpuv)
# # library(websocketClient)
# 
# 
# 
# req_rook_to_curl <- function(req, host) {
#   # browser()
#   # Rename headers. Example: HTTP_CACHE_CONTROL => Cache-Control
#   r <- as.list(req)
#   
#   # Uncomment to print out request headers
#   #cat("== Original ==\n")
#   #cat(capture.output(print(str(r))), sep = "\n")
#   
#   r <- r[grepl("^HTTP_", names(r))]
#   nms <- names(r)
#   nms <- sub("^HTTP_", "", nms)
#   nms <- tolower(nms)
#   nms <- gsub("_", "-", nms, fixed = TRUE)
#   nms <- gsub("\\b([a-z])", "\\U\\1", nms, perl = TRUE)
#   names(r) <- nms
#   
#   # Overwrite host field
#   r$Host <- host
#   # Uncomment to print out modified request headers
#   #cat("== Modified ==\n")
#   #cat(capture.output(print(str(r))), sep = "\n")
#   r
# }
# 
# 
# resp_httr_to_rook <- function(resp) {
#   # browser()
#   status <- as.integer(sub("^HTTP\\S+ (\\d+).*", "\\1", curl::parse_headers(resp$headers)[1]))
#   list(
#     status = status,
#     headers = parse_headers_list(resp$headers),
#     body = resp$content
#   )
# }
# 
# 
# s <- startServer("0.0.0.0", 5002,
#   list(
#     call = function(req) {
#       host <- "localhost:3923"
#       
#       req_curl <- req_rook_to_curl(req, host)
#       h <- new_handle()
#       do.call(handle_setheaders, c(h, req_curl))
#       
#       resp_curl <- curl_fetch_memory(paste0("http://", host, req$PATH_INFO), handle = h)
#       
#       resp_httr_to_rook(resp_curl)
#     },
#     onWSOpen = function(clientWS) {
#       serverWS <- WebsocketClient$new("ws://localhost:3923", onMessage = function(msgFromServer) {
#         cat("Got message from server: ", msgFromServer, "\n")
#         clientWS$send(msgFromServer)
#       })
#       clientWS$onMessage(function (isBinary, msgFromClient) {
#         cat("Got message from client: ", msgFromClient, "\n")
#         serverWS$send(msgFromClient)
#       })
#     }
#   )
# )
# 
# # Stop the server
# # stopServer(s)
