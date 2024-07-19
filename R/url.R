# Join two path components with a slash, ensuring only one slash remains between
# them.
joinPaths <- function(p1, p2) {
  left_slash <- endsWith(p1, "/")
  right_slash <- startsWith(p2, "/")
  if ((!left_slash && right_slash) || (left_slash && !right_slash)) {
    paste0(p1, p2)
  } else if (left_slash && right_slash) {
    paste0(p1, substring(p2, 2))
  } else {
    paste0(p1, "/", p2)
  }
}

URLBuilder <- R6::R6Class("URLBuilder",
  public = list(
    initialize = function(str) {
      if (!missing(str)) {
        # Not vectorized
        stopifnot(length(str) == 1)
        parsed <- xml2::url_parse(str)
        self$length <- nrow(parsed)
        self$scheme <- parsed[, "scheme"]
        self$host <- parsed[, "server"]
        self$port <- strtoi(parsed[, "port"])
        self$path <- parsed[, "path"]
        self$query <- parsed[, "query"]
      }
    },
    setScheme = function(scheme) {
      copy <- self$clone()
      copy$scheme <- scheme
      copy
    },
    setHost = function(host) {
      copy <- self$clone()
      copy$host <- host
      copy
    },
    setPort = function(port) {
      copy <- self$clone()
      copy$port <- port
      copy
    },
    setQuery = function(query = "") {
      copy <- self$clone()
      copy$query <- query
      copy
    },
    setPath = function(path) {
      stopifnot(is.character(path))
      copy <- self$clone()
      copy$path <- path
      copy
    },
    appendPath = function(path) {
      self$setPath(joinPaths(self$path, path))
    },
    build = function() {
      scheme <- paste0(ifelse(self$scheme == "", "http", self$scheme), "://")
      host_port <- paste0(self$host, ifelse(is.na(self$port), "", paste0(":", self$port)))
      query <- ifelse(self$query == "", "", paste0("?", self$query))
      paste0(scheme, joinPaths(host_port, self$path), query)
    },
    length = NA,
    scheme = NA,
    host = NA,
    port = NA,
    path = "",
    query = NA
  )
)
