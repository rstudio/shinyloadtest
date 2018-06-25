URLBuilder <- R6::R6Class("URLBuilder",
  public = list(
    initialize = function(str) {
      if (!missing(str)) {
        # Not vectorized
        stopifnot(length(str) == 1)
        parsed <- urltools::url_parse(str)
        self$length <- nrow(parsed)
        self$scheme <- parsed[,"scheme"]
        self$host <- parsed[,"domain"]
        self$port <- strtoi(parsed[,"port"])
        self$paths <- stringr::str_split(parsed[,"path"], "/")[[1]]
        self$paths <- self$paths[self$paths != "" & !is.na(self$paths)]
        self$fragment <- parsed[,"fragment"]
      }
    },
    setScheme = function(scheme) {
      copy <- self$clone()
      copy$scheme <- scheme
      copy
    },
    setHost = function(host) {
      copy <-  self$clone()
      copy$host <- host
      copy
    },
    setPort = function(port) {
      copy <-  self$clone()
      copy$port <- port
      copy
    },
    setPaths = function(paths, raw = FALSE, append = FALSE) {
      stopifnot(is.character(paths))
      paths <- if (raw) paths else URLencode(paths)
      paths <- unlist(stringr::str_split(paths, "/"))
      paths <- paths[paths != "" & !is.na(paths)]
      copy <- self$clone()
      copy$paths <- if (append) c(self$paths, paths) else paths
      copy
    },
    appendPaths = function(paths, raw = FALSE) {
      self$setPaths(paths, raw = raw, append = TRUE)
    },
    build = function() {
      scheme <- paste0(ifelse(is.na(self$scheme), "http", self$scheme), "://")
      host_port <- paste0(self$host, ifelse(is.na(self$port), "", paste0(":", self$port)))
      paths <- paste(collapse = "/", self$paths)
      paste0(scheme, host_port, "/", paths)
    },
    length = NA,
    scheme = NA,
    host = NA,
    port = NA,
    paths = character(0),
    # TODO
    fragment = NA
  )
)
