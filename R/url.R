URLBuilder <- R6::R6Class("URLBuilder",
  public = list(
    initialize = function(str) {
      if (!missing(str)) {
        # Not vectorized
        stopifnot(length(str) == 1)
        parsed <- xml2::url_parse(str)
        self$length <- nrow(parsed)
        self$scheme <- parsed[,"scheme"]
        self$host <- parsed[,"server"]
        self$port <- strtoi(parsed[,"port"])
        self$paths <- stringr::str_split(parsed[,"path"], "/")[[1]]
        self$paths <- self$paths[self$paths != "" & !is.na(self$paths)]
        self$query <- parsed[,"query"]
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
    # TODO Complexity with slashes only belongs in place where appending/joining to existing path
    setPaths = function(paths, raw = TRUE, append = FALSE) {
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
      # handling for a query passed on the url
      if(self$query != ""){

        query <- paste0("?",self$query)
      } else {
        query <- ""
      }
      paste0(scheme, host_port, "/", paths,query)
    },
    length = NA,
    scheme = NA,
    host = NA,
    port = NA,
    paths = character(0),
    query = NA
  )
)
