URLBuilder <- R6::R6Class("URLBuilder",
  public = list(
    initialize = function(str) {
      if (!missing(str)) {
        parsed <- urltools::url_parse(str)
        self$length <- nrow(parsed)
        self$scheme <- parsed[,"scheme"]
        self$host <- parsed[,"domain"]
        self$port <- strtoi(parsed[,"port"])
        self$paths <- stringr::str_split(parsed[,"path"], "/")
        self$fragment <- parsed[,"fragment"]
      }
    },
    setScheme = function(scheme) {
      with_let(copy = self$clone(), copy$scheme <- fill_to(scheme, self$length))
    },
    setHost = function(host) {
      with_let(copy = self$clone(), copy$host <- fill_to(host, self$length))
    },
    setPort = function(port) {
      with_let(copy = self$clone(), copy$port <- fill_to(port, self$length))
    },
    setPaths = function(paths, raw = FALSE, append = FALSE) {
      stopifnot(is.character(paths))
      paths <- if (raw) paths else URLencode(paths)
      paths <- paths[!paths == "/"]
      with_let(copy = self$clone(),
        copy$paths <- lapply(self$paths, function(oldpath) {
          if (append) c(oldpath, paths) else paths
        }))
    },
    appendPaths = function(paths, raw = FALSE) {
      self$setPaths(paths, raw = raw, append = TRUE)
    },
    build = function() {
      scheme <- paste0(ifelse(is.na(self$scheme), "http", self$scheme), "://")
      host_port <- paste0(self$host, ifelse(is.na(self$port), "", paste0(":", self$port)))
      paths <- sapply(self$paths, function(path) {
        path <- path[!is.na(path)]
        paste(collapse = "/", path)
      })
      paste0(scheme, host_port, "/", paths)
    },
    length = NA,
    scheme = NA,
    host = NA,
    port = NA,
    paths = character(0),
    fragment = NA
  )
)
