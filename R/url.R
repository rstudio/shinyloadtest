URLBuilder <- R6::R6Class("URLBuilder",
  public = list(
    initialize = function(str) {
      # TODO parse str if non-missing and non-empty
    },
    clone = function() {
      copy <- URLBuilder$new()
      copy$scheme <- self$scheme
      copy$host <- self$host
      copy$port <- self$port
      copy$paths <- self$paths
      fragment <- self$fragment
      copy
    },
    setScheme = function(scheme) {
      if (!scheme %in% c("http", "https", "ws", "wss"))
        error("Unknown URL scheme: ", scheme)
      copy <- self$clone()
      copy$scheme <- scheme
      copy
    },
    setHost = function(host) {
      copy <- self$clone()
      copy$host <- host
      copy
    }
    setPort = function(port) {
      if (!is.numeric(port) || port < 0 || port > 65535)
        error("Invalid port: ", port)
      copy <- self$clone()
      copy$port <- port
      copy
    }
    appendPaths = function(paths, raw = FALSE) {
      # TODO assert paths is a character vector
      # TODO url encode on the way in if raw is TRUE
    }
    build = function() {
      # TODO construct valid url string and return
      # TODO error if any required pieces are missing
    }
    scheme = "http",
    host = NULL,
    port = NULL,
    paths = character(0),
    fragment = NULL
  )
)
