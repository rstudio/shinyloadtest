

#' Poll server(s) during load test
#'
#' @param servers vector of servers supporting app i.e.
#'   c('http://connect1.example.com', 'http://connect2.example.com')
#' @param appName name of the deployed application
#' @param duration_sec duration of polling in seconds
#' @param platform one of: 'connect'
#'
#' @return generates a real time plot of concurrent connections, aggregated
#'   load, and aggregated RAM used by the app
#'
#' @export
poll <- function(servers, appName, duration_sec, platform) {
  # validate inputs
  assertthat::assert_that(platform %in% c('connect'))

  ## TODO: validate other inputs
  ## especially servers is a vector of http(s):// looking things

  # get endpoints
  endpoints <- resolve_endpoints(servers, appName, platform)

  # initialize
  init <- data.frame(date = NULL, value = NULL, metric = NULL)
  cpu <- init; ram <- init; conns <- init;

  end <- Sys.time() + duration_sec
  plt <- TRUE
  pb <- progress::progress_bar$new(total = ceiling(duration_sec/5))
  while (Sys.time() < end) {
    pb$tick()
    ram <- rbind(ram, poll_multiple(endpoints, 'ram'))
    cpu <- rbind(cpu, poll_multiple(endpoints, 'cpu'))
    conns <- rbind(conns, poll_multiple(endpoints, 'concurrent'))

    # don't plot the first time
    if (plt)
      print(suppressWarnings({plot_metrics(rbind(ram, cpu, conns))}))

    Sys.sleep(5)
    plt <- TRUE

  }

  return(rbind(ram, cpu, conns))

}


#' Plot server-reported metrics
#'
#' @param metrics server reported metrics from \code{\link{poll}}
#' @import ggplot2
#' @export
plot_metrics <- function(metrics) {
  ggplot(metrics) +
    geom_line(aes(x = date, y = value)) +
    facet_grid(metric ~ ., scales = "free") +
    theme_minimal() +
    theme(
      panel.spacing = unit(2, "lines")
    )
}

#' Create endpoints object from inputs
#'
#' @param servers server base url, see \code{\link{poll}}
#' @param appName app name, see \code{\link{poll}}
#' @param platform one of: 'connect'
#'
#' @return a list where each server has an endpoint, which is a list with server: url,
#'   appName: appName. The endpoint's class is the platform, used to
#'   dispatch the appropriate polling methods
resolve_endpoints <- function(servers, appName, platform) {
  endpoints <- list()
  for (i in seq_along(servers)) {
    endpoints[[i]] <- list(
      server = servers[i],
      appName = appName
    )
    class(endpoints[[i]]) <- platform
  }
  endpoints
}

#' Responsible for polling the desired metrics across all servers and
#' aggregating the results
#'
#' @param endpoints see \code{\link{resolve_endpoints}}
#' @param metric one of: ram, cpu, concurrent
#'
#' @return data frame
poll_multiple <- function(endpoints, metric) {

  val <- c()
  for (i in seq_along(endpoints)) {
    val[i] <- poll_once(endpoints[[i]], metric)[['value']]
  }

  data.frame(
    date = Sys.time(),
    value = sum(val),
    metric = metric
  )

}


#' Switches between metrics for one server
#'
#' @param endpoint a single endpoint, see \code{\link{resolve_endpoints}}
#' @param metric one of ram, cpu, concurrent
#'
#' @return A dataframe row with timestamp, metric, and metric value for one server
poll_once <- function(endpoint, metric) {
  switch(metric,
         cpu = poll_once_cpu(endpoint),
         ram = poll_once_ram(endpoint),
         concurrent = poll_once_concurrent(endpoint)
  )
}

# Functions to dispatch based on platform

poll_once_cpu <- function(endpoint) {
  UseMethod("poll_once_cpu", endpoint)
}

poll_once_ram <- function(endpoint) {
  UseMethod("poll_once_ram", endpoint)
}

poll_once_concurrent <- function(endpoint) {
  UseMethod("poll_once_concurrent", endpoint)
}

# specific implementations
poll_once_ram.connect <- function(endpoint) {
  res <- poll_metrics(endpoint)
  check_app_running(endpoint, res)

  res <- res[which(res$appName == endpoint$appName),"ram"]

  if (nrow(res) > 1)
    res$ram <- sum(res$ram)

  res$date <- Sys.time()
  res$metric <- "ram"
  res$value <- res$ram
  res$ram <- NULL

  res
}

poll_once_cpu.connect <- function(endpoint) {
  res <- poll_metrics(endpoint)
  check_app_running(endpoint, res)

  res <- res[which(res$appName == endpoint$appName),"cpuCurrent"]

  if (nrow(res) > 1)
    res$cpu <- sum(res$cpu)

  res$date <- Sys.time()
  res$metric <- "cpu"
  res$value <- res$cpu
  res$cpu <- NULL

  res
}


poll_once_concurrent.connect <- function(endpoint) {
  # first, check if there are more than 1 app deployed
  res <- poll_metrics(endpoint)

  check_app_running(endpoint, res)

  if (length(unique(res$appName)) > 1)
    warning(paste0("More than 1 app running at ", endpoint$server))

  url <- paste0(endpoint$server, '/__health-check__')

  data.frame(
    value = auth_GET(url)$TypeCounts$sockJS,
    metric = 'concurrent',
    date = Sys.time()
  )
}

# helper for Connect authenticated endpoints with authorization header
# returns a response as a list
auth_GET <- function(url) {
  key = Sys.getenv("RSC_KEY")
  if (key == "")
    stop("Need RSC_KEY variable with a admin Connect API Key")

  res <- httr::GET(url,
                   httr::add_headers(Authorization = key)
  )

  if (res$status_code != 200)
    stop(paste0(url, ' not accessible'))

  httr::content(res)
}

poll_metrics <- function(endpoint) {
  if (class(endpoint) != 'connect')
    stop('This function is only used for Connect!')

  url <- paste0(endpoint$server, '/', '/__api__/metrics/procs')
  res <- purrr::map_df(auth_GET(url), ~.x)

  res
}

check_app_running <- function(endpoint, running) {
  if (class(endpoint) != 'connect')
    stop('This function is only used for Connect!')

  if (!(endpoint$appName %in% unique(running$appName)))
    stop(paste0(endpoint$appName, " is not running at ", endpoint$server))
}


