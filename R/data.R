#' Metrics for a single-user simulation suitable for passing to \code{\link{load_runs}}
#'
#' A dataset containing latency data for 1323 \code{shinycannon} events.
#'  The variables are as follows:
#'
#' @format A data frame with 1323 rows and 12 variables:
#' \describe{
#'   \item{run}{Name of the run}
#'   \item{session_id}{simulated session identifier, 0-based}
#'   \item{user_id}{simulated user identifier, 0-based}
#'   \item{iteration}{user session identifier, 0-based}
#'   \item{input_line_number}{recording line number associated with event}
#'   \item{event}{type of the event}
#'   \item{start}{time the event started, in seconds, relative to the time at which all simulated users were running.}
#'   \item{end}{time the event ended, in seconds, relative to the time at which all simulated users were running}
#'   \item{time}{event duration, in seconds}
#'   \item{concurrency}{number of events that happened at the same time as this one}
#'   \item{maintenance}{whether this event occurred before or after all simulated users were running}
#'   \item{label}{event-specific text label}
#'   \item{json}{Removed for portability. Normally would contain the raw message JSON And parsed JSON Of the event.}
#' }
"slt_demo_data_1"

#' Metrics for a 4-user simulation suitable for passing to \code{\link{load_runs}}
#'
#' A dataset containing latency data for 3294 \code{shinycannon} events.
#'  The variables are as follows:
#'
#' @format A data frame with 3294 rows and 12 variables:
#' \describe{
#'   \item{run}{Name of the run}
#'   \item{session_id}{simulated session identifier, 0-based}
#'   \item{user_id}{simulated user identifier, 0-based}
#'   \item{iteration}{user session identifier, 0-based}
#'   \item{input_line_number}{recording line number associated with event}
#'   \item{event}{type of the event}
#'   \item{start}{time the event started, in seconds, relative to the time at which all simulated users were running.}
#'   \item{end}{time the event ended, in seconds, relative to the time at which all simulated users were running}
#'   \item{time}{event duration, in seconds}
#'   \item{concurrency}{number of events that happened at the same time as this one}
#'   \item{maintenance}{whether this event occurred before or after all simulated users were running}
#'   \item{label}{event-specific text label}
#'   \item{json}{Removed for portability. Normally would contain the raw message JSON And parsed JSON Of the event.}
#' }
"slt_demo_data_4"

#' Metrics for a 16-user simulation suitable for passing to \code{\link{load_runs}}
#'
#' A dataset containing latency data for 3942 \code{shinycannon} events.
#'  The variables are as follows:
#'
#' @format A data frame with 3942 rows and 12 variables:
#' \describe{
#'   \item{run}{Name of the run}
#'   \item{session_id}{simulated session identifier, 0-based}
#'   \item{user_id}{simulated user identifier, 0-based}
#'   \item{iteration}{user session identifier, 0-based}
#'   \item{input_line_number}{recording line number associated with event}
#'   \item{event}{type of the event}
#'   \item{start}{time the event started, in seconds, relative to the time at which all simulated users were running.}
#'   \item{end}{time the event ended, in seconds, relative to the time at which all simulated users were running}
#'   \item{time}{event duration, in seconds}
#'   \item{concurrency}{number of events that happened at the same time as this one}
#'   \item{maintenance}{whether this event occurred before or after all simulated users were running}
#'   \item{label}{event-specific text label}
#'   \item{json}{Removed for portability. Normally would contain the raw message JSON And parsed JSON Of the event.}
#' }
"slt_demo_data_16"
