#' Fetch Series with Recent Updates
#'
#' Function that returns a data.frame containing information on series in FRED that were recently updated.
#'
#' @param key 32 character lower-cased alpha-numeric character string
#' @param args named list of other arguments passed to the API. See details.
#'
#' @details
#'
#' Arguments accept any of the following:
#'
#' \describe{
#'  \item{\code{realtime_start}, \code{realtime_end}}{character string of format \code{"YYYY-MM-DD"} determining the real-time period. This is used to find historical series.}
#'  \item{\code{limit}}{integer between 1 and 1000 determining the maximum number of results to return.}
#'  \item{\code{offset}}{Non-negative integer}
#'  \item{\code{filter_value}}{Either \code{"macro"}, \code{"regional"}, or \code{"all"} *(the default)* describing the geographic type of the series}
#'  \item{\code{start_time}, \code{end_time}}{character string of format \code{"YYYYMMDDHhmm"} determining the time range, down to minutes, with which to filter the results}
#' }
#'
#'
#' @return data.frame with values for the series id, its title, start and end date of its observations, its frequency, units, whether it is seasonally adjusted, its popularity, when it was last updated, and the realtime start and end dates
#' @export
#'
fred_updates <- function(key = NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  val <- .fred_general("", key, "updates?", other)$series
  val$realtime_start <- as.Date(val$realtime_start)
  val$realtime_end   <- as.Date(val$realtime_end)
  val$observation_start <- as.Date(val$observation_start)
  val$observation_end   <- as.Date(val$observation_end)
  val$last_updated      <- as.POSIXct(val$last_updated)
  val
}
