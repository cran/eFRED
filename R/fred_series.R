#' Fetch Data or Information on a SERIES in FRED
#'
#' Each of the functions accept a series ID from FRED and return a data.frame with information about the series.
#'
#' @param ... character vectors of series to fetch. If named, then the names are used to identify the series in the result.
#' @param key 32 character lower-cased alpha-numeric character string
#' @param all logical; should all observations be used? Defaults to \code{TRUE}. If \code{FALSE}, only observations existing in all series will be kept.
#' @param realtime_start,realtime_end character date strings of format "YYY-MM-DD" used obtain information that was known during the specified time period. If empty, the latest update of the information is used.
#'
#' @details
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_observations}}{Date of observation, its value, and the realtime start and end dates}
#'  \item{\code{fred_series}}{Series id, its title, observations start and end, frequency, units, seasonal adjustment type, popularity, realtime start and end, and when it was last updated}
#'  \item{\code{fred_series_categories}}{Category id, its name, and the id of its parent}
#'  \item{\code{fred_series_release}}{Release id, its name, a url link, whether there is a press release, and realtime start and end dates}
#'  \item{\code{fred_series_tags}}{Tag name, its group id, popularity, the number of series that are tagged, when it was created, and any notes about the tag}
#'  \item{\code{fred_series_related_tags}}{Same as \code{fred_series_tags}}
#'  \item{\code{fred_series_vintage}}{dates of each vintage}
#'
#'
#'  \item{\code{fred_category_related_tags}}{Same as \code{fred_category_tags}}
#' }
#'
#' @return data.frame or list
#' @describeIn fred_series get information about the series including observation periods, seasonal adjustment, units, title, etc.
#' @export
#'
fred_series <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_rbind(series, .fred_series, key, realtime)
}
#' @describeIn fred_series get the release information about the series
#' @export
fred_series_release <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_rbind(series, .fred_series_release, key, realtime)
}
#' @describeIn fred_series get the categories into which a series falls
#' @export
fred_series_categories <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_rbind(series, .fred_series_category, key, realtime)
}
#' @describeIn fred_series get the FRED tags for the series
#' @export
fred_series_tags <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_rbind(series, .fred_series_tags, key, realtime)
}
#' @describeIn fred_series get the observational data for a series
#' @export
fred_observations <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL, all=TRUE){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_merge(series, .fred_series_observations, key, realtime, all=all, by="date")
}
#' @describeIn fred_series get the dates in which the data for the series were revised or released. Returns a named list of dates.
#' @export
fred_series_vintage <- function(..., key = NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  series   <- .id_parse(...)
  realtime <- .real_parse(realtime_start, realtime_end)
  .loop_id_list(series, .fred_series_vintage, key, realtime)
}
