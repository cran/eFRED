#' Fetch Information on a Release in FRED
#'
#' Each of the functions accept a release name/number and return the requested information in a data.frame.
#'
#' @param ... character or numeric vectors of release ids to search
#' @param key 32 character lower-cased alpha-numeric character string
#' @param args named list of other arguments passed to the API, including "limit", "offset", "sort_order", etc. See \code{\link{fred_search}} for more details on accepted parameters.
#'
#' @details
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_releases}}{Release id, its name, a url link, whether there is a press release, and realtime start and end dates}
#'  \item{\code{fred_releases_dates}}{Release id, its name, and date of release}
#'  \item{\code{fred_release}}{Release id, its name, a url link, whether there is a press release, and realtime start and end dates}
#'  \item{\code{fred_release_dates}}{Release id and its release date}
#'  \item{\code{fred_release_series}}{Series id, its title, observations start and end, frequency, units, seasonal adjustment type, popularity, realtime start and end, and when it was last updated}
#'  \item{\code{fred_release_sources}}{Source id, name of source, url link, and realtime start and end dates}
#'  \item{\code{fred_release_tags}}{Tag name and its group id, popularity of tag, number of series in each tag, when it was created, and other notes}
#'  \item{\code{fred_release_related_tags}}{Same as \code{fred_release_tags}}
#'  \item{\code{fred_release_tables}}{Release, element, parent, and series ids, type, and name}
#' }
#'
#' @return data.frame with entries described in details
#' @describeIn fred_releases get all releases of economic data
#' @export
#'
fred_releases <- function(key=NULL, args=list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_releases(key, other)
}
#' @describeIn fred_releases get the dates of all economic releases
#' @export
fred_releases_dates <- function(key=NULL, args=list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_releases_dates(key, other)
}

#' @describeIn fred_releases get information about a particular release
#' @export
fred_release <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release, key, other)
}

#' @describeIn fred_releases get release dates for a particular release
#' @export
fred_release_dates <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_dates, key, other)
}

#' @describeIn fred_releases get all series associated with a particular release
#' @export
fred_release_series <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_series, key, other)
}

#' @describeIn fred_releases get the sources associated with a particular release
#' @export
fred_release_sources <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_sources, key, other)
}

#' @describeIn fred_releases get all tags associated with a particular release
#' @export
fred_release_tags <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_tags, key, other)
}

#' @describeIn fred_releases get all related tags associated with a particular release
#' @export
fred_release_related_tags <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_related_tags, key, other)
}

#' @describeIn fred_releases get the release tables for a particular release
#' @export
fred_release_tables <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_release_tables, key, other)
}
