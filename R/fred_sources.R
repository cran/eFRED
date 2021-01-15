#' Fetch Information on a Data Source in FRED
#'
#' Each of the functions accept a source number and return the requested information in a data.frame.
#'
#' @param ... character or numeric vectors of release ids to search
#' @param key 32 character lower-cased alpha-numeric character string
#' @param args named list of other arguments passed to the API, mostly for \code{fred_source_releases}, including "limit", "offset", "sort_order", etc. See \code{\link{fred_search}} for more details on accepted parameters.
#'
#' @details
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_sources}}{Source id, its name, a url link, and realtime start and end dates}
#'  \item{\code{fred_source}}{Same as \code{fred_sources}}
#'  \item{\code{fred_source_releases}}{Release id, its name, a url link, whether there is a press release, and realtime start and end dates}
#' }
#'
#' @return data.frame with entries described in details
#' @describeIn fred_sources get all sources of economic data
#' @export
#'
fred_sources <- function(key=NULL, args=list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_sources(key, other)
}

#' @describeIn fred_sources get information about a particular source
#' @export
fred_source <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_source, key, other)
}

#' @describeIn fred_sources get releases by a particular source
#' @export
fred_source_releases <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_source_releases, key, other)
}
