#' Fetch Information on a Tag in FRED
#'
#' Each of the functions accept a tag name and return the requested information in a data.frame.
#'
#' @param ... character vectors of tag names to search
#' @param key 32 character lower-cased alpha-numeric character string
#' @param args named list of other arguments passed to the API, including "limit", "offset", "sort_order", etc. See \code{\link{fred_search}} for more details on accepted parameters.
#'
#' @details
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_tags}}{Tag name, its group id, popularity, the number of series that are tagged, when it was created, and any notes about the tag}
#'  \item{\code{fred_related_tags}}{Same as \code{fred_tags}}
#'  \item{\code{fred_tags_series}}{Series id, its title, observations start and end, frequency, units, seasonal adjustment type, popularity, realtime start and end, and when it was last updated}
#' }
#'
#'
#' @return data.frame with entries described in details
#' @describeIn fred_tags get the tag names associated with the search parameters
#' @export
#'
fred_tags <- function(key=NULL, args=list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_tags(key, other)
}
#' @describeIn fred_tags get the tags that are related to one or more tag names
#' @export
fred_related_tags <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- parse_search(...)
  .fred_related_tags(ids, key, other)
}

#' @describeIn fred_tags get the series associated with tags
#' @export
fred_tags_series <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- parse_search(...)
  .fred_tags_series(ids, key, other)
}
