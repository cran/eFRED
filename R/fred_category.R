#' Fetch Information on a Category in FRED
#'
#' Each of the functions accept a category ID and return the requested information in a data.frame.
#'
#' @param ... character or numeric vectors of a category ID to fetch. Names are ignored.
#' @param key 32 character lower-cased alpha-numeric character string
#' @param realtime_start,realtime_end character date strings of format "YYY-MM-DD" used obtain information that was known during the specified time period. If empty, the latest update of the information is used.
#' @param args named list of other arguments passed to the API, including "limit", "offset", "sort_order", etc. See \code{\link{fred_search}} for more details on accepted parameters.
#'
#' @details
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_category}}{Category id, its name, and the id of its parent}
#'  \item{\code{fred_category_children}}{Same as \code{fred_category}}
#'  \item{\code{fred_category_related}}{Same as \code{fred_category}}
#'  \item{\code{fred_category_series}}{Series id, its title, observations start and end, frequency, units, seasonal adjustment type, popularity, realtime start and end, and when it was last updated}
#'  \item{\code{fred_category_tags}}{Tag name and its group id, popularity of tag, number of series in each tag, when it was created, and other notes}
#'  \item{\code{fred_category_related_tags}}{Same as \code{fred_category_tags}}
#' }
#'
#' @return data.frame with entries described in details
#' @describeIn fred_category get the name and parent_id for a FRED category
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- "abcdefghijklmnopqrstuvwxyz123456"
#'
#' # What category is the unemployment rate in?
#' un_cat <- fred_category("UNRATE", key=api_key)
#'
#' # What other series are in the same category?
#' other_in_cat <- fred_category_series(un_cat$id, key=api_key)
#' head(other_in_cat)
#' }
#'
fred_category <- function(..., key=NULL){
  if (is.null(key)) key <- .use_default_key()
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category, key, "")
}
#' @describeIn fred_category get the category children ids of a given parent_id
#' @export
fred_category_children <- function(..., key=NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  realtime <- .real_parse(realtime_start, realtime_end)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category_children, key, realtime)
}

#' @describeIn fred_category get the categories that are related to a category id
#' @export
fred_category_related <- function(..., key=NULL, realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  realtime <- .real_parse(realtime_start, realtime_end)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category_related, key, realtime)
}
#' @describeIn fred_category get all series within a particular category
#' @export
fred_category_series <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category_series, key, other)
}
#' @describeIn fred_category get all tags associated with a particular category
#' @export
fred_category_tags <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category_tags, key, other)
}
#' @describeIn fred_category get all tags related to tags within a particular category
#' @export
fred_category_related_tags <- function(..., key=NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  ids <- as.character(Reduce(c, list(...)))
  .loop_general_rbind(ids, .fred_category_related_tags, key, other)
}
