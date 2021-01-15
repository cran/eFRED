#' Search for Series on FRED
#'
#' Each function receives a character string and returns a data.frame containing information on different series, tags, or related tags of the search.
#'
#' @param text character string used for search. All spaces will automatically be converted appropriately.
#' @param key 32 character lower-cased alpha-numeric character string
#' @param args named list of other arguments passed to the API, including "limit", "offset", "sort_order", etc. See details below.
#'
#' @details
#'
#' Search arguments accept any of the following:
#'
#' \describe{
#'  \item{\code{search_type}}{either \code{"full_text"} or \code{"series_id"} determining whether the description, title, units, and other information are searched or only the id. Defaults to full text.}
#'  \item{\code{realtime_start}, \code{realtime_end}}{character string of format \code{"YYYY-MM-DD"} determining the real-time period. This is used to find historical series.}
#'  \item{\code{limit}}{integer between 1 and 1000 determining the maximum number of results to return.}
#'  \item{\code{offset}}{Non-negative integer}
#'  \item{\code{order_by}}{One of the following: \code{"search_rank"}, \code{"series_id"}, \code{"title"}, \code{"units"}, \code{"frequency"}, \code{"seasonal_adjustment"}, \code{"realtime_start"}, \code{"realtime_end"}, \code{"last_updated"}, \code{"observation_start"}, \code{"observation_end"}, \code{"popularity"}, or \code{"group_popularity"}. Default is \code{"search_rank"}.}
#'  \item{\code{sort_order}}{Either \code{"asc"} for ascending or \code{"desc"} for descending order.}
#'  \item{\code{filter_variable}}{String to filter the results by.}
#'  \item{\code{tag_names}}{String of tag names separated by semicolons for filtering results.}
#'  \item{\code{exclude_tag_names}}{String of tag names separated by semicolons that should not be included.}
#' }
#'
#' These should be included in a list such as \code{args = list(limit = 10, sort_order = "asc")}.
#' The results are included in a data.frame with all information about the series,
#' including detailed notes. The results are best viewed in the RStudio Viewer or
#' with the title extracted.
#'
#' Each function returns the following information in a data.frame.
#'
#' \describe{
#'  \item{\code{fred_search}}{Series id, its title, observations start and end, frequency, units, seasonal adjustment type, popularity, realtime start and end, and when it was last updated}
#'  \item{\code{fred_search_tags}}{Tag name and its group id, popularity of tag, number of series in each tag, when it was created, and other notes}
#'  \item{\code{fred_search_related_tags}}{Same as \code{fred_search_tags}}
#' }
#'
#' @return data.frame with entries described in details
#' @describeIn fred_search obtain series related to search
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- "abcdefghijklmnopqrstuvwxyz123456"
#'
#' # Searching GDP in China, descending order by "title"
#' results <- fred_search("GDP China", key=api_key,
#'   args = list(sort_order="desc", order_by="title"))
#'
#' # Fetch top 2 series
#' df <- fred(results$id[1:2])
#' head(df)
#' }
#'
fred_search <- function(text, key = NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_search(text, key, "?search_text=", other)$seriess
}
#' @describeIn fred_search obtain tags related to search
#' @export
fred_search_tags <- function(text, key = NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_search(text, key, "/tags?series_search_text=", other)$tags
}
#' @describeIn fred_search obtain related tags related to search
#' @export
fred_search_related_tags <- function(text, key = NULL, args = list()){
  if (is.null(key)) key <- .use_default_key()
  other <- .args_parse(args)
  .fred_search(text, key, "/related_tags?series_search_text=", other)$tags
}


#' Convert Search Terms to a API Friendly Format
#'
#' The function replaces spaces from character vectors with \code{"+"} and separates
#' search terms with \code{";"}. Use whenever search terms are needed in the \code{args}
#' list for API calls. It is automatically applied for \code{\link{fred_search}} and not needed.
#'
#' @param ... character vectors of search terms
#'
#' @return character string
#' @export
#'
#' @examples
#' terms <- c("Real GDP", "medical inflation", "Japan")
#' parse_search(terms, "unemployment")
parse_search <- function(...){
  chars <- as.character(Reduce(c, list(...)))
  paste(gsub("\\s+", "+", chars), collapse=";")
}
