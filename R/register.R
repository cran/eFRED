#' Set a FRED API Key
#'
#' Function used to set the default key. If included, then \code{key} arguments
#' in FRED API calls are no longer needed. API keys can be requested at
#' \url{https://fred.stlouisfed.org/docs/api/api_key.html}.
#'
#' @param key 32 character lower-cased alpha-numeric character string
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' set_fred_key("abcdefghijklmnopqrstuvwxyz123456")
#'
set_fred_key <- function(key){
  key <- as.character(key)[1]
  if (nchar(key) != 32) stop("FRED API key must be exactly 32 characters.")
  Sys.setenv(FRED_KEY = key)
}
