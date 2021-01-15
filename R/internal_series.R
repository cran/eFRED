#' @importFrom  httr GET content
#' @importFrom jsonlite fromJSON
.fred_search <- function(text, key, type, other=""){
  url <- paste0(
    'https://api.stlouisfed.org/fred/series/search',
    type,
    gsub("\\s+", "+", text)[1],
    "&api_key=", key,
    "&file_type=json",
    other
  )
  return(fromJSON(content(GET(url), as="text")))
}

#' @importFrom  httr GET content
#' @importFrom jsonlite fromJSON
.fred_general <- function(id, key, type, other=""){
  url <- paste0(
    'https://api.stlouisfed.org/fred/',
    type, id,
    "&api_key=", key,
    "&file_type=json",
    other
  )
  return(fromJSON(content(GET(url), as="text")))
}

#' @importFrom  httr GET content
#' @importFrom jsonlite fromJSON
.fred_get_series <- function(id, key, type, other=""){
  url <- paste0(
    'https://api.stlouisfed.org/fred/series',
    type,
    "?series_id=", toupper(id),
    "&api_key=", key,
    "&file_type=json",
    other
  )
  return(fromJSON(content(GET(url), as="text")))
}

.fred_series <- function(id, key, other=""){
  val <- .fred_get_series(id, key, "", other)$seriess
  val$realtime_start <- as.Date(val$realtime_start)
  val$realtime_end   <- as.Date(val$realtime_end)
  val$observation_start <- as.Date(val$observation_start)
  val$observation_end   <- as.Date(val$observation_end)
  val$last_updated      <- as.POSIXct(val$last_updated)
  val
}

.fred_series_observations <- function(id, key, other=""){
  vals <- .fred_get_series(id, key, "/observations", other)
  if (other == ""){
    data.frame(
      date  = as.Date(vals$observations$date),
      value = as.numeric(vals$observations$value)
    )
  } else {
    data.frame(
      realtime_start = as.Date(vals$observations$realtime_start),
      realtime_end   = as.Date(vals$observations$realtime_end),
      date  = as.Date(vals$observations$date),
      value = as.numeric(vals$observations$value)
    )
  }
}

.fred_series_release <- function(id, key, other=""){
  val <- .fred_get_series(id, key, "/release", other)$releases
  val$realtime_start <- as.Date(val$realtime_start)
  val$realtime_end   <- as.Date(val$realtime_end)
  val
}

.fred_series_category <- function(id, key, other=""){
  .fred_get_series(id, key, "/categories", other)$categories
}

.fred_series_tags <- function(id, key, other=""){
  val <- .fred_get_series(id, key, "/tags", other)$tags
  val$created <- as.POSIXct(val$created)
  val
}

.fred_series_vintage <- function(id, key, other=""){
  as.Date(.fred_get_series(id, key, "/vintagedates", other)$vintage_dates)
}



