.fred_category <- function(id, key, other){
  .fred_general(id, key, "category?category_id=", "")$categories
}

.fred_category_children <- function(id, key, other){
  .fred_general(id, key, "category/children?category_id=", other)$categories
}

.fred_category_related <- function(id, key, other){
  .fred_general(id, key, "category/related?category_id=", other)$categories
}

.fred_category_series <- function(id, key, other){
  .fred_general(id, key, "category/series?category_id=", other)$seriess
}

.fred_category_tags <- function(id, key, other){
  .fred_general(id, key, "category/tags?category_id=", other)$tags
}

.fred_category_related_tags <- function(id, key, other){
  .fred_general(id, key, "category/related_tags?category_id=", other)$tags
}


.fred_tags <- function(key, other){
  .fred_general("", key, "tags?", other)$tags
}

.fred_related_tags <- function(ids, key, other){
  .fred_general(paste(gsub("\\s+", "+", ids), collapse=";"), key, "related_tags?tag_names=", "")$tags
}

.fred_tags_series <- function(ids, key, other){
  .fred_general(paste(gsub("\\s+", "+", ids), collapse=";"), key, "tags/series?tag_names=", "")$seriess
}


.fred_releases <- function(key, other){
  .fred_general("", key, "releases?", other)$releases
}

.fred_releases_dates <- function(key, other){
  x <- .fred_general("", key, "releases/dates?", other)$release_dates
  x$date <- as.Date(x$date)
  x
}

.fred_release <- function(id, key, other){
  .fred_general(id, key, "release?release_id=", other)$releases
}

.fred_release_dates <- function(id, key, other){
  x <- .fred_general(id, key, "release/dates?release_id=", other)$release_dates
  x$date <- as.Date(x$date)
  x
}

.fred_release_series <- function(id, key, other){
  x <- .fred_general(id, key, "release/series?release_id=", other)$seriess
  x$observation_start <- as.Date(x$observation_start)
  x$observation_end   <- as.Date(x$observation_end)
  x$last_updated      <- as.POSIXct(x$last_updated)
  x
}

.fred_release_sources <- function(id, key, other){
  .fred_general(id, key, "release/sources?release_id=", other)$sources
}

.fred_release_tags <- function(id, key, other){
  x <- .fred_general(id, key, "release/tags?release_id=", other)$tags
  x$created <- as.POSIXct(x$created)
  x
}

.fred_release_related_tags <- function(id, key, other){
  x <- .fred_general(id, key, "release/related_tags?release_id=", other)$tags
  x$created <- as.POSIXct(x$created)
  x
}

.fred_release_tables <- function(id, key, other){
  .fred_general(id, key, "release/tables?release_id=", other)$elements
}

.fred_sources <- function(key, other){
  .fred_general("", key, "sources?", other)$sources
}

.fred_source <- function(id, key, other){
  .fred_general(id, key, "source?source_id=", other)$sources
}

.fred_source_releases <- function(id, key, other){
  .fred_general(id, key, "source/releases?source_id=", other)$sources
}
