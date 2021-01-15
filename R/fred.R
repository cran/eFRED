#' Fetch Data Series from FRED
#'
#' Function that fetches economic data from the FRED based on the series ids.
#'
#' @param ... character vectors of series ids to search. Any names will be used to label the series in the resulting data.frame
#' @param key 32 character lower-cased alpha-numeric character string
#' @param realtime_start,realtime_end character date strings of format "YYY-MM-DD" used obtain information that was known during the specified time period. If empty, the latest update of the information is used.
#' @param all logical; should all observations be used? Defaults to \code{TRUE}. If \code{FALSE}, only observations existing in all series will be kept.
#' @param long logical; should the resulting data.frame be long or wide? Defaults to \code{FALSE} so that a new column is created for each series
#' @param info logical; should information about each series, such as units and title, be kept? If so, the information will be stored in a data.frame under the attribute \code{"info"}.
#'
#' @details
#' The \code{fred} function search for series ids from the FRED and returns the
#' values in a data.frame. Unless \code{long=TRUE}, a new column will be created
#' for each series. The dates for each observation will be included in the \code{"date"}
#' column and will be of class "Date". Each series will be a numeric value.
#'
#' If a series cannot be found, then it will be skipped and R will issue a warning.
#'
#' To access ALFRED, or the archived Federal Reserve Economic Data, use the \code{realtime_start}
#' and \code{realtime_end} arguments to specify a time frame. More details about
#' real time periods can be found at \url{https://fred.stlouisfed.org/docs/api/fred/realtime_period.html}.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- "abcdefghijklmnopqrstuvwxyz123456"
#'
#' # Naive Phillips Curve Estimation
#' df <- fred(p = "CPIAUCSL", u = "u6rate", key=api_key)
#' head(df)
#'
#' df$pi  <- log(df$p) - log(c(NA, df$p[2:nrow(df)]))
#' reg_pc <- lm(pi ~ u, data = df)
#' summary(reg_pc)
#'
#' # Different variations of the CPI
#' cpis <- c("CPIMEDSL", "CPIFABSL", "CPIHOSSL")
#' names(cpis) <- c("P_Med", "P_Food", "P_House")
#'
#' df_cpi <- fred(cpis, key=api_key, long=TRUE)
#' head(df)
#' }
fred <- function(..., key=NULL, all=TRUE, info=TRUE, realtime_start=NULL, realtime_end=NULL, long=FALSE){
  if (is.null(key)) key <- .use_default_key()
  realtime <- .real_parse(realtime_start, realtime_end)
  series   <- .id_parse(...)
  if (long){df <- .loop_id_rbind(series, .fred_series_observations, key, realtime)
  } else {df <- .loop_id_merge(series, .fred_series_observations, key, realtime, all=all, by="date")}
  if (info) attr(df, "info") <- .loop_id_rbind(series, .fred_series, key, realtime)
  df
}

#' Fetch and Create a Panel of Data
#'
#' Function that fetches data series with related codes and places the results into
#' a long data.frame.
#'
#' @param id character string containing the common series id segment
#' @param groups character vector denoting the variation in the series
#' @param type how should the group vectors be combined with the id? \code{"prefix"} places the groups before the \code{id}, \code{"postfix"} after the \code{id}, and use \code{"infix"} to place the group code between values in \code{id} *(requires \code{id} to be a length 2 character vector)*
#' @param key 32 character lower-cased alpha-numeric character string
#' @param name character string describing the name of the value column. If \code{NULL}, the \code{id} will be used.
#' @param grp_name character string describing the name of the group column. Defaults to \code{"group"}.
#' @param realtime_start,realtime_end character date strings of format "YYY-MM-DD" used obtain information that was known during the specified time period. If empty, the latest update of the information is used.
#'
#' @details
#' The \code{fred_group} command is convenience function that searches for many
#' different series at once and places them in a panel. This is similar to the
#' \code{\link{fred}} command except that it can be more convenient when the series
#' are related to each other. For example, the statewide unemployment rates from
#' the Bureau of Labor Statistics have all codes with the format: \code{"<State Abbreviation><UR>"}.
#' If state abbreviations are located in variable \code{state.abb}, then the command
#' \code{fred_group("UR", state.abb, "prefix", key)} would search place all unemployment
#' rates into a data.frame.
#'
#' \code{fred_state} is a wrapper around \code{fred_group} with the state abbreviations
#' used for groups and with the group column \code{"state"}.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- "abcdefghijklmnopqrstuvwxyz123456"
#'
#' # Panel of Unemployment Rates Across States
#' unemploy <- fred_state("UR", key = api_key)
#' unemploy
#'
#' # Panel of Constant GDP per Capita by Country
#' GDP <- fred_group("NYGDPPCAPKD", eFRED::countries, type="postfix",
#'   key=api_key, name= "gdppc", grp_name="country")
#' GDP
#' }
#'
fred_group <- function(id, groups, type = c("prefix", "postfix", "infix"), key=NULL, name = NULL, grp_name = "group", realtime_start=NULL, realtime_end=NULL){
  if (is.null(key)) key <- .use_default_key()
  realtime <- .real_parse(realtime_start, realtime_end)
  if (is.null(name)) name <- id[1]
  if (type[1] == "prefix"){id <- toupper(paste0(groups, id[1]))
  } else if (type[1] == "postfix"){id <- toupper(paste0(id[1], groups))
  } else if (type[1] == "infix"){id <- toupper(paste0(id[1], groups, id[2]))
  } else {stop("Type not recognized.")}

  vals <- lapply(seq_along(id), function(i){
    tryCatch(
      {
        res <- .fred_series_observations(id[i], key, realtime)
        res[[grp_name]] <- groups[i]
        res
      },
      error = function(e){
        warning(paste0("Series '", id[i], "' could not be found."))
        NULL
      }
    )
  })
  df <- Reduce(rbind, vals)
  colnames(df)[which(colnames(df) == "value")] <- name
  df
}
#' @describeIn fred_group Fetch data across each state
#' @export
fred_state <- function(id, type = c("prefix", "postfix", "infix"), key=NULL, name = NULL, realtime_start=NULL, realtime_end=NULL){
  fred_group(id, eFRED::states, type, key, name, "state", realtime_start, realtime_end)
}
