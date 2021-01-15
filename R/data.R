#' U.S. State Abbreviations
#'
#' A vector containing U.S. state abbreviations, including the District of Columbia.
#'
#' @format Character vector of length 51
#' @source \url{https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations}
"states"

#' Extended State Abbreviations
#'
#' A vector containing abbreviations of U.S. states and territories.
#'
#' @format Character vector of length 51
#' @source \url{https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations}
"states_extended"

#' Country Codes
#'
#' A vector containing ISO country codes used in the Penn World Table.
#'
#' @format Character vector of length 182
#' @source Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table", American Economic Review, 105(10), 3150-3182, available for download at \url{https://www.rug.nl/ggdc/productivity/pwt/}
"countries"

#' Country Codes
#'
#' A vector containing ISO 2-digit country codes.
#'
#' @format Character vector of length 249
#' @source \url{https://en.wikipedia.org/wiki/ISO_3166}
"country2"

#' Country Codes
#'
#' A vector containing ISO 3-digit country codes.
#'
#' @format Character vector of length 249
#' @source \url{https://en.wikipedia.org/wiki/ISO_3166}
"country3"

#' Economic Releases
#'
#' A data.frame containing information on the economic releases in FRED. Information
#' is from January 2021. Use \code{\link{fred_releases}} for an up-to-date version.
#'
#' @format data.frame with 292 observations and 7 variables:
#' \describe{
#'   \item{id}{release id}
#'   \item{realtime_start, realtime_end}{date releases were saved}
#'   \item{name}{name of the release}
#'   \item{press_release}{logical value for whether there is an associated press release}
#'   \item{link}{a url for more data}
#'   \item{notes}{character string containing information about the release}
#' }
#' @source \url{https://fred.stlouisfed.org/docs/api/fred/releases.html}
"releases"


#' Sources of Economic Data
#'
#' A data.frame containing information on the sources of data in FRED. Information
#' is from January 2021. Use \code{\link{fred_sources}} for an up-to-date version.
#'
#' @format data.frame with 102 observations and 6 variables:
#' \describe{
#'   \item{id}{source id}
#'   \item{realtime_start, realtime_end}{date releases were saved}
#'   \item{name}{name of the source}
#'   \item{link}{a url that links to the source's website}
#'   \item{notes}{character string containing information about the source}
#' }
#' @source \url{https://fred.stlouisfed.org/docs/api/fred/sources.html}
"sources"


#' Popular Tags
#'
#' A data.frame containing information on 1000 of the most common tags in FRED. Information
#' is from January 2021. Use \code{\link{fred_tags}} for an up-to-date version
#' and the ability to refine it further.
#'
#' @format data.frame with 1000 observations and 6 variables:
#' \describe{
#'   \item{name}{tag name}
#'   \item{group_id}{name of the group associated with the tag}
#'   \item{created}{when the tag was created}
#'   \item{popularity}{integer of the tag's popularity (up to 100)}
#'   \item{notes}{character string containing information about the source}
#'   \item{series_count}{number of series associated with the tag}
#' }
#' @source \url{https://fred.stlouisfed.org/docs/api/fred/tags.html}
"tags"
