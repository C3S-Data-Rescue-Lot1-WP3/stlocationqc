#' ISPD stations
#'
#' A dataset of coordinate pairs with the longitude in the range [0, 360],
#' extracted from the list of meteorological stations belonging to \strong{ISPD
#' - International Surface Pressure Databank}.
#'
#' @format A data frame with 83814 rows and 2 variables:
#' \describe{
#'   \item{lon}{longitude in decimal degrees (0 to 360) and with two decimal
#'   places}
#'   \item{lat}{latitude in decimal degrees (-90 to 83.65) and with two decimal
#'   places}
#' }
#' @references \url{http://reanalyses.org/observations/international-surface-pressure-databank}
"ispd"
#'
#' ERACLIM Upper-air sations
#'
#' A dataset of coordinate pairs with the longitude in the range [-180, +180]
#' and respective country, extracted from the \strong{ERACLIM Upper-air
#' Observations - Fixed Platforms} inventory.
#'
#' @format A data frame with 6444 rows and 3 variables:
#' \describe{
#'   \item{lon}{longitude in decimal degrees with three decimal places, that
#'   should be in the range (-180 to 180) but has out of bounds values and
#'   missing values}
#'   \item{lat}{latitude in decimal degrees with three decimal places, which
#'   has some missing values}
#'   \item{country_gv}{country name given in the inventory, which is written in
#'   different languages and has non ASCII characters}
#' }
#' @source \url{http://eraclim-global-registry.rd.ciencias.ulisboa.pt/upperairdata.php}
"eraclim_uao_fp"
