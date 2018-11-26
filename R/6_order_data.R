#' Reorder the List of Coordinates by the Initial Order.
#'
#' After the user first runs \code{\link{get_lon180}} or
#' \code{\link{test_geocoord}} and \code{\link{get_country}},
#' \code{\link{get_country_shoreline}} or \code{\link{get_sea}}, this function
#' uses the output of those. It combines by row data frames with geographic
#' names assigned to the coordinates and orders the data by the same order as in
#' the input text file (given by the user and with the list of coordinates
#' only). If some problem were found in the coordinates, instead of a geographic
#' name a brief description of the problem will be assigned.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item Data frames created by \code{\link{get_country}},
#' \code{\link{get_country_shoreline}}, \code{\link{get_sea}} and
#' \code{\link{get_lon180}} or \code{\link{test_geocoord}}, respectively. The
#' function runs with only one of those.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file named 'geographic_names.txt' with the coordinate pairs by
#' the inicial order and the following header: \strong{id | lon | lat |
#' geo_name}. If a name could not be assigned because of the polygon's data set
#' lack of coverage, the description in the field 'geo_name' will be
#' "MISSING_NAME". If a name could not be assigned because the coordinates are
#' missing or out of bounds, the description will be 'ERRONEOUS-MISSING_COORDS'.
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' ## First run
#' get_lon180(coords_lon360)
#' ## Or
#' test_geocoord(coords_sample)
#' ## Then run sequentially:
#' get_country(icoords)
#' get_country_shoreline(icoords, tol)
#' get_sea(icoords)
#' order_data(countries = NULL, countries_sh = NULL, seas = NULL, miss_seas =
#' NULL, excl_coords = NULL)
#' }
#'
#' @usage
#' order_data(countries = NULL, countries_sh = NULL, seas = NULL, miss_seas =
#' NULL, excl_coords = NULL)
#'
#' @param countries data frame output of \code{\link{get_country}}.
#' @param countries_sh data frame output of \code{\link{get_country_shoreline}}.
#' @param seas data frame output of \code{\link{get_sea}}.
#' @param miss_seas data frame output of \code{\link{get_sea}}.
#' @param excl_coords data frame output of \code{\link{get_lon180}} or
#'   \code{\link{test_geocoord}}.
#'
#' @import utils
#'
#' @export
#'
order_data <- function(countries = NULL, countries_sh = NULL, seas = NULL,
  miss_seas = NULL, excl_coords = NULL) {
  df <- data.frame()
  if (!is.null(countries)) {
    countries <- countries[, 1:4, drop = FALSE]
    names(countries)[4] <- "geo_name"
    df <- rbind(df, countries)
    geo_name <- df[order(df[, 1]), ]
  }
  if (!is.null(countries_sh)) {
    countries_sh <- countries_sh[, 1:4, drop = FALSE]
    names(countries_sh)[4] <- "geo_name"
    df <- rbind(df, countries_sh)
    geo_name <- df[order(df[, 1]), ]
  }
  if (!is.null(seas)) {
    names(seas)[4] <- "geo_name"
    df <- rbind(df, seas)
    geo_name <- df[order(df[, 1]), ]
  }
  if (!is.null(miss_seas)) {
    miss_seas$geo_name <- "MISSING_NAME"
    df <- rbind(df, miss_seas)
    geo_name <- df[order(df[, 1]), ]
  }
  if (!is.null(excl_coords)) {
    excl_coords$geo_name <- "ERRONEOUS-MISSING_COORDS"
    df <- rbind(df, excl_coords)
    geo_name <- df[order(df[, 1]), ]
  }
  write.table(geo_name, file = "geographic_names.txt", row.names = FALSE,
    col.names = TRUE, sep = "\t", quote = FALSE)
  cat("\n")
  cat("Please, check 'geographic_names.txt'.\n\n")
}
