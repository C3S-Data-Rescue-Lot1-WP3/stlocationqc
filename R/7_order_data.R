#' Reorder the List of Coordinates by the Initial Order.
#'
#' After running sequentially \code{\link{get_lon180}} or
#' \code{\link{test_geocoord}} and \code{\link{get_country}},
#' \code{\link{get_country_shoreline}} and/or \code{\link{get_sea}} to assign
#' the geographic names, this function uses the output of those. It combines by
#' row the data frames with a description - geographic name or indication of
#' some problem with the coordinates (if that occurs) - and orders the data by
#' the same order as in the input text file or data frame (given by the user,
#' with the list of coordinates).
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item The output of \code{\link{get_country}} - \strong{'countries'} -,
#' \code{\link{get_country_shoreline}} - \strong{'countries_sh'} -,
#' \code{\link{get_sea}} - \strong{'seas'} and \strong{'miss_seas'} - and
#' \code{\link{get_lon180}} or \code{\link{test_geocoord}} -
#' \strong{'excl_coords'}. However, only the 'countries' data frame is
#' compulsory because all those data frames may not exist.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file with the coordinate pairs by the inicial order and the
#' respective geographic name. That file has the following header: \strong{id |
#' lon | lat | geo_name}. If a country or sea name could not be assigned because
#' of the polygon's data set lack of coverage, the description in the field
#' 'geo_name' will be 'MISSING_NAME'. If a name could not be assigned because
#' the coordinates are missing or out of bounds, the description will be
#' 'ERRONEOUS-MISSING_COORDS'.
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' ## First run
#' get_lon180(coords = ispd)
#' ## Or
#' test_geocoord(coords = eraclim_uao_fp)
#' ## Then run sequentially:
#' get_country(icoords = coords_ok)
#' get_country_shoreline(icoords = miss_countries, tol)
#' get_sea(icoords = miss_countries_sh)
#' ## And finally
#' ## For the ISPD case
#' order_data(countries = countries, countries_sh = countries_sh, seas = seas)
#' ## For the ERACLIM_UAO_FP
#' order_data(countries = countries, countries_sh = countries_sh, seas = seas,
#' excl_coords = excl_coords)
#' }
#'
#' @usage
#' order_data(countries, countries_sh = NULL, seas = NULL, miss_seas =
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
order_data <- function(countries, countries_sh = NULL, seas = NULL,
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
  # Output directory
  if (!file.exists("txt-order_data")) {
    dir.create("txt-order_data")
  }
  write.table(geo_name, file = "txt-order_data/geographic_names.txt",
    row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  cat("\n")
  cat("Please check the directory \\txt-order_data.\n\n")
}
