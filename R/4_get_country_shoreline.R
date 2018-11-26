#' Determines Country Names for Coordinate Points Located in the Shoreline.
#'
#' Determines country names for a list of coordinate points which are supposed
#' to be located on land but without country names assigned by the function
#' \code{\link{get_country}} because probably they are very close to the
#' shoreline and have lack of precision. Due to this fact when the user runs
#' \code{\link{get_sea}} over that points they get ocean/sea names assigned. The
#' present function uses a SpatialPolygonsDataFrame with 50 m precision and a
#' buffer zone around the coordinate points. The buffer represents the tolerance
#' and is given by the user. If the tolerance is 500 m, for example, and the
#' point gets a country name, means that it belongs to that country with an
#' error until 500 m towards the sea.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item The output of \code{\link{get_country}}, i.e., a data frame with three
#' columns: \strong{id | lon | lat}.
#' }
#' \strong{Output:}
#' \itemize{
#' \item Eventually, a text file with the points for which a country name was
#' determined. That file has the following header: \strong{id | lon | lat |
#' country | sovereignt | adm0_a3 | name_de | name_es | name_fr | name_pt}.
#' \item Eventually, a text file with the points missing country names.
#' \item A .RData file with the country names data frame - 'countries_sh' -, the
#' missing country names data frame - 'miss_countries_sh' - or the both.
#' 'miss_countries_sh' is the input of \code{\link{get_sea}} to determine the
#' remaining names.
#' }
#'
#' @seealso Requires \code{\link[raster]{buffer}}.
#'
#' @references
#' Made with Natural Earth. Free vector and raster map data @@
#' naturalearthdata.com.
#'
#' @examples
#' \dontrun{
#' ##
#' ## First run
#' get_lon180(coords_lon360)
#' ## Or
#' test_geocoord(coords_sample)
#' ## Then run sequentially
#' get_country(icoords)
#' get_country_shoreline(icoords, tol)
#' }
#'
#' @usage
#' get_country_shoreline(icoords, tol)
#'
#' @param icoords data frame with three columns: \strong{id | lon | lat} where:
#'   'id' is the row identifier for the coordinates in the original list of
#'   coordinates, 'lon' is the longitude in the range [-180, +180] and 'lat' is
#'   the latitude. Both coordinates are in decimal degrees.
#' @param tol is the tolerance in meters. By default, tol = 500 meters.
#'
#' @import sp
#' @import utils
#'
#' @export
#'
get_country_shoreline <- function(icoords, tol = 500) {
  coords <- icoords
  coords$id <- NULL
  load("countries_polys_50m.RData", envir = .GlobalEnv)
  cat("\n")
  cat("Processing Country names for points close to the shoreline...\n\n")
  gname_sp <- countries_polys_50m
  coords_sp <- sp::SpatialPoints(coords,
    proj4string = sp::CRS(sp::proj4string(gname_sp),
      doCheckCRSArgs = FALSE))
  # Creates a buffer of tol meters width
  coords_buf <- raster::buffer(coords_sp, width = tol, dissolve = FALSE)
  indices_sh <- sp::over(coords_buf, gname_sp)
  gname_sh <- data.frame(id = as.integer(icoords$id),
    lon = icoords$lon, lat = icoords$lat,
    country = as.character(indices_sh$NAME),
    sovereignt = as.character(indices_sh$SOVEREIGNT),
    adm0_a3 = as.character(indices_sh$ADM0_A3),
    name_de = as.character(indices_sh$NAME_DE),
    name_es = as.character(indices_sh$NAME_ES),
    name_fr = as.character(indices_sh$NAME_FR),
    name_pt = as.character(indices_sh$NAME_PT),
    stringsAsFactors = FALSE)
  countries_sh_na <- gname_sh[is.na(gname_sh$country), ]
  countries_sh <- gname_sh[!(gname_sh$id %in% countries_sh_na$id), ]
  report <- cbind(c("Coordinate pairs processed: ",
    "Country names returned: ",
    "Country names missing: "),
    c(nrow(icoords), nrow(countries_sh), nrow(countries_sh_na)))
  if (nrow(countries_sh) != 0 && nrow(countries_sh_na) == 0) {
    ctxt <- paste("countries_shoreline_", as.character(tol),  ".txt", sep = "")
    write.table(countries_sh, file = ctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    crdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(countries_sh, file = crdata)
    cat("\n")
    cat("Please check 'countries_shoreline_tol.txt' and \n")
    cat("'out_get_country_shoreline_tol.RData'.\n\n")
  }
  if (nrow(countries_sh) != 0 && nrow(countries_sh_na) != 0) {
    ctxt <- paste("countries_shoreline_", as.character(tol),  ".txt", sep = "")
    write.table(countries_sh, file = ctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    mctxt <- paste("missing_countries_shoreline_",
      as.character(tol), ".txt", sep = "")
    write.table(countries_sh_na, file = mctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    miss_countries_sh <- countries_sh_na[, 1:3, drop = FALSE]
    crdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(countries_sh, miss_countries_sh, file = crdata)
    cat("\n")
    cat("There are still unnamed points.\n")
    cat("Please, try with a greater tolerance or run get_sea() \n")
    cat("with 'miss_countries_sh' as input parameter. \n")
    cat("Please check 'countries_shoreline_tol.txt', \n")
    cat("'missing_countries_shoreline_tol.txt' and \n")
    cat("'out_get_country_shoreline_tol.RData'.\n\n")
  }
  if (nrow(countries_sh) == 0 && nrow(countries_sh_na) != 0) {
    mctxt <- paste("missing_countries_shoreline_",
      as.character(tol), ".txt", sep = "")
    write.table(countries_sh_na, file = mctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    miss_countries_sh <- countries_sh_na[, 1:3, drop = FALSE]
    mcrdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(miss_countries_sh, file = mcrdata)
    cat("\n")
    cat("Missing country names for all points.\n")
    cat("Please, try with a greater tolerance.\n")
    cat("Please check 'missing_countries_shoreline_tol.txt' and \n")
    cat("'out_get_country_shoreline_tol.RData'.\n\n")
  }
  rm("countries_polys_50m", envir = .GlobalEnv)
  return(as.table(report))
}
