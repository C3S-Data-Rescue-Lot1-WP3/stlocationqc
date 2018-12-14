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
#' \item The output of \code{\link{get_country}} - \strong{'miss_countries'} -,
#' i.e., a data frame with three columns: \strong{id | lon | lat}.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file with the points for which a country name was
#' determined, if they exist. That file has the following header: id | lon | lat |
#' country | sovereignt | adm0_a3 | name_de | name_es | name_fr | name_pt.
#' \item A text file with the missing country names, if they exist.
#'
#' \item A .RData file with the output data frame(s). The data frame
#' \strong{'countries_sh'} has the header \strong{id | lon | lat | country |
#' sovereignt | iso3 | subregion | continent}. The data frame
#' \strong{'miss_countries_sh'} has the header \strong{id | lon | lat} and it is
#' the input of \code{\link{get_sea}} to determine the sea name were those
#' points are located.
#' }
#'
#' @references
#' Made with Natural Earth. Free vector and raster map data @@
#' naturalearthdata.com.
#'
#' @examples
#' \dontrun{
#' ##
#' ## First run
#' get_lon180(coords = ispd)
#' ## Or
#' test_geocoord(coords = eraclim_uao_fp)
#' ## Then run sequentially
#' get_country(icoords = coords_ok)
#' get_country_shoreline(icoords = miss_countries, tol)
#' }
#'
#' @usage
#' get_country_shoreline(icoords, tol)
#'
#' @param icoords data frame with three columns: \strong{id | lon | lat} where:
#'   'id' is the row identifier for the coordinates in the original list of
#'   coordinates, 'lon' is the longitude in the range (-180, +180) and 'lat' is
#'   the latitude. Both coordinates are in decimal degrees.
#' @param tol is the tolerance in meters. By default, tol = 500 meters.
#'
#' @importFrom raster buffer
#' @import rgeos
#' @import sp
#' @import utils
#'
#' @export
#'
get_country_shoreline <- function(icoords, tol = 500) {
  coords <- icoords
  coords$id <- NULL
  if (file.exists("polys/countries_polys_50m.rda")) {
    load("polys/countries_polys_50m.rda", envir = .GlobalEnv)
  } else {
    countries_polys()
    load("polys/countries_polys_50m.rda", envir = .GlobalEnv)
  }
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
  # Output directory
  if (!file.exists("txt-get_country_shoreline")) {
    dir.create("txt-get_country_shoreline")
  }
  report <- cbind(c("Coordinate pairs processed: ",
    "Country names returned: ",
    "Country names missing: "),
    c(nrow(icoords), nrow(countries_sh), nrow(countries_sh_na)))
  if (nrow(countries_sh) != 0 && nrow(countries_sh_na) == 0) {
    ctxt <- paste("countries_shoreline_", as.character(tol),  ".txt", sep = "")
    folctxt <- paste("txt-get_country_shoreline", ctxt, sep = "/")
    write.table(countries_sh, file = folctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    crdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(countries_sh, file = crdata)
    cat("The data frame 'countries_sh' was saved into \n")
    cat("out_get_country_shoreline_tol.RData in the working directory.\n\n")
    cat("Please check also the directory \\txt-get_country_shoreline.\n\n")
  }
  if (nrow(countries_sh) != 0 && nrow(countries_sh_na) != 0) {
    ctxt <- paste("countries_shoreline_", as.character(tol),  ".txt", sep = "")
    folctxt <- paste("txt-get_country_shoreline", ctxt, sep = "/")
    write.table(countries_sh, file = folctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    mctxt <- paste("missing_countries_shoreline_",
      as.character(tol), ".txt", sep = "")
    folmctxt <- paste("txt-get_country_shoreline", mctxt, sep = "/")
    write.table(countries_sh_na, file = folmctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    miss_countries_sh <- countries_sh_na[, 1:3, drop = FALSE]
    crdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(countries_sh, miss_countries_sh, file = crdata)
    cat("The data frames 'countries_sh' and 'miss_countries_sh' were saved \n")
    cat("into out_get_country_shoreline_tol.RData in the working \n")
    cat("directory.\n\n")
    cat("Please check also the directory \\txt-get_country_shoreline.\n\n")
    cat("There are still unnamed points.\n")
    cat("Please, try with a greater tolerance or run get_sea() \n")
    cat("with 'miss_countries_sh' as input parameter. \n")
  }
  if (nrow(countries_sh) == 0 && nrow(countries_sh_na) != 0) {
    mctxt <- paste("missing_countries_shoreline_",
      as.character(tol), ".txt", sep = "")
    folmctxt <- paste("txt-get_country_shoreline", mctxt, sep = "/")
    write.table(countries_sh_na, file = folmctxt,
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    miss_countries_sh <- countries_sh_na[, 1:3, drop = FALSE]
    mcrdata <- paste("out_get_country_shoreline_", as.character(tol),
      ".RData", sep = "")
    save(miss_countries_sh, file = mcrdata)
    cat("The data frame 'miss_countries_sh' was saved into \n")
    cat("out_get_country_shoreline_tol.RData in the working directory.\n\n")
    cat("Please check also the directory \\txt-get_country_shoreline.\n\n")
    cat("Missing country names for all the given points.\n")
    cat("Please, try with a greater tolerance.\n")
  }
  rm("countries_polys_50m", envir = .GlobalEnv)
  return(as.table(report))
}
