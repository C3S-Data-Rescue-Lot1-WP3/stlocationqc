#' Determines Country Names for Coordinate Points Located in the Continent.
#'
#' Given a list of geographic coordinates in the continent, determines the
#' country name for each coordinate point. First uses a SpatialPolygonsDataFrame
#' with 10 m precision to calculate the country names. Then if there are still
#' points without a name assigned, uses a SpatialPolygonsDataFrame with 50 m
#' precision, thus trying to calculate the name for less accurate points.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item A data frame with three columns \strong{id | lon | lat}, i.e., the
#' output of \code{\link{test_geocoord}} - 'coords_ok' -, or the output of
#' \code{\link{get_lon180}} - 'coords_lon180'. To use the present function it is
#' necessary that the coordinates do not have any errors or missing values. This
#' requires previous running of \code{\link{test_geocoord}} or
#' \code{\link{get_lon180}} that in addition to testing the coordinates, creates
#' the 'id' which is the row identifier for the coordinates in the original list
#' of coordinates. After running sequentially \code{\link{get_country}},
#' \code{\link{get_country_shoreline}} and \code{\link{get_sea}} to assign the
#' geographic names, the 'id' is necessary to display the coordinates in the
#' initial order at the end of the process (which consists of a sucessive
#' elimination of missing names), with or without all the names assigned.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file with all the points for which a country name was
#' determined. That file has the following header: \strong{id | lon | lat |
#' country | sovereignt | adm0_a3 | name_de | name_es | name_fr | name_pt}.
#' \item A text file with the missing country names, if they exist.
#' \item A .RData file  with the output data frame(s). The data frame
#' 'countries' as the header \strong{id | lon | lat | country | sovereignt |
#' iso3 | subregion | continent}. The data frame 'miss_countries' as the header
#' \strong{id | lon | lat} and is the input of
#' \code{\link{get_country_shoreline}} to determine the missing country names or
#' the input of \code{\link{get_sea}}, if the user wants to verify if the still
#' unamed points fall into to the sea and calculate the sea name were they are
#' located.
#' }
#'
#' @references
#' Made with Natural Earth. Free vector and raster map data @@
#' naturalearthdata.com.
#'
#' @examples
#' \dontrun{
#' ##
#' ## Example:
#' ## A sample of coordinate pairs with the longitude in the range [-180, +180],
#' ## extracted from the list of meteorological stations belonging to
#' ## ISPD - International Surface Pressure Databank, ERACLIM upper-air
#' ## inventory and MEDARE - MEditerranean climate DAta REscue
#' ##
#' ## First run
#' get_lon180(coords_lon360)
#' ## Or
#' test_geocoord(coords_sample)
#' ## Then run
#' get_country(icoords)
#' }
#'
#' @usage
#' get_country(icoords)
#'
#' @param icoords data frame with three columns: \strong{id | lon | lat} where:
#'   'id' is the row identifier for the coordinates in the original list of
#'   coordinates, 'lon' is the longitude in the range [-180, +180] and 'lat' is
#'   the latitude. Both coordinates are in decimal degrees.
#'
#' @import sp
#' @import utils
#'
#' @export
#'
get_country <- function(icoords = NULL) {
  # Data frame with the coordinates
  coords <- icoords
  coords$id <- NULL
  # SpatialPolygonsDataFrames - 10 m and 50 m
  load("countries_polys_10m.RData", envir = .GlobalEnv)
  load("countries_polys_50m.RData", envir = .GlobalEnv)
  cat("\n")
  cat("Processing Country names...\n\n")
  gname_sp10 <- countries_polys_10m
  # Transforms the input geographic coordinates into a SpatialPointsDataFrame
  # and assigns it the same coordinate reference system (CRS) of the
  # SpatialPolygonsDataFrame
  coords_sp10 <- sp::SpatialPoints(coords,
    proj4string = sp::CRS(sp::proj4string(gname_sp10), doCheckCRSArgs = FALSE))
  # Spatial overlay of points and polygons
  indices10 <- sp::over(coords_sp10, gname_sp10)
  # Creates a data frame with the names of interest
  gname10 <- data.frame(id = as.integer(icoords$id),
    lon = coords$lon, lat = coords$lat,
    country = as.character(indices10$ADMIN),
    sovereignt = as.character(indices10$SOVEREIGNT),
    adm0_a3 = as.character(indices10$ADM0_A3),
    name_de = as.character(indices10$NAME_DE),
    name_es = as.character(indices10$NAME_ES),
    name_fr = as.character(indices10$NAME_FR),
    name_pt = as.character(indices10$NAME_PT),
    stringsAsFactors = FALSE)
  # Subsets the coordinate points without country names assigned
  countries_na10 <- gname10[is.na(gname10$country), ]
  # Subsets the coordinate points with country names assigned
  countries10 <- gname10[!(gname10$id %in% countries_na10$id), ]
  # Country names are assigned to all points using the shapefile with 10 m
  # precision
  if (nrow(countries_na10) == 0) {
    write.table(countries10, file = "countries.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    countries <- countries10
    save(countries, file = "countries.RData")
    report <- cbind(c("Coordinate pairs processed: ",
      "Country names returned: ",
      "Country names missing: "),
      c(nrow(gname10), nrow(countries10), nrow(countries_na10)))
    cat("Please, check 'countries.txt' and 'countries.RData'.\n\n")
  # Case: country names are missing after using the shapefile with 10 m
  # precision
  } else {
    # Trying to get more country names for points with less precision. Uses the
    # SpatialPolygonsDataFrame with country polygons of 50 m precision
    gname_sp50 <- countries_polys_50m
    # Creates a data frame with only two columns from the unnamed points data
    # frame
    coords_na <- data.frame(lon = countries_na10$lon,
      lat = countries_na10$lat)
    # Applying the same functions as above...
    coords_sp50 <- sp::SpatialPoints(coords_na,
      proj4string = sp::CRS(sp::proj4string(gname_sp50),
      doCheckCRSArgs = FALSE))
    indices50 <- sp::over(coords_sp50, gname_sp50)
    gname50 <- data.frame(id = countries_na10$id,
      lon = coords_na$lon, lat = coords_na$lat,
      country = as.character(indices50$ADMIN),
      sovereignt = as.character(indices50$SOVEREIGNT),
      adm0_a3 = as.character(indices50$ADM0_A3),
      name_de = as.character(indices50$NAME_DE),
      name_es = as.character(indices50$NAME_ES),
      name_fr = as.character(indices50$NAME_FR),
      name_pt = as.character(indices50$NAME_PT),
      stringsAsFactors = FALSE)
    countries_na50 <- gname50[is.na(gname50$country), ]
    countries50 <- gname50[!(gname50$id %in% countries_na50$id), ]
    # Data frame that contains all the points with country names assignd
    countries <- rbind(countries10, countries50)
    write.table(countries, file = "countries.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    report <- cbind(c("Coordinate pairs processed: ",
      "Country names returned: ",
      "Country names missing: "),
      c(nrow(gname10), nrow(countries), nrow(countries_na50)))
    if (nrow(countries_na50) == 0) {
      save(countries, file = "countries.RData")
      cat("Please check 'countries.txt' and 'countries.RData'.\n\n")
    } else {
      # Case: country names still missing after using the shapefile with 50 m
      # precision
      write.table(countries_na50, file = "missing_countries.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      miss_countries <- countries_na50[, 1:3, drop = FALSE]
      save(countries, miss_countries, file = "out_get_country.RData")
      cat("Please check 'countries.txt', 'missing_countries.txt' and \n")
      cat("'out_get_country.RData'.\n")
      cat("There are names missing, please run the function \n")
      cat("'get_country_shoreline()' or 'get_sea()' with the data frame \n")
      cat("'miss_countries' as input parameter.\n\n")
    }
  }
  # Removing the big objects
  rm("countries_polys_10m", envir = .GlobalEnv)
  rm("countries_polys_50m", envir = .GlobalEnv)
  return(as.table(report))
}
