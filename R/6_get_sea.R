#' Determines Sea Names for Points Located in a Marine Area.
#'
#' Given a list of geographic coordinates in marine areas of the world,
#' determines the geographical name of the sea, ocean, bay, gulf, fjord, etc.
#' were each one of the points is located. Uses a SpatialPolygonsDataFrame with
#' 10 m precision containing the polygons and sea names, which covers most of
#' the marine territories on Earth.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item The output of \code{\link{get_country}} - \strong{'miss_countries'} -,
#' or the output of \code{\link{get_country_shoreline}}, -
#' \strong{'miss_countries_sh'} -, i.e., a data frame with three columns:
#' \strong{id | lon | lat}.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file with the points for which a sea name was determined, if
#' they exist. That file has the following header: \strong{id | lon | lat |
#' sea}.
#' \item A text file with the missing sea names, if they exist.
#' \item A .RData file with the output data frame(s). The data frame
#' \strong{'seas'} has the header \strong{id | lon | lat | sea}. The eventual
#' data frame \strong{'miss_seas'} has the header \strong{id | lon | lat}.
#' }
#'
#' @references
#' Made with Natural Earth. Free vector and raster map data @@
#' naturalearthdata.com.
#'
#' @examples
#' \dontrun{
#' ## First run
#' get_lon180(coords = ispd)
#' ## Or
#' test_geocoord(coords = eraclim_uao_fp)
#' ## Then run sequentially
#' get_country(icoords = coords_ok)
#' get_country_shoreline(icoords = miss_countries, tol)
#' get_sea(icoords = miss_countries_sh)
#' }
#'
#' @usage
#' get_sea(icoords)
#'
#' @param icoords data frame with three columns: \strong{id | lon | lat} where:
#'   'id' is the row identifier for the coordinates in the original list of
#'   coordinates, 'lon' is the longitude in the range (-180, +180) and 'lat' is
#'   the latitude. Both coordinates are in decimal degrees.
#'
#' @import sp
#' @import utils
#'
#' @export
#'
get_sea <- function(icoords){
  # Data frame with the coordinates
  coords1 <- icoords
  coords1$id <- NULL
  # SpatialPolygonsDataFrame - 10 m
  if (file.exists("polys/seas_polys_10m.rda")) {
    load("polys/seas_polys_10m.rda", envir = .GlobalEnv)
  } else {
    seas_polys()
    load("polys/seas_polys_10m.rda", envir = .GlobalEnv)
  }
  cat("\n")
  cat("Processing Sea names...\n\n")
  # Uses a SpatialPolygonsDataFrame with marine areas polygons of 10 meters
  # precision
  gname_sp <- seas_polys_10m
  # Transforms the input geographic coordinates in a SpatialPointsDataFrame and
  # assigns it the same coordinate reference system (CRS) of the
  # SpatialPolygonsDataFrame
  coords_sp1 <- sp::SpatialPoints(coords1,
    proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
  # Spatial overlay of points and polygons
  indices1 <- sp::over(coords_sp1, gname_sp)
  # Data frame with coordinates ID, geographic coordinates and marine area
  # name if any was retrived
  gname1 <- data.frame(id = as.integer(icoords$id),
    lon = coords1$lon, lat = coords1$lat,
    sea = as.character(indices1$name),
    stringsAsFactors = FALSE)
  # Subsets the coordinate points without sea names assigned
  seas_na1 <- gname1[is.na(gname1$sea), ]
  # Subsets the coordinate points with sea names assigned
  seas1 <- gname1[!(gname1$id %in% seas_na1$id), ]
  # Output directory
  if (!file.exists("txt-get_sea")) {
    dir.create("txt-get_sea")
  }
  # Sea names are assigned to all points using the general names
  if (nrow(seas_na1) == 0) {
    report <- cbind(c("Coordinate pairs processed: ",
      "Sea names returned: ",
      "Sea names missing: "),
      c(nrow(gname1), nrow(seas1), nrow(seas_na1)))
    seas <- seas1
    write.table(seas, file = "txt-get_sea/seas.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    save(seas, file = "seas.RData")
    cat("The data frame 'seas' was saved as .RData in the working \n")
    cat("directory.\n\n")
    cat("Please check also the directory \\txt-get_sea.\n\n")
  # Case: Sea names are still missing
  } else {
    # Creates a data frame with only two columns from the unamed points data
    # frame
    # Assigns other sea names (most of them "Internal Waters")
    coords2 <- seas_na1[, 2:3, drop = FALSE]
    # Applying to remaining points the same proceeding as above...
    coords_sp2 <- sp::SpatialPoints(coords2,
      proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
    indices2 <- sp::over(coords_sp2, gname_sp)
    gname2 <- data.frame(id = seas_na1$id,
      lon = coords2$lon, lat = coords2$lat,
      sea = as.character(indices2$note),
      stringsAsFactors = FALSE)
    seas_na2 <- gname2[is.na(gname2$sea), ]
    seas2 <- gname2[!(gname2$id %in% seas_na2$id), ]
    seas <- rbind(seas1, seas2)
    report <- cbind(c("Coordinate pairs processed: ",
      "Sea names returned: ",
      "Sea names missing: "),
      c(nrow(gname1), nrow(seas), nrow(seas_na2)))
    write.table(seas, file = "txt-get_sea/seas.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    if (nrow(seas_na2) == 0) {
      save(seas, file = "seas.RData")
      cat("The data frame 'seas' was saved as .RData in the working \n")
      cat("directory.\n\n")
      cat("Please check also the directory \\txt-get_sea.\n\n")
    } else {
      write.table(seas_na2, file = "txt-get_sea/missing_seas.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      miss_seas <- seas_na2[, 1:3, drop = FALSE]
      save(seas, miss_seas, file = "out_get_sea.RData")
      cat("The data frames 'seas' and 'miss_seas' were saved into \n")
      cat("'out_get_sea.RData' in the working directory.\n\n")
      cat("Please check also the directory \\txt-get_sea.\n\n")
      cat("There are names missing.\n\n")
    }
  }
  rm("seas_polys_10m", envir = .GlobalEnv)
  return(as.table(report))
}
