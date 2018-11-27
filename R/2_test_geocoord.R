#' Tests the Geographic Coordinates.
#'
#' Given a list of geographic coordinates, tests if the longitude is in the
#' range [-180, +180] and if the latitude is in the range [-90, +90]. Also tests
#' the coordinates for missing values.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item A text file without header with the geographic coordinates in decimal
#' degrees: the longitude in the first column and the latitude in the
#' second column, separated by tabs. Missing values must be codified as
#' '-999.0'.
#' \item Or a data frame with that same format and the column names: \strong{lon
#' | lat}.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file named 'coords_ok' with three columns \strong{id | lon |
#' lat} where: 'id' is the row identifier for the coordinates in the original
#' list of coordinates, 'lon' is the longitude in the range [-180, +180] and
#' 'lat' is the latitude. Both coordinates are in decimal degrees.
#' \item If there are errors, the function writes a text file with the
#' coordinate pairs containing at least one coordinate out of bounds.
#' \item If there are missing coordinates, the function writes a text file with
#' the coordinate pairs containing at least one missing coordinate.
#' \item A .RData file  with the output data frame(s) which has/have the column
#' names: \strong{id | lon | lat}. The data frame 'coords_ok' can be used as
#' input parameter of \code{\link{get_country}} or \code{\link{get_sea}} to
#' determine the country or the sea names. Eventually is created the data frame
#' 'excl_coords' with the erroneous and/or missing coordinates.
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' test_geocoord(coords = coords_sample) # CRIAR O OBJECTO coords_sample!!!!
#' }
#'
#' @usage
#' ## If there are the coordinates data frame
#' test_geocoord(coords)
#' ##
#' ## If there are the coordinates text file
#' test_geocoord(coords = NULL)
#'
#' @param coords data frame with the geographic coordinates in decimal degrees,
#' the longitude in the first column, the latitude in the second column and
#' the column names: \strong{lon | lat}.
#'
#' @import stats
#' @import utils
#'
#' @export
#'
test_geocoord <- function(coords = NULL) {
  # Creates the data frame with the coordinates
  if (!is.null(coords)) {
    coords <- coords
  } else {
    cat("\n")
    cat("Please, choose the text file with the coordinates.\n")
    coords <- read.table(file.choose(), header = FALSE, sep = "\t", quote = "",
      col.names = c("lon", "lat"), na.strings = "-999",
      stringsAsFactors = FALSE)
  }
  # Data limits
  cat("\n")
  cat("Checking longitude and latitude...\n\n")
  print(str(coords))
  cat("\n")
  cat("Longitude limits:\n")
  print(range(coords$lon))
  cat("\n")
  cat("Latitude limits:\n")
  print(range(coords$lat))
  cat("\n")
  # Coordinates ID
  icoords <- coords
  icoords$id <- as.integer(row.names.data.frame(icoords))
  icoords <- icoords[c("id", "lon", "lat")]
  # Tests
  excl_coords <- data.frame()
  # Testing longitude [-180, +180] and latitude [-90, +90]
  erro_coords <- na.omit(icoords[icoords$lon < -180 | icoords$lon > 180 |
      icoords$lat < -90 | icoords$lat > 90, ])
  # Testing missing coordinates
  miss_coords <- icoords[is.na(icoords$lon) | is.na(icoords$lat), ]
  # Split
  # Excluded rows
  excl_coords <- unique(rbind(erro_coords, miss_coords))
  # Correct coordinate pairs
  coords_ok <- icoords[!(icoords$id %in% excl_coords$id), ]
  #
  if (nrow(excl_coords) == 0) {
    cat("Coordinates OK.\n\n")
  } else {
    cat("There are errors in the coordinates.\n\n")
  }
  # Output without errors
  if (nrow(excl_coords) == 0) {
    write.table(coords_ok, file = "coords_ok.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    save(coords_ok, file = "coords_ok.RData")
    cat("Please check, 'coords_ok.txt'.\n\n")
    cat("The data frame 'coords_ok' was saved as .RData in the working \n")
    cat("directory.\n\n")
    # Output with errors
  } else {
    # Correct coordinates
    write.table(coords_ok, file = "coords_ok.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    # Erroneous coordinates
    if (nrow(erro_coords) != 0) {
      write.table(erro_coords, file = "erroneous_coordinates.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      cat("Were found longitude and/or latitude values out of bounds.\n")
      cat("Please check 'erroneous_coordinates.txt'.\n\n")
    }
    # Missing coordinates
    if (nrow(miss_coords) != 0) {
      write.table(miss_coords, file = "missing_coordinates.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      cat("Were found missing coordinates.\n")
      cat("Please check 'missing_coordinates.txt'.\n\n")
    }
    save(coords_ok, excl_coords, file = "out_test_geocoord.RData")
    cat("Erroneous and/or missing coordinates have been excluded from the \n")
    cat("output 'coords_ok'.\n\n")
    cat("Please check, 'coords_ok.txt'.\n\n")
    cat("The data frames 'coords_ok' and 'excl_coords' were saved into \n")
    cat("'out_test_geocoord.RData' in the working directory.\n\n")
  }
}
