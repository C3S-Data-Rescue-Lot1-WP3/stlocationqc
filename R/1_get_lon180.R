#' Tests the Geographic Coordinates and Transforms the Longitude from (0, 360)
#' to (-180, +180).
#'
#' Given a list of geographic coordinates, first tests the coordinates for out
#' of bounds values and missing values. Then transforms the longitude from (0,
#' 360) to (-180, +180) degrees, considering 6 significative digits.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item A \strong{text file} without header with the geographic coordinates in
#' decimal degrees: the longitude in the first column and the latitude in the
#' second column, separated by tabs (other columns could exist, however are
#' unnecessary for this function). Missing values must be codified as 'NA' in
#' all fields.
#' \item Or a \strong{data frame} with that same format and the column names:
#' \strong{lon | lat}.
#' }
#' \strong{Output:}
#' \itemize{
#' \item A text file named 'coords_lon180' with three columns id | lon | lat,
#' where: 'id' is the row identifier for the coordinates in the original list of
#' coordinates, 'lon' is the longitude in the range (-180, +180) and 'lat' is
#' the latitude. Both coordinates are in decimal degrees.
#' \item If there are errors, the function writes a text file with the
#' coordinate pairs containing at least one coordinate out of bounds.
#' \item If there are missing coordinates, the function writes a text file with
#' the coordinate pairs containing at least one missing coordinate.
#' \item A .RData file with the output data frame(s) that has/have the column
#' names: \strong{id | lon | lat}. The data frame \strong{'coords_lon180'} can
#' be used as input parameter of \code{\link{get_country}} to determine the
#' country names. Eventually is created the data frame \strong{'excl_coords'}
#' with the erroneous and/or missing coordinates.
#' }
#'
#' @examples
#' \dontrun{
#' get_lon180(coords = ispd)
#' }
#'
#' @usage
#' ## If there are the coordinates data frame
#' get_lon180(coords)
#' ##
#' ## If there are the coordinates text file
#' get_lon180(coords = NULL)
#'
#' @param coords data frame with the geographic coordinates in decimal degrees,
#'   the longitude in the first column, the latitude in the second column and
#'   the column names: \strong{lon | lat} (other columns can exist, however are
#'   unnecessary for this function).
#'
#' @import stats
#' @import utils
#'
#' @export
#'
get_lon180 <- function(coords = NULL) {
  # Creates the data frame with the coordinates
  if (!is.null(coords)) {
    coords <- coords[, 1:2, drop = FALSE]
  } else {
    cat("\n")
    cat("Please, choose the text file with the coordinates.\n")
    coords <- read.table(file.choose(), header = FALSE, sep = "\t", quote = "",
      stringsAsFactors = FALSE)
    coords <- coords[, 1:2, drop = FALSE]
    names(coords) <- c("lon", "lat")
    coords$lon <- as.numeric(coords$lon)
    coords$lat <- as.numeric(coords$lat)
  }
  # Data limits
  cat("\n")
  cat("Checking longitude and latitude...\n\n")
  print(str(coords))
  cat("\n")
  cat("Original longitude limits:\n")
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
  # Testing longitude [0, 360] and latitude [-90, +90]
  erro_coords <- na.omit(icoords[icoords$lon < 0 | icoords$lon > 360 |
      icoords$lat < -90 | icoords$lat > 90, ])
  # Testing missing coordinates
  miss_coords <- icoords[is.na(icoords$lon) | is.na(icoords$lat), ]
  # Split
  # Excluded rows
  excl_coords <- unique(rbind(erro_coords, miss_coords))
  # Correct coordinate pairs
  corr_coords <- icoords[!(icoords$id %in% excl_coords$id), ]
  #
  if (nrow(excl_coords) == 0) {
    cat("Coordinates OK.\n\n")
  } else {
    cat("There are errors in the coordinates.\n\n")
  }
  cat("Transforming longitude...\n\n")
  # Transforming the longitude to the range [-180, +180]
  coords_lon180 <- corr_coords
  for (i in 1:nrow(coords_lon180)) {
    # Greather or equal to 180
    if (coords_lon180$lon[i] >= 180) {
      coords_lon180$lon[i] <- round(coords_lon180$lon[i] - 360, digits = 6)
    }
  }
  # Calculated longitude limits
  cat("New longitude limits:\n")
  print(range(coords_lon180$lon))
  cat("\n")
  # Output directory
  if (!file.exists("txt-get_lon180")) {
    dir.create("txt-get_lon180")
  }
  # Output without errors
  if (nrow(excl_coords) == 0) {
    write.table(coords_lon180, file = "txt-get_lon180/coords_lon180.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    save(coords_lon180, file = "coords_lon180.RData")
    cat("The data frame 'coords_lon180' was saved as .RData in the working \n")
    cat("directory.\n\n")
    cat("Please check also the directory \\txt-get_lon180.\n\n")
  # Output with errors
  } else {
    # Correct and transformed coordinates
    write.table(coords_lon180, file = "txt-get_lon180/coords_lon180.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    # Erroneous coordinates
    if (nrow(erro_coords) != 0) {
      write.table(erro_coords,
        file = "txt-get_lon180/erroneous_coordinates.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      cat("Were found longitude and/or latitude values out of bounds.\n\n")
    }
    # Missing coordinates
    if (nrow(miss_coords) != 0) {
      write.table(miss_coords, file = "txt-get_lon180/missing_coordinates.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      cat("Were found missing coordinates.\n\n")
    }
    save(coords_lon180, excl_coords, file = "out_get_lon180.RData")
    cat("Erroneous and/or missing coordinates have been excluded from the \n")
    cat("output with the transformed longitude.\n\n")
    cat("The data frames 'coords_lon180' and 'excl_coords' were saved into \n")
    cat("'out_get_lon180.RData' in the working directory.\n\n")
    cat("Please check also the directory \\txt-get_lon180.\n\n")
  }
}
