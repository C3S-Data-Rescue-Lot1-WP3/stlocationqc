#'
#' Transforms the Longitude from [0, 360] to [-180, +180].
#'
#' Given a list of geographic coordinates, test the coordinates and transforms
#' the longitude from [0, 360] to [-180, +180].
#'
#' \itemize{
#' \item The input is a .txt file with the geographic coordinates in decimal
#' degrees and the longitude in the range [0, 360]. The longitude must be in the
#' first column and the latitude in the second column, separated by tabs. The
#' header of the file must be 'longitude' and 'latitude', respectively.
#' \item The function does the following:
#' \enumerate{
#' \item Opens a window to choose the input file;
#' \item Tests if the longitude and latitude are out of bounds and writes a file
#' with the errors, if they exist;
#' \item Transforms the longitude into the range [-180, +180] and writes a file
#' with the coordinate pairs.
#' }
#' \item Output:
#' \enumerate{
#' \item A .txt file named 'err_coord-get_lon180.txt' with the values out of
#' bounds, if they exist;
#' \item A .txt file named 'coords2functions.txt' with the longitude in the
#' range [-180, +180] and the latitude, both in decimal degrees. The header is
#' 'longitude' and 'latitude', respectively. This file is the input of
#' \code{\link{get_country}} or \code{\link{get_sea}} to determine the country
#' or sea names.
#' }
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' ## A sample of coordinate pairs with the longitude in the range [0, 360],
#' ## extracted from the list of stations belonging to ISPD (International
#' ## Surface Pressure Databank).
#' ## Run the three lines of code below to see the example:
#' ##
#' eg_ispd <- coords_ispd_sample
#' get_lon180()
#' rm(eg_ispd)
#' }
#'
#' @export
get_lon180 <- function() {

  if (exists("eg_ispd")) {
    print("Running the example...", quote = FALSE)
    print("", quote = FALSE)
    coords <- eg_ispd
  } else {
    print("Load the .txt file with the coordinates.", quote = FALSE)
    print("", quote = FALSE)
    coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
  }

  print("Checking the data...", quote = FALSE)
  print("", quote = FALSE)
  print(str(coords))
  print("", quote = FALSE)
  if (nrow(coords) >= 10) {
    print("First 10 rows of the file:", quote = FALSE)
    print(head(coords, n = 10L))
    print("", quote = FALSE)
  }
  if (nrow(coords) >= 50) {
    print("Last 10 rows of the file:", quote = FALSE)
    print(tail(coords, n = 10L))
    print("", quote = FALSE)
  }

  x <- coords$longitude
  y <- coords$latitude
  coords_id <- row.names.data.frame(coords)
  err_coord <- data.frame()

  for (i in 1:nrow(coords)) {
    if (x[i] < 0 || x[i] > 360) {
      print("Was found a longitude value out of bounds.", quote = FALSE)
      print("It is recommended to fix it and run the function again.",
        quote = FALSE)
      print("Coordinates ID:", quote = FALSE)
      print(i)
      print("Value:", quote = FALSE)
      print(x[i])
      print("The longitude values out of bounds are processed anyway.",
        quote = FALSE)
      print("However, it is not possible to determine a geographical name.",
        quote = FALSE)
      print("", quote = FALSE)
      err_lon360 <- data.frame(id = coords_id[i], longitude = x[i], latitude = y[i])
      err_coord <- rbind(err_coord, err_lon360)
    }
    if (y[i] < -90 || y[i] > 90) {
      print("Was found a latitude value out of bounds.", quote = FALSE)
      print("It is recommended to fix it and run the function again.",
        quote = FALSE)
      print("Coordinates ID:", quote = FALSE)
      print(i)
      print("Value:", quote = FALSE)
      print(y[i])
      print("The latitude values out of bounds are processed anyway.",
        quote = FALSE)
      print("However, it is not possible to determine a geographical name.",
        quote = FALSE)
      print("", quote = FALSE)
      err_lat <- data.frame(id = coords_id[i], longitude = x[i], latitude = y[i])
      err_coord <- rbind(err_coord, err_lat)
    }
  }
  if (nrow(err_coord) != 0) {
  write.table(err_coord, file = "err_coord-get_lon180.txt", row.names = FALSE,
    col.names = TRUE, sep = "\t", quote = FALSE)
  print("Check the error file 'err_coord-get_lon180.txt'.", quote = FALSE)
  print("", quote = FALSE)
  }

  print("Original longitude limits:", quote = FALSE)
  print(range(coords$longitude))
  print("", quote = FALSE)
  print("Latitude limits:", quote = FALSE)
  print(range(coords$latitude))
  print("", quote = FALSE)

  lon180 <- c()
  for (i in 1:nrow(coords)) {
    if (x[i] > 180) {
      lon180[i] <- round(x[i] - 360, digits = 3)
    } else {
      lon180[i] <- x[i]
    }
  }

  print("New longitude limits:", quote = FALSE)
  print(range(lon180))
  print("", quote = FALSE)
  coords180 <- data.frame(longitude = lon180, latitude = coords$latitude)
  write.table(coords180, file = "coords2functions.txt", row.names = FALSE,
    col.names = TRUE, sep = "\t", quote = FALSE)
  print("A file with the longitudes in the range [-180, +180] named 'coords2function .txt' as been created.", quote = FALSE)
  print("You can run the functions 'get_country()' or 'get_sea()' now.",
    quote = FALSE)
  print("", quote = FALSE)
}
