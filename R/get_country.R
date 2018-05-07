#'
#' Determines Country Names for Points Located in the Continent.
#'
#' Given a list of geographic coordinates in the continent, determines the
#' country name for each coordinate point.
#'
#' @details
#' \itemize{
#' \item The input is a .txt file with the geographic coordinates in decimal
#' degrees and the longitude in the range [-180, +180]. The longitude must be in
#' the first column and the latitude in the second column, separated by tabs.
#' The header of the file must be 'longitude' and 'latitude', respectively.
#' \item Note that this function was tested for a maximum value of 84.000 pairs
#' of coordinates. Loading larger files will likely cause a long processing time
#' or memory problems.
#' \item The function does the following:
#' \enumerate{
#' \item Reads the output file of \code{\link{get_lon180}} function
#' ('coords2functions.txt'), if exists;
#' \item Else if, the longitude is already in the range [-180, +180], opens a
#' window to choose the input file which must be formatted according to the
#' instructions above;
#' \item Creates an 'id' with the row number, which consists of an identifier to
#' the coordinates: the 'id' is necessary to display the coordinates in the
#' initial order at the end of the process;
#' \item Tests if the longitude and latitude are out of bounds and writes a file
#' with the errors, if they exist;
#' \item Uses a SpatialPolygonsDataFrame with the polygons and names of
#' administrative regions. Uses functions from sp package to handle the
#' SpatialPolygonsDataFrame and determine the country names;
#' \item Writes a file with points and their administrative region names and
#' another file with points that remain unnamed (if they exist). Also writes a
#' report about the points processed.
#' }
#' \item Output:
#' \enumerate{
#' \item A .txt file named 'err_coord-get_country.txt' with the coordinates out
#' of bounds, if they exist;
#' \item A .txt file named 'countries.txt'. The header is: 'id'  'longitude'
#' 'latitude'  'country';
#' \item A .txt file named 'countries_na.txt' with the points unnamed, if they
#' exist. This file is the input of \code{\link{get_sea}} to determine the sea
#' names;
#' \item A .txt file named 'report_get_country.txt'.
#' }
#' }
#'
#' @seealso Requires \code{\link[sp]{SpatialPoints}},
#'   \code{\link[sp]{CRS-class}}, and \code{\link[sp]{over}} to determine the
#'   country names.
#'
#' @references 1:10m Cultural Vectors: Admin 0 - Countries. Made with Natural
#'   Earth. Free vector and raster map data @@ naturalearthdata.com. Consulted
#'   on 2017-11-15.
#'
#' @examples
#' \dontrun{
#' ##
#' ## Example 1:
#' ## A sample of coordinate pairs with the longitude in the range [-180, +180],
#' ## extracted from the list of stations belonging to ERACLIM upper-air
#' ## inventory.
#' ## Run the three lines of code below to see the example:
#' ##
#' eg_upperair <- coords_upperair_sample
#' get_country()
#' rm(eg_upperair)
#' ##
#' ## Example 2:
#' ## A sample of coordinate pairs with the longitude in the range [-180, +180],
#' ## extracted from the list of stations belonging to MEDARE.
#' ## Run the three lines of code below to see the example:
#' ##
#' eg_medare <- coords_medare_sample
#' get_country()
#' rm(eg_medare)
#' ##
#' ## About the definition of 'country name':
#' ## Considering the Portuguese territory, if the point is in Continental
#' ## Portugal, attributes the name 'Portugal'. If the point is in the
#' ## Portuguese islands Azores or Madeira, which are the two Autonomous
#' ## Regions of the Portuguese Sovereign, attributes the name 'Azores' and
#' ## 'Madeira', respectively.
#' }
#'
#' @export
get_country <- function() {

  if (exists("eg_upperair")) {
    print("Running the Example 1...", quote = FALSE)
    print("", quote = FALSE)
    coords <- eg_upperair
  } else if (exists("eg_medare")) {
    print("Running the Example 2...", quote = FALSE)
    print("", quote = FALSE)
    coords <- eg_medare
  } else if (file.exists("coords2functions.txt")) {
    print("Processing the output of get_lon180()...", quote = FALSE)
    print("", quote = FALSE)
    coords <- read.table("coords2functions.txt", header = TRUE, sep = "\t",
      quote = "")
  } else {
    print("Load the .txt file with the coordinates.", quote = FALSE)
    print("", quote = FALSE)
    coords0 <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
    if("country" %in% colnames(coords0))
    {
      coords0$country <- NULL
    }
    coords <- coords0
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
    if (x[i] < -180 || x[i] > 180) {
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
      err_lon180 <- data.frame(id = coords_id[i], longitude = x[i], latitude = y[i])
      err_coord <- rbind(err_coord, err_lon180)
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
    write.table(err_coord, file = "err_coord-get_country.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  print("Check the error file 'err_coord-get_country.txt'.", quote = FALSE)
  print("", quote = FALSE)
  }

  print("Original longitude limits:", quote = FALSE)
  print(range(coords$longitude))
  print("", quote = FALSE)
  print("Latitude limits:", quote = FALSE)
  print(range(coords$latitude))
  print("", quote = FALSE)


  print("Processing Country names...", quote = FALSE)
  print("", quote = FALSE)
  gname_sp <- data_countries
  coords_sp <- sp::SpatialPoints(coords,
    proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
  indices <- sp::over(coords_sp, gname_sp)
  gname <- data.frame(id = coords_id, longitude = coords$longitude,
    latitude = coords$latitude, country = indices$admin_region)
  countries <- na.omit(gname)
  write.table(countries, file = "countries.txt", row.names = FALSE,
    col.names = TRUE, sep = "\t", quote = FALSE)
  countries_na <- gname[is.na(gname$country), ]
  if (nrow(countries_na) != 0) {
    write.table(countries_na, file = "countries_na.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
  }
  report <- cbind(c("Coordinate pairs processed: ",
    "Country names returned: ",
    "Country names missing: "),
    c(nrow(gname), nrow(countries), nrow(countries_na)))
  write.table(report, file = "report_get_country.txt", row.names = FALSE,
    col.names = FALSE, sep = "\t", quote = FALSE)

  print("Check 'countries.txt', 'countries_na.txt' and 'report_get_country.txt'.",
    quote = FALSE)
  print("If there are country names missing, please run the function 'get_sea()'.",
    quote = FALSE)
  print("", quote = FALSE)
  return(report)
}
