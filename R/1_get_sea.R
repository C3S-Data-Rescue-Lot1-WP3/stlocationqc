#'
#' Determines Sea Names for Points Located in the Sea.
#'
#' Given a list of geographic coordinates in the sea, determines the Exclusive
#' Economic Zone (EEZ) name or the International Hydrographic Organization (IHO)
#' Sea name for each coordinate point.
#'
#' @details
#' \itemize{
#' \item The input is a .txt file with the geographic coordinates in decimal
#' degrees and the longitude in the range [-180, +180]. The longitude must be in
#' the first column and the latitude in the second column, separated by tabs.
#' The header of the file must be 'longitude' and 'latitude', respectively.
#' \item The function does the following:
#' \enumerate{
#' \item If exists 'countries_na.txt' created by \code{\link{get_country}},
#' automatically loads and reads the coordinates file;
#' \item Else if it is assumed that all the points are in the sea, opens a
#' window to choose the input file which must be formatted according to the
#' instructions above. In this case, tests if the longitude and latitude are out
#' of bounds and writes a file with the errors, if they exist;
#' \item Preserves or creates the 'id', necessary to display the coordinates in
#' the initial order at the end of the process;
#' \item Uses \code{\link[mregions]{mr_shp}} to access the Marineregions.org Web
#' Feature Service and get the shapefile with the polygons of the Exclusive
#' Economic Zones (EEZ) and the International Hydrographic Organization (IHO)
#' Seas, which covers most of marine regions on Earth;
#' \item Writes a file with points and their sea names and another file with
#' points that remain unnamed (if they exist). Also writes a report about the
#' points processed.
#' }
#' \item Output:
#' \enumerate{
#' \item A .txt file named 'err_coord-get_sea.txt' with the coordinates out of
#' bounds, if they exist;
#' \item A .txt file named 'seas.txt'. The header is: 'id' 'longitude' 'latitude'
#' 'eez'/'eez_iho';
#' \item A .txt file named 'seas_na.txt' with the points unnamed, if they exist.
#' This file is the input of \code{\link{get_country_sea}} to determine the
#' remaining country or sea names;
#' \item A .txt file named 'report_get_sea.txt'.
#' }
#' }
#'
#' @seealso Requires \code{\link[mregions]{mr_shp}},
#'   \code{\link[sp]{SpatialPoints}}, \code{\link[sp]{CRS-class}} and
#'   \code{\link[sp]{over}} to determine the country names.
#'
#' @references VLIZ (2012). Intersect of IHO Sea Areas and Exclusive Economic
#'   Zones (version 2). Available online at http://www.marineregions.org/.
#'   Consulted on 2017-11-15.
#'
#' @examples
#' \dontrun{
#' ##
#' ## Execute one of the examples of \code{\link{get_country}} and then run the
#' ## line below:
#' get_sea()
#' ##
#' ## About the sea names:
#' ## Considering the Portuguese territory, if the point is in the Portuguese
#' ## Continental Shelf, attributes the name 'Portuguese Exclusive Economic
#' ## Zone'. If the point is not part of a continental shelf attributes the
#' ## name of the sea /ocean (e.g. 'North Atlantic Ocean').
#' }
#'
#' @export
get_sea <- function() {
  if (file.exists("countries_na.txt")) {
    coords <- read.table("countries_na.txt", header = TRUE, sep = "\t", quote = "")
    coords_id <- coords$id
    # Remove the 'id' and the missing values column
    coords$id <- NULL
    coords$country <- NULL
  } else {
    print("Choose the .txt file.", quote = FALSE)
    coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")

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
      write.table(err_coord, file = "err_coord-get_sea.txt",
        row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      print("Check the error file 'err_coord-get_sea.txt'.", quote = FALSE)
      print("", quote = FALSE)
    }

    print("Original longitude limits:", quote = FALSE)
    print(range(coords$longitude))
    print("", quote = FALSE)
    print("Latitude limits:", quote = FALSE)
    print(range(coords$latitude))
    print("", quote = FALSE)

  }

  print("Processing EEZ names...", quote = FALSE)
  print("", quote = FALSE)
  eez_iho <- mregions::mr_shp(key = "MarineRegions:eez_iho_union_v2", maxFeatures = 1000)
  nec_fields <- c("mrgid", "iho_sea", "eez", "country")
  eez_iho_subset <- eez_iho[ ,nec_fields]
  names(eez_iho_subset) <- nec_fields
  gname_sp <- eez_iho_subset
  # 'coords' is a data.frame with only two columns (longitude and latitude)
  coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
  indices <- sp::over(coords_sp, gname_sp)
  # First get the EEZ names
  gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, eez = indices$eez)
  eezs <- na.omit(gname)

  eezs_na <- gname[is.na(gname$eez), ]

  if (nrow(eezs_na) == 0) {
    write.table(eezs, file = "seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    report <- cbind(c("Coordinate pairs processed: ", "EEZ names returned: ", "EEZ names missing: "),
        c(nrow(gname), nrow(eezs), nrow(eezs_na)))
    write.table(report, file = "report_get_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t",
        quote = FALSE)
    print("", quote = FALSE)
    print("Check 'seas.txt' and 'report_get_sea.txt'.", quote = FALSE)
    print("", quote = FALSE)
    return(report)

  } else {

    print("Processing IHO Sea names...", quote = FALSE)
    print("", quote = FALSE)
    coords2_id <- eezs_na$id
    eezs_na$id <- NULL
    eezs_na$eez <- NULL
    coords2_sp <- sp::SpatialPoints(eezs_na, proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
    indices2 <- sp::over(coords2_sp, gname_sp)
    # If there are points unnamed, then get the IHO Sea names
    gname2 <- data.frame(id = coords2_id, longitude = eezs_na$longitude, latitude = eezs_na$latitude, iho_sea = indices2$iho_sea)
    ihos <- na.omit(gname2)
    ihos_na <- gname2[is.na(gname2$iho_sea), ]

    names(eezs)[4] <- "eez_iho"
    names(ihos)[4] <- "eez_iho"
    eezs_ihos <- rbind(eezs, ihos)
    write.table(eezs_ihos, file = "seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

    rep_eez <- cbind(c("Coordinate pairs processed: ", "EEZ names returned: ", "EEZ names missing: "), c(nrow(gname), nrow(eezs), nrow(eezs_na)))
    rep_iho <- cbind(c("Coordinate pairs processed: ", "IHO Sea names returned: ", "IHO Sea names missing: "), c(nrow(gname2), nrow(ihos), nrow(ihos_na)))
    rep_tot <- cbind(c("Total of coordinate pairs processed: ", "EEZ and IHO Sea names returned: ", "EEZ and IHO Sea names missing: "), c(nrow(gname), nrow(eezs) + nrow(ihos), nrow(ihos_na)))
    report <- rbind(rep_eez, rep_iho, rep_tot)
    write.table(report, file = "report_get_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

    print("Check 'seas.txt' and 'report_get_sea.txt'.", quote = FALSE)
    print("", quote = FALSE)

    if (nrow(ihos_na) != 0){
      write.table(ihos_na, file = "seas_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      print("Check 'seas_na.txt'.", quote = FALSE)
      print("There are sea names missing, please run the function 'get_country_sea()'.", quote = FALSE)
      print("", quote = FALSE)
    }

    # Plotar os NA sobre a controrno dos países
    # Fazer uma função para plotar que é chamada pelas outras

    return(report)
  }
}
