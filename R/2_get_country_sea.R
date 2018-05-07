#'
#' Determines Geographic Names for other Points Located in the Continent or in
#' the Sea.
#'
#' Determines the  country names for points in the shoreline and the EEZ names
#' for other points in a marine region.
#'
#' @details
#' \itemize{
#' \item The input is the file 'marine_regions_na.txt', created by
#' \code{\link{get_sea}}, which contains the missing name points that left.
#' Those points are located in the boundaries of continent with marine regions
#' and in marine regions not covered by the polygons of the shapefile used in
#' \code{\link{get_sea}}.
#' \item The function does the following:
#' \enumerate{
#' \item If exists 'seas_na.txt' created by get_sea, automatically loads and
#' reads the coordinates file;
#' \item Preserves the 'id' assigned to the coordinates in
#' \code{\link{get_country}} or in \code{\link{get_sea}}, necessary to display
#' the coordinates in the initial order at the end of the process;
#' \item Uses \code{\link[mregions]{mr_shp}} to access the Marineregions.org Web
#' Feature Service and get: 1. The shapefile with the polygons resulting from
#' the union of EEZ with respective countries; 2. The shapefile with the EEZ
#' polygons;
#' \item Writes a file with points and their country and/or sea names and
#' another file with points that remain unnamed (if they exist). Also writes a
#' report about the points processed;
#' \item Assigns the designation "NO_NAME" to the points that eventually remain
#' unnamed.
#' }
#' \item Output:
#' \enumerate{
#' \item A .txt file named 'countries_seas.txt'. The header is: 'id' 'longitude'
#' 'latitude' 'country'/'country_eez'. The points without a name that eventualy
#' can occur after running the function, are integrated in that file with the
#' designation 'NO_NAME';
#' \item A .txt file named 'countries_seas_na.txt' with the points unnamed, if
#' they exist;
#' \item A .txt file named 'report_get_country_sea.txt'.
#' }
#' }
#'
#' @seealso Requires \code{\link[mregions]{mr_shp}},
#'   \code{\link[sp]{SpatialPoints}}, \code{\link[sp]{CRS-class}} and
#'   \code{\link[sp]{over}} to determine the country names.
#'
#' @references Flanders Marine Institute (2016). Maritime Boundaries
#'   Geodatabase, version 1. Available online at http://www.marineregions.org/.
#'   Consulted on 2017-11-15.
#'
#'   VLIZ (2014). Union of the ESRI Country shapefile and the Exclusive Economic
#'   Zones (version 2). Available online at http://www.marineregions.org/.
#'   Consulted on 2017-11-15.
#'
#' @examples
#' \dontrun{
#' ##
#' ## Execute one of the examples of \code{\link{get_country}} and then run the
#' ## two functions below sequentially:
#' get_sea()
#' get_country_sea()
#' }
#'
#' @export
get_country_sea <- function() {
  if (file.exists("seas_na.txt")) {
    coords <- read.table("seas_na.txt", header = TRUE, sep = "\t", quote = "")
    coords_id <- coords$id
    coords$id <- NULL
    coords$iho_sea <- NULL
    print("", quote = FALSE)
    print("Processing country names for points in the shoreline...", quote = FALSE)
    print("", quote = FALSE)

    eez_land <- mregions::mr_shp(key = "MarineRegions:eez_land", maxFeatures = 300)
    nec_fields <- c("objectid","iso_3digit","country")
    eez_land_subset <- eez_land[ ,nec_fields]
    names(eez_land_subset) <- nec_fields
    gname_sp <- eez_land_subset

    coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(sp::proj4string(gname_sp), doCheckCRSArgs = FALSE))
    indices <- sp::over(coords_sp, gname_sp)
    gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, country = indices$country)
    countries2 <- na.omit(gname)
    countries2_na <- gname[is.na(gname$country), ]

    if (nrow(countries2_na) == 0) {
      write.table(countries2, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      rep_coun <- cbind(c("Coordinate pairs processed: ", "Country names (for points in the shoreline) returned: ", "Country names (for points in the shoreline) missing: "), c(nrow(gname), nrow(countries2), nrow(countries2_na)))
      write.table(rep_coun, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
      print("", quote = FALSE)
      print("Check 'countries_seas.txt' and 'report_get_country_sea.txt'.", quote = FALSE)
      print("Please run 'order_data()' to restore the inicial order of the coordinates.", quote = FALSE)
      print("", quote = FALSE)
      return(rep_coun)

    } else {

      print("Processing the remaining EEZ names...", quote = FALSE)
      print("", quote = FALSE)
      coords2_id <- countries2_na$id
      countries2_na$id <- NULL
      countries2_na$country <- NULL

      eez <- mregions::mr_shp(key = "MarineRegions:eez", maxFeatures = 300)
      nec_fields2 <- c("mrgid", "geoname")
      eez_subset <- eez[ ,nec_fields2]
      names(eez_subset) <- c("mrgid", "eez")
      gname2_sp <- eez_subset
      coords2_sp <- sp::SpatialPoints(countries2_na, proj4string = sp::CRS(sp::proj4string(gname2_sp), doCheckCRSArgs = FALSE))
      indices2 <- sp::over(coords2_sp, gname2_sp)
      gname2 <- data.frame(id = coords2_id, longitude = countries2_na$longitude, latitude = countries2_na$latitude, eez = indices2$eez)
      eezs2 <- na.omit(gname2)
      eezs2_na <- gname2[is.na(gname2$eez), ]

      names(countries2)[4] <- "country_eez"
      names(eezs2)[4] <- "country_eez"
      countries_eezs <- rbind(countries2, eezs2)

      if (nrow(eezs2_na) == 0){
        write.table(countries_eezs, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        rep_coun <- cbind(c("Coordinate pairs processed: ", "Country names (for points in the shoreline) returned: ", "Country names (for points in the shoreline) missing: "), c(nrow(gname), nrow(countries2), nrow(countries2_na)))
        rep_eez <- cbind(c("Coordinate pairs processed: ", "EEZ (remaining) names returned: ", "EEZ (remaining) names missing: "), c(nrow(gname2), nrow(eezs2), nrow(eezs2_na)))
        rep_tot <- cbind(c("Total of coordinate pairs processed: ", "Country and EEZ names returned: ", "Country and EEZ names missing: "), c(nrow(gname), nrow(countries2) + nrow(eezs2), nrow(eezs2_na)))
        report <- rbind(rep_coun, rep_eez, rep_tot)
        write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
        print("Check 'countries_seas.txt' and 'report_get_country_sea.txt'.", quote = FALSE)
        print("", quote = FALSE)

      } else {
        names(eezs2_na)[4] <- "country_eez"
        eezs2_na$country_eez <- "NO_NAME"
        countries_eezs_all <- rbind(countries_eezs, eezs2_na)
        write.table(countries_eezs_all, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        rep_coun <- cbind(c("Coordinate pairs processed: ", "Country names (for points in the shoreline) returned: ", "Country names (for points in the shoreline) missing: "), c(nrow(gname), nrow(countries2), nrow(countries2_na)))
        rep_eez <- cbind(c("Coordinate pairs processed: ", "EEZ (remaining) names returned: ", "EEZ (remaining) names missing: "), c(nrow(gname2), nrow(eezs2), nrow(eezs2_na)))
        rep_tot <- cbind(c("Total of coordinate pairs processed: ", "Country and EEZ names returned: ", "Country and EEZ names missing: "), c(nrow(gname), nrow(countries2) + nrow(eezs2), nrow(eezs2_na)))
        report <- rbind(rep_coun, rep_eez, rep_tot)
        write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
        write.table(eezs2_na, file = "countries_seas_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        print("", quote = FALSE)
        print("Check 'countries_seas.txt' and 'report_get_country_sea.txt'.", quote = FALSE)
        print("There are sea names missing, please  check 'countries_seas_na.txt.", quote = FALSE)
        # Plotar os NA sobre a controrno dos países
        # Fazer uma função para plotar que é chamada pelas outras
      }
      print("Please run 'order_data()' to restore the inicial order of the coordinates.")
      print("", quote = FALSE)
      return(report)
    }
  } else {
    print("", quote = FALSE)
    print("Missing the input file 'seas_na.txt'.", quote = FALSE)
  }
}
