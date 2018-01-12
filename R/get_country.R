#'
#' Determines the country names for coordinate points
#'
#' Given a pair of geographic coordinates in the continent, determines the
#' country name.
#'
#' @details Input: A text file with the coordinates in decimal degrees and a
#'   shapefile with the country names.
#'
#'   The .txt file has to be in the following format: with the longitude in the
#'   first column and the latitude in the second column, separated by tabs.The
#'   header of the file must be 'longitude' and 'latitude', respectively and
#'   longitude must be in the range [-180, +180].
#'
#'   If exists <coords2getf.txt> created by \code{\link{get_lon180}},
#'   automatically loads and reads the coordinates file. If wasn't necessary to
#'   use \code{\link{get_lon180}}, because the longitude was already in the
#'   range [-180, +180], opens a window, for choosing the input file that must
#'   be in the working directory.
#'
#'   Creates an 'id' with the row number, which consists of an identifier to the
#'   coordinates: the 'id' is necessary to display the coordinates in the
#'   initial order at the end of the process.
#'
#'   Output: writes the country names in <countries.txt>; writes the missing
#'   name points (if they exist) in <countries_na.txt>, which is the input of
#'   \code{\link{get_marine}}; writes a processing report in
#'   <report_get_country.txt>.
#'
#' @seealso Requires \code{\link[rgdal]{readOGR}},
#'   \code{\link[sp]{SpatialPoints}} and \code{\link[sp]{over}} to determine the
#'   country names.
#'
#' @references 1:10m Cultural Vectors: Admin 0 - Countries. Made with Natural
#'   Earth. Free vector and raster map data @@ naturalearthdata.com. Consulted
#'   on 2017-11-15.
#'
#' @examples
#' E.g. the Portuguese territory
#' If the point is in Continental Portugal
#' attributes the name 'Portugal'
#' If the point is in the Portuguese
#' islands Azores or Madeira
#' which are the two Autonomous Regions
#' of the Portuguese sovereign
#' attributes 'Azores' and 'Madeira'
#' respectively.
#'
#' @export
get_country <- function() {
    if (file.exists("coords2getf.txt")) {
        coords <- read.table("coords2getf.txt", header = TRUE, sep = "\t", quote = "")
    } else {
        print("Load the .txt file.")
        print("Note that this function was tested for about 84.000 (maximum) pairs of coordinates.")
        print("Loading larger files is likely to cause long processing time or memory problems.")
        coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
    }
    coords_id <- row.names.data.frame(coords)
    gname_sp <- rgdal::readOGR(dsn = ".", layer = "ne_10m_admin_0_countries")
    coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(proj4string(gname_sp)))
    indices <- sp::over(coords_sp, gname_sp)
    gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, country = indices$ADMIN)
    countries <- na.omit(gname)
    write.table(countries, file = "countries.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    countries_na <- gname[is.na(gname$country), ]
    if (nrow(countries_na) != 0) {
        write.table(countries_na, file = "countries_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    }
    report <- cbind(c("Coordinate pairs processed: ", "Country names returned: ", "Country names missing: "), c(nrow(gname),
        nrow(countries), nrow(countries_na)))
    write.table(report, file = "report_get_country.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
    print("", quote = FALSE)
    print("Check < countries.txt > and < report_get_country.txt >.", quote = FALSE)
    print("If there are country names missing, run < get_marine() >.", quote = FALSE)
    print("", quote = FALSE)
    return(report)
}
