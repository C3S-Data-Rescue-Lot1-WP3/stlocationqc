#'
#' Determines the marine regions names for coordinate points
#'
#' Given a pair of geographic coordinates in a marine region, determines the
#' marine region name.
#'
#' @details Input: A text file with the coordinates in decimal degrees and a
#'   shapefile with the marine regions names (resulting from the union of the
#'   Exclusive Economic Zones (EEZ) with the International Hydrographic
#'   Organization (IHO) Seas.
#'
#'   The .txt file has to be in the following format: with the longitude in the
#'   first column and the latitude in the second column, separated by tabs.The
#'   header of the file must be 'longitude' and 'latitude', respectively and
#'   longitude must be in the range [-180, +180].
#'
#'   If exists <countries_na.txt> created by \code{\link{get_country}},
#'   automatically loads and reads the coordinates file. If wasn't necessary to
#'   use \code{\link{get_country}}, because all the points are in a marine
#'   region, opens a window for choosing the input file that must be in the
#'   working directory.
#'
#'   Preserves the 'id' assigned to the coordinates in \code{\link{get_country}}
#'   or, for the case of \code{\link{get_country}} wasn't necessary, creates an
#'   'id' with the row number, which consists of an identifier to the
#'   coordinates: the 'id' is necessary to display the coordinates in the
#'   initial order at the end of the process.
#'
#'   Output: writes the marine regions names in <marine_regions.txt>; writes the
#'   missing name points (if they exist) in <marine_regions_na.txt>, which is
#'   the input of \code{\link{get_country_sea}}; writes a processing report in
#'   <report_get_marine.txt>.
#'
#' @seealso Requires \code{\link[rgdal]{readOGR}},
#'   \code{\link[sp]{SpatialPoints}} and \code{\link[sp]{over}} to determine the
#'   country names.
#'
#' @references VLIZ (2012). Intersect of IHO Sea Areas and Exclusive Economic
#'   Zones (version 2). Available online at http://www.marineregions.org/.
#'   Consulted on 2017-11-15.
#'
#' @examples
#' E.g. the Portuguese territory
#' If the point is in the
#' Portuguese Continental Shelf
#' attributes the name
#' 'Portuguese Part of the
#' Atlantic Ocean'.
#' If the point isnt part of a
#' continental shelf
#' attributes the name of the
#' sea/ocean
#' e.g. 'North Atlantic Ocean'.
#'
#' @export
get_marine <- function() {
    if (file.exists("countries_na.txt")) {
        coords <- read.table("countries_na.txt", header = TRUE, sep = "\t", quote = "")
        coords_id <- coords$id
        coords$id <- NULL
        # Eliminate the missing values column
        coords$country <- NULL
    } else {
        print("Choose the .txt file.", quote = FALSE)
        coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
        coords_id <- row.names.data.frame(coords)
    }
    gname_sp <- rgdal::readOGR(dsn = ".", layer = "EEZ_IHO_union_v2")
    # 'coords' is a data.frame with only two columns (longitude and latitude)
    coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(proj4string(gname_sp)))
    indices <- sp::over(coords_sp, gname_sp)
    gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, marine_region = indices$MarRegion)
    marine_regions <- na.omit(gname)
    write.table(marine_regions, file = "marine_regions.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    marine_regions_na <- gname[is.na(gname$marine_region), ]
    if (nrow(marine_regions_na) != 0) {
        write.table(marine_regions_na, file = "marine_regions_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t",
            quote = FALSE)
    }
    report <- cbind(c("Coordinate pairs processed: ", "Marine Region names returned: ", "Marine Region names missing: "),
        c(nrow(gname), nrow(marine_regions), nrow(marine_regions_na)))
    write.table(report, file = "report_get_marine.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
    print("", quote = FALSE)
    print("Check < marine_regions.txt > and < report_get_marine.txt >.", quote = FALSE)
    print("If there are marine region names missing, run < get_country_sea() >.", quote = FALSE)
    print("", quote = FALSE)
    return(report)
}
