#' Downloads Spatial Data with the World Seas Polygons and Saves it as a R
#' Object.
#'
#' Downloads from Natural Earth a shapefile with the world seas polygons of 10 m
#' precision and then transforms it into a SpatialPolygonsDataFrame that is
#' saved in a folder called 'polys', inside the user's working directory. The
#' data it contains is necessary for the function \code{\link{get_sea}} to work.
#'
#' @details
#' The SpatialPolygonsDataFrame is saved in the working directory, inside the
#' 'polys' folder as a .rda file and it is loaded from there when running the
#' function \code{\link{get_sea}}.
#'
#' \strong{Shapefile:}
#' \itemize{
#' \item
#' \url{http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_geography_marine_polys.zip}
#' }
#' \strong{Shapefile metadata:}
#' \itemize{
#' \item
#' \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-physical-labels/}
#' }
#'
#' @usage seas_polys()
#'
#' @references
#' Made with Natural Earth. Free vector and raster map data @@
#' naturalearthdata.com.
#'
#' @import sp
#' @import rgdal
#'
#' @export
#'
seas_polys <- function() {
# Downloads the shapefile with the marine polygons of 10m precision to a
# temporary directory which is empty when you quit the R session
cat("\n")
cat("Downloading the shapefile from Natural Earth...\n")
tmp10 <- tempfile(fileext = ".zip")
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_geography_marine_polys.zip", tmp10)
unzip(tmp10, exdir = tempdir())
rm(tmp10)
# list.files(tempdir())
# Reads the data into a SpatialPolygonsDataFrame (R object from the "sp"
# package)
mar10_sp <- rgdal::readOGR(dsn = tempdir(),
  layer = "ne_10m_geography_marine_polys", stringsAsFactors = FALSE,
  encoding = "UTF-8", use_iconv = TRUE)
# View the field names in the shapefile attribute table
# Could use names(mar_sp@data) or just:
# names(mar10_sp)
# View the lists of sea names
# mar10_sp$name
# mar10_sp$note
# sum(is.na(mar10_sp$name)) # 11
# mar10_sp$note # Have to fill the NA of "name" with the names in column "note"
# Subsets the necessary fields which allows to reduce a litle bit the object
# size
nec_fields10 <- c("name", "note", "name_fr", "name_de", "name_en", "name_es",
  "name_pt", "ne_id")
seas_polys_10m <- mar10_sp[, nec_fields10]
# Saves the SpatialPolygonsDataFrame as .RData
if (!file.exists("polys")) {
  dir.create("polys")
}
# devtools::use_data(seas_polys_10m, overwrite = TRUE)
# con <- pipe("xz -T8 -6 -e > seas_polys_10m.xz", "wb")
# save(seas_polys_10m, file = con); close(con)
save(seas_polys_10m, file = "polys/seas_polys_10m.rda")
cat("\n")
cat("A spatial object with the seas polygons of 10 m precision as \n")
cat("been created and saved in the 'polys' directory as \n")
cat("'seas_polys_10m.rda'.\n")
print(class(seas_polys_10m))
cat("Extension of the object (longitude, latitude):\n")
print(seas_polys_10m@bbox)
cat("\n")
# cat("\n Coordinate reference system:\n")
# print(seas_polys_10m@proj4string)
# Maps
# plot(seas_polys_10m)
}
