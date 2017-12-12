#'
#' Determines the marine region
#'
library(sp)
library(rgdal)

get_marine <- function() {
  if (file.exists("countries_na.txt")) {
    coords <- read.table("countries_na.txt", header = TRUE, sep = "\t", quote = "")
    # Save the "id" in a vector and delete the "id" column from the data.frame
    # Need to preserve the "id" to order the data at the end
    coords_id <- coords$id
    coords$id <- NULL
    # Eliminate the missing values column
    coords$country <- NULL
  } else {
    print("Choose the .txt file.", quote = FALSE)
    coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
    # Create a vector with the row number, which consists of an identifier (id) to the coordinates
    # The "id" is required to display the coordinates in the initial order at the end of the process
    coords_id <- row.names.data.frame(coords)
  }
  #
  gname_sp <- rgdal::readOGR(dsn = ".", layer = "EEZ_IHO_union_v2")
  # "coords" is a data.frame with only two columns (longitude and latitude)
  coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(proj4string(gname_sp)))
  indices <- sp::over(coords_sp, gname_sp)
  gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, marine_region = indices$MarRegion)
  marine_regions <- na.omit(gname)
  write.table(marine_regions, file = "marine_regions.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  marine_regions_na <- gname[is.na(gname$marine_region), ]
  if (nrow(marine_regions_na)!=0) {
    write.table(marine_regions_na, file = "marine_regions_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  report <- cbind(c("Coordinate pairs processed: ", "Marine Region names returned: ",                              "Marine Region names missing: "), c(nrow(gname), nrow(marine_regions), nrow(marine_regions_na)))
  write.table(report, file = "report_get_marine.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  print("", quote = FALSE)
  print("Check < marine_regions.txt > and < report_get_marine.txt >.", quote = FALSE)
  print("If there are marine region names missing, run < get_country_sea() >.", quote = FALSE)
  print("", quote = FALSE)
  return(report)
}



