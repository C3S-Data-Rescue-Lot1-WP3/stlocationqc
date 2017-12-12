#'
#' Determines the country names for points in the continent
#'
library(sp)
library(rgdal)

get_country <- function() {
  if (file.exists("coords2getf.txt")) {
    coords <- read.table("coords2getf.txt", header = TRUE, sep = "\t", quote = "")
  } else {
    print("Load the .txt file.")
    print("Note that this function was tested for about 84.000 (maximum) pairs of coordinates.")
    print("Loading larger files is likely to cause long processing time or memory problems.")
    # Load and read the file
    coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
  }
  # Create a vector with the row number, which consists of an identifier (id) to the coordinates
  # The "id" is required to display the coordinates in the initial order at the end of the process
  coords_id <- row.names.data.frame(coords)
  # Read the shapefile with the country names
  gname_sp <- rgdal::readOGR(dsn = ".", layer = "ne_10m_admin_0_countries")
  # SpatialPoints() receive a data.frame with only two columns (latitude and longitude)
  # Longitude must be in the range [-180, +180]
  # The input Coordinate Reference System (CRS) is the shapefile CRS
  coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(proj4string(gname_sp))) 
  indices <- sp::over(coords_sp, gname_sp)
  gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, country = indices$ADMIN)
  countries <- na.omit(gname)
  write.table(countries, file = "countries.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  countries_na <- gname[is.na(gname$country), ]
  # Write the missing name points in a file, which is the input of get_marine()
  if (nrow(countries_na)!=0) {
    write.table(countries_na, file = "countries_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  report <- cbind(c("Coordinate pairs processed: ", "Country names returned: ", "Country names missing: "),                        c(nrow(gname), nrow(countries), nrow(countries_na)))
  write.table(report, file = "report_get_country.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE )
  print("", quote = FALSE)
  print("Check < countries.txt > and < report_get_country.txt >.", quote = FALSE)
  print("If there are country names missing, run < get_marine() >.", quote = FALSE)
  print("", quote = FALSE)
  return(report)
}
