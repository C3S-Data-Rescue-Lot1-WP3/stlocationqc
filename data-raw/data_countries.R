###############################################################################
# Download the shapefile with the country names from Natural Earth            #
###############################################################################
#
# Data:
# http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip
#
# Metadata:
# http://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/

# Create a directory for the data
if (!file.exists("data-raw/R_GIS_data")) {
  dir.create("data-raw/R_GIS_data")
}
# Download the shapefile of country polygons
tmp <- tempfile(fileext = ".zip")
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip", tmp)
unzip(tmp, exdir = "data-raw/R_GIS_data")
rm(tmp)

# Show the unzipped file
list.files("data-raw/R_GIS_data")
# Load the required libraries
library(sp)
library(rgdal)
# Read the data into a SpatialPolygonsDataFrame (R object from the "sp" package)
# Layer is the name of the unzipped shapefile without file type extension
coun_sp <- rgdal::readOGR(dsn = "data-raw/R_GIS_data", layer = "ne_10m_admin_0_countries")
# Take a look at the data
class(coun_sp)
slotNames(coun_sp)
coun_sp@data
coun_sp@bbox
coun_sp@proj4string
plot(coun_sp)
# Names of the fiels in the shapefile attribute table
# Could use names(coun_sp@data) or just:
names(coun_sp)
# Select the necessary fields which allows to reduce a litle bit the object size
nec_fields <- c("ISO_A3", "SUBREGION", "SOVEREIGNT", "ADMIN", "CONTINENT")
# User friendly names
new_names <- c("iso_a3", "subregion", "sovereign", "admin_region", "continent")
# Subset the full dataset extracting only the desired fields
coun_sp_subset <- coun_sp[ ,nec_fields]
print(object.size(coun_sp), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 15.9 MB
print(object.size(coun_sp_subset), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 15.4 MB
# Assign the new field names
names(coun_sp_subset) <- new_names
# Rename the final SpatialPolygonsDataFrame
data_countries <- coun_sp_subset
# Save the converted data as .RData for faster loading in the future
if (!file.exists("data")) {
  dir.create("data")
}
save(list = c("data_countries"), file = paste("data", "data_countries.RData", sep = "/"))
# "data_countries.RData" has 5.2 MB
# Upon inspecting the data we can see that there are 255 administrative regions in the world
data_countries$admin_region
class(data_countries)

