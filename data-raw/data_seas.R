###############################################################################
# Access the Web Feature Service (WFS) from Marine Regions using the          #
# "mregions" package developed by rOpenSci                                    #
# https://github.com/ropenscilabs/mregions                                    #
#                                                                             #
# Development version                                                         #
# devtools::install_github("ropenscilabs/mregions")                           #
###############################################################################

# Data:
# http://www.marineregions.org/webservices.php

# Metadata:
# http://www.marineregions.org/downloads.php#marbound

# Install "mregions"
# install.packages("mregions")

# Load "mregions"
library("mregions")
# mr_shp {mregions}
# ?mr_shp
# Get a region shp file
# mr_shp(key = NULL, name = NULL, maxFeatures = 50, overwrite = TRUE,
# read = TRUE, filter = NULL, ...)

eez_iho <- mregions::mr_shp(key = "MarineRegions:eez_iho_union_v2", maxFeatures = 1000)
# works
plot(eez_iho)
names(eez_iho)
#seas <- mr_shp(key = "MarineRegions:World_Seas_IHO_v2", maxFeatures = 200)
# gives error
# Error: unzip failed, check query, try again - & removed files
iho <- mregions::mr_shp(key = "MarineRegions:iho", maxFeatures = 300)
# works

#eez_land <- mr_shp(key = "MarineRegions:EEZ_land_v2_201410", maxFeatures = 300)
# gives error
# Error: unzip failed, check query, try again - & removed files
eez_land <- mregions::mr_shp(key = "MarineRegions:eez_land", maxFeatures = 300)
# works

eez_bound <- mregions::mr_shp(key = "MarineRegions:eez_boundaries", maxFeatures = 300)
# works

eez <- mregions::mr_shp(key = "MarineRegions:eez", maxFeatures = 300) # works
# plot(eez) # Takes too long to draw (dont use)


##########################################################################

# Try to access the web services by myself...

# For IHO Sea Areas
dsn <- "http://geo.vliz.be/geoserver/MarineRegions/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=MarineRegions:iho&maxFeatures=10"
rgdal::ogrInfo(dsn) # works

# ogr2ogr(dsn, "iho_.shp")
# NULL
# Warning messages:
#   1: In gdal_setInstallation(ignore.full_scan = ignore.full_scan, verbose = verbose) :
#   No GDAL installation found. Please install 'gdal' before continuing:
#   - www.gdal.org (no HDF4 support!)
# - www.trac.osgeo.org/osgeo4w/ (with HDF4 support RECOMMENDED)
# - www.fwtools.maptools.org (with HDF4 support)
#
# 2: In gdal_setInstallation(ignore.full_scan = ignore.full_scan, verbose = verbose) :
#   If you think GDAL is installed, please run:
#   gdal_setInstallation(ignore.full_scan=FALSE)
# > install.packages("gdal")
# Installing package into ‘C:/Users/Clara/Documents/R/win-library/3.4’
# (as ‘lib’ is unspecified)
# Warning in install.packages :
#   package ‘gdal’ is not available (for R version 3.4.3)

# writeOGR(obj = iho, layer = 'test', dsn = ".", driver="GML") # Gives error

##########################################################################


##############################################################################
# Transform the shapefiles from Marine Regions into SpatialObjectDataFrames  #
# and save as .RData which can be useful in the future                       #
##############################################################################
#
# Data:
# http://www.marineregions.org/downloads.php#marbound
#
# Metadata:
# http://www.marineregions.org/downloads.php#marbound
#
# Download the necessary R objects
# if (file.exists("data-raw/R_GIS_data")) {
#   tmp <- tempfile(fileext = ".zip")
#   download.file("http://eraclim2.rd.ciencias.ulisboa.pt/shp2rgeoname.zip", tmp)
#   # Change the name
#   unzip(tmp, exdir = "data-raw/R_GIS_data", junkpaths = TRUE)
#   # junkpaths = TRUE to eliminate the directory shp2rgeoname
# } # works


##############################
#      IHO Sea names         #
##############################

# Show the unzipped file
list.files("data-raw/R_GIS_data")
# Load the required libraries
library(sp)
library(rgdal)
# Read the data into a SpatialPolygonsDataFrame (R object from the "sp" package)
# Layer is the name of the unzipped shapefile without file type extension
iho_sp <- rgdal::readOGR(dsn = "data-raw/R_GIS_data", layer = "World_Seas_IHO_v2")
# Take a look at the data
class(iho_sp)
slotNames(iho_sp)
iho_sp@data
iho_sp@bbox
iho_sp@proj4string
# plot(sea_sp) # Takes too long to draw (dont use)
# Names of the fiels in the shapefile attribute table
# Could use names(sea_sp@data) or just:
names(iho_sp)
iho_sp$NAME
# Select the necessary fields which allows to reduce a litle bit the object size
nec_fields <- c("NAME", "MRGID")
# User friendly names
new_names <- c("iho_sea", "mrgid")
# Subset the full dataset extracting only the desired fields
iho_sp_subset <- iho_sp[ ,nec_fields] # All the lines from that columns
print(object.size(iho_sp), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 466.1 MB
print(object.size(iho_sp_subset), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 466.1 MB
# Assign the new field names
names(iho_sp_subset) <- new_names
# Rename the final SpatialPolygonsDataFrame
data_iho <- iho_sp_subset
# Save the converted data as .RData for faster loading in the future
if (!file.exists("data")) {
  dir.create("data")
}
save(list = c("data_iho"), file = paste("data", "data_iho.RData", sep = "/"))

data_iho$iho_sea
class(data_iho)

#############################################################################
#   Name of the Country in the border with Exclusive Economic Zones (EEZ)   #
#############################################################################

eez_land_sp <- rgdal::readOGR(dsn = "data-raw/R_GIS_data", layer = "EEZ_land_v2_201410")

class(eez_land_sp)
slotNames(eez_land_sp)
eez_land_sp@data
eez_land_sp@bbox
eez_land_sp@proj4string
plot(eez_land_sp)

names(eez_land_sp)
eez_land_sp$Country

nec_fields <- c("ISO_3digit", "Country")

new_names <- c("iso3", "country")

eez_land_sp_subset <- eez_land_sp[ ,nec_fields]
print(object.size(eez_land_sp), quote = FALSE, units = "MB", standard = "SI", digits = 1L)
print(object.size(eez_land_sp_subset), quote = FALSE, units = "MB", standard = "SI", digits = 1L)

names(eez_land_sp_subset) <- new_names

data_eez_land <- eez_land_sp_subset

if (!file.exists("data")) {
  dir.create("data")
}
save(list = c("data_eez_land"), file = paste("data", "data_eez_land.RData", sep = "/"))

data_eez_land$country
class(data_eez_land)

####################################
#   EEZ, IHO Sea or Country name   #
####################################

eez_iho_sp <- rgdal::readOGR(dsn = "data-raw/R_GIS_data", layer = "EEZ_IHO_union_v2")

class(eez_iho_sp)
slotNames(eez_iho_sp)
eez_iho_sp@data
eez_iho_sp@bbox
eez_iho_sp@proj4string
plot(eez_iho_sp)

names(eez_iho_sp)
eez_iho_sp$Country

nec_fields <- c("MarRegion", "MRGID", "IHO_Sea", "EEZ", "Country")

new_names <- c("maritime_region", "mrgid", "iho_sea", "eez", "country")

eez_iho_sp_subset <- eez_iho_sp[ ,nec_fields]
print(object.size(eez_iho_sp), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 11.9 mb
print(object.size(eez_iho_sp_subset), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 11.8 mb

names(eez_iho_sp_subset) <- new_names

data_eez_iho <- eez_iho_sp_subset

if (!file.exists("data")) {
  dir.create("data")
}
save(list = c("data_eez_iho"), file = paste("data", "data_eez_iho.RData", sep = "/"))

data_eez_iho$country
class(data_eez_iho)

####################################
#               EEZ                #
####################################

eez_sp <- rgdal::readOGR(dsn = "data-raw/R_GIS_data", layer = "eez_v10")

class(eez_sp)
slotNames(eez_sp)
eez_sp@data
eez_sp@bbox
eez_sp@proj4string
plot(eez_sp)

names(eez_sp)
eez_sp$GeoName

nec_fields <- c("MRGID", "GeoName")

new_names <- c("mrgid", "eez")

eez_sp_subset <- eez_sp[ ,nec_fields]
print(object.size(eez_sp), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 480.7 Mb
print(object.size(eez_sp_subset), quote = FALSE, units = "MB", standard = "SI", digits = 1L) # 480.5 Mb

names(eez_sp_subset) <- new_names

data_eez <- eez_sp_subset

if (!file.exists("data")) {
  dir.create("data")
}
save(list = c("data_eez"), file = paste("data", "data_eez.RData", sep = "/"))

data_eez$eez
class(data_eez)


