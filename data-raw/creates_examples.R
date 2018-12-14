# Creates Example Datasets

# Directory for the Examples
if (!file.exists("data-raw/Examples-inventories")) {
  dir.create("data-raw/Examples-inventories")
}
# Copy the example text files to "Examples-inventories" directory
if (!file.exists("data-raw/Examples-inventories/coords_ispd.txt")) {
  file.copy(file.choose(), "data-raw/Examples-inventories")
}
if (!file.exists("data-raw/Examples-inventories/coords&country_eraclim_uao_fp.txt")) {
  file.copy(file.choose(), "data-raw/Examples-inventories")
}
if (!file.exists("data-raw/Examples-inventories/coords&country_medare.txt")) {
  file.copy(file.choose(), "data-raw/Examples-inventories")
}
if (!file.exists("data-raw/Examples-inventories/coords&other_eraclim_lso.txt")) {
  file.copy(file.choose(), "data-raw/Examples-inventories")
}
# ISPD
ispd <- read.table("data-raw/Examples-inventories/coords_ispd.txt",
  header = FALSE, sep = "\t", quote = "",
  col.names = c("lon", "lat"),
  stringsAsFactors = FALSE)
print(str(ispd))
devtools::use_data(ispd)
tools::checkRdaFiles("data/ispd.rda")
devtools::use_data(ispd, compress = "bzip2", overwrite = TRUE)
# ERACLIM Upper-air Fixed Platforms
eraclim_uao_fp <-
  read.table("data-raw/Examples-inventories/coords&country_eraclim_uao_fp.txt",
    header = FALSE, sep = "\t", quote = "",
    col.names = c("lon", "lat", "country_gv"),
    stringsAsFactors = FALSE)
print(str(eraclim_uao_fp))
devtools::use_data(eraclim_uao_fp)
tools::checkRdaFiles("data/eraclim_uao_fp.rda")
devtools::use_data(eraclim_uao_fp, compress = "bzip2", overwrite = TRUE)
# MEDARE
medare <-
  read.table("data-raw/Examples-inventories/coords&country_medare.txt",
    header = FALSE, sep = "\t", quote = "",
    col.names = c("lon", "lat", "country_gv"),
    stringsAsFactors = FALSE)
print(str(medare))
# ERACLIM Land Surface Observations
eraclim_lso <-
  read.table("data-raw/Examples-inventories/coords&other_eraclim_lso.txt",
    header = FALSE, sep = "\t", quote = "",
    col.names = c("lon", "lat", "country_gv", "wmo_id", "wmo_region", "alt_msl",
      "station_name"),
    stringsAsFactors = FALSE)
print(str(eraclim_lso))
