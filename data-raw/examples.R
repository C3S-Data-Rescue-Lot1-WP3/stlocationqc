#####################
# Creating examples #
#####################

# Create a directory for the Examples data
if (!file.exists("data-raw/R_Examples_data")) {
  dir.create("data-raw/R_Examples_data")
}
# Copy the samples to "R_Examples_data" directory
if (!file.exists("data-raw/R_Examples_data/coords_db_ispd.txt")) {
  file.copy(file.choose(), "data-raw/R_Examples_data")
}
if (!file.exists("data-raw/R_Examples_data/coords_eraclim_upperair.txt")) {
  file.copy(file.choose(), "data-raw/R_Examples_data")
}
if (!file.exists("data-raw/R_Examples_data/coords_medare.txt")) {
  file.copy(file.choose(), "data-raw/R_Examples_data")
}

# ISPD sample #################################################################

coords1 <- read.table("data-raw/R_Examples_data/coords_db_ispd.txt",
    header = TRUE, sep = "\t", quote = "")
coords_ispd_sample <- coords1[c(1:6,18:23, 37:47, 57:60, 158:160, 175:177, +
    191:294, 512:514, 1204:1208,1519:1521, 1815:1817, 2028:2031, 2788:2789, +
    1996:2998, 3293:3295, 4432:4435, 4454:4456, 5458:5461, 6346:6350, +
    6887:6889, 6736:6738, 7029:7042, 7149:7151, 7160:7162, 18488:18496, +
    19593:19598, 22992:22995, 23847:23849, 75356:75360, 78969:78990),]

# Save the sample as .RData to use has example
if (!file.exists("data")) {
  dir.create("data")
}
# save(list = c("coords_ispd_sample"), +
#file = paste("data", "coords_ispd_sample.RData", sep = "/"))

# This way is better because creates a smaller file
devtools::use_data(coords_ispd_sample)

# ERACLIM Upper-air sample ####################################################

coords2 <- read.table("data-raw/R_Examples_data/coords_eraclim_upperair.txt",
    header = TRUE, sep = "\t", quote = "")
coords_upperair_sample <- coords2[c(1:30, 39:50, 368:385, 834:852, 1625:1660, +
    1848:1858, 1872:1881, 1914:1920, 1969:1975, 2009:2015, 2044:2050, +
    2059:2065, 2069:2075, 2089:2095, 2119:2125, 2139:2145, 2189:2195, +
    2278:2284, 2629:2634, 2711:2716, 2757:2763, 4717:4729, 4826:4829, +
    5630:5649, 5777:5783, 5842:5848),]
devtools::use_data(coords_upperair_sample)

# Medare sample ###############################################################

coords3 <- read.table("data-raw/R_Examples_data/coords_medare.txt",
  header = TRUE, sep = "\t", quote = "")
coords_medare_sample <- coords3
devtools::use_data(coords_medare_sample)


