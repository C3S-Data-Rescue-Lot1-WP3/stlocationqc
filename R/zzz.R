# To avoid R CMD Check NOTE ('no visible binding for global variable...')
.onLoad <- function(libname = find.package("stlocationqc"), pkgname = "stlocationqc") {
  utils::globalVariables(c("countries_polys_10m", "countries_polys_50m",
    "seas_polys_10m"))
}
