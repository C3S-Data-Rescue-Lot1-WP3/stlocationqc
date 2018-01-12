#'
#' Transforms the longitude from [0, 360] to [-180, +180]
#'
#' Given a pair of geographic coordinates, transforms the longitude from [0,
#' 360] to [-180, +180].
#'
#' Input: a .txt file with the longitude in the first column and the latitude in
#' the second column, separated by tabs. The header of the file must be
#' 'longitude' and 'latitude', respectively.
#'
#' Opens a window, for choosing the input file that must be in the working
#' directory. Verify if the longitude and latitude are valid. Transforms the
#' longitude.
#'
#' Output: creates a .txt file <coords2getf.txt> with the transformed longitude
#' and the latitude. This file is the input of \code{\link{get_country}} or
#' \code{\link{get_marine}} to determine the geographic names.
#'
get_lon180 <- function() {
    print("Load the coordinates file.")
    print("", quote = FALSE)
    coords <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "")
    print("Checking the data...", quote = FALSE)
    print("", quote = FALSE)
    print(str(coords))
    print("", quote = FALSE)
    if (nrow(coords) >= 10) {
        print("First 10 rows of the file:", quote = FALSE)
        print(head(coords, n = 10L))
        print("", quote = FALSE)
        print("Last 10 rows of the file:", quote = FALSE)
        print(tail(coords, n = 10L))
        print("", quote = FALSE)
    }
    x <- coords$longitude
    y <- coords$latitude
    for (i in 1:nrow(coords)) {
        if (x[i] < 0 || x[i] > 360) {
            print("Found an invalid longitude value!", quote = FALSE)
            print("Row number:", quote = FALSE)
            print(i + 1)
            print("Value:", quote = FALSE)
            print(x[i])
            print("", quote = FALSE)
            return("Please correct the invalid value(s).")
        }
    }
    for (i in 1:nrow(coords)) {
        if (y[i] < -90 || y[i] > 90) {
            print("Found an invalid latitude value!", quote = FALSE)
            print("Row number:", quote = FALSE)
            print(i + 1)
            print("Value:", quote = FALSE)
            print(y[i])
            print("", quote = FALSE)
            return("Please correct the invalid value(s).")
        }
    }
    print("Original longitude limits:", quote = FALSE)
    print(range(coords$longitude))
    print("", quote = FALSE)
    print("Latitude limits:", quote = FALSE)
    print(range(coords$latitude))
    print("", quote = FALSE)
    lon180 <- c()
    for (i in 1:nrow(coords)) {
        if (x[i] > 180) {
            lon180[i] <- round(x[i] - 360, digits = 2)
        } else {
            lon180[i] <- x[i]
        }
    }
    print("New longitude limits:", quote = FALSE)
    print(range(lon180))
    print("", quote = FALSE)
    coords180 <- data.frame(longitude = lon180, latitude = coords$latitude)
    write.table(coords180, file = "coords2getf.txt", row.names = FALSE,
      col.names = TRUE, sep = "\t", quote = FALSE)
    print("A file with the longitudes in the range [-180, +180], < coords2getf.txt >, as been created.", quote = FALSE)
    print("You can run < get_country() > or < get_marine() > now.", quote = FALSE)
    print("", quote = FALSE)
}
