#'
#' Transforms the longitude from [0, 360] to [-180, +180]
#' 
get_lon180 <- function() {
  print("Load the coordinates file.")
  print("", quote = FALSE)
  # Load and read the file
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
  # Verify if the longitude and latitude are valid
  x <- coords$longitude
  y <- coords$latitude
  for (i in 1:nrow(coords)) {
    if (x[i] < 0 || x[i] > 360) {
    print("Found an invalid longitude value!", quote = FALSE)
    print("Row number:", quote = FALSE)
    print(i+1)
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
      print(i+1)
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

  # Creates a vector with the longitudes in the range [-180, +180]
  lon180 <- c() 
  for (i in 1:nrow(coords)) {
      if (x[i] > 180) {
        lon180[i] = round(x[i] - 360, digits = 2)
      } else {
        lon180[i] = x[i]
      }
  }
  
  print("New longitude limits:", quote = FALSE)
  print(range(lon180))
  print("", quote = FALSE)
  
  # Creates a data.frame with the new coordinates and a .txt file (the input of get_country or get_marine)
  coords180 <- data.frame(longitude = lon180, latitude = coords$latitude)
  write.table(coords180,file = "coords2getf.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  print("A file with the longitudes in the range [-180, +180], < coords2getf.txt >, as been created.", quote = FALSE)
  print("You can run < get_country() > or < get_marine() > now.", quote = FALSE)
  print("", quote = FALSE)
}
