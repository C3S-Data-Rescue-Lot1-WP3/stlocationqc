#'
#' Compare the Existing Country Names with the Determined Country Names.
#'
#' Compare the given country names with the names assigned by the function
#' \code{\link{get_country}} and also by the functions \code{\link{get_sea}} and
#' \code{\link{get_country_sea}}, if applicable.
#'
#' @details
#' \itemize{
#' \item Input:
#' \enumerate{
#' \item The file 'geographic_names.txt', created by
#' \code{\link{order_data}}, which contains the names assigned by the function
#' \code{\link{get_country}} and also by the functions \code{\link{get_sea}} and
#' \code{\link{get_country_sea}}, if applicable.
#' \item The file used as input of \code{\link{get_country}} with the given
#' names to compare with the ones determinated by the functions.
#' }
#' \item The function does the following:
#' \enumerate{
#' \item Writes a file with all the points that have a different name;
#' \item Writes a file with the points that have a different name for the same
#' country (both names correct but different);
#' \item Writes a file with the points that have a country name assigned to the
#' given name and have the  Exclusive Economic Zone name of that country
#' assigned to the determinated name. Probably this points are very close to
#' the coastline and the associated positioning error causes them to be
#' effectively located at sea. It's a tolerance issue;
#' \item Writes a file whose points have unrelated names. This indicates that
#' they are probably incorrectly located, consisting in errors that must be
#' corrected;
#' \item Writes a processing report.
#' }
#' \item Output:
#' \enumerate{
#' \item A .txt file named 'different_names.txt';
#' \item A .txt file named 'err_just_name.txt';
#' \item A .txt file named 'err_just_tol.txt';
#' \item A .txt file named 'err_location.txt';
#' \item A .txt file named 'report_compare_country.txt'.
#' }
#' }
#'
#' @export
compare_country <- function() {
  print("Choose the .txt file with the existing country names.", quote = FALSE)
  print("", quote = FALSE)
  df1 <- read.table(file.choose(), header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE )
  if (file.exists("geographic_names.txt")) {
    df2 <- read.table("geographic_names.txt", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)
  } else {
    print("Missing file with the determined country names ('geographic_names.txt').", quote = FALSE)
  }

  coun1 <- df1$country
  coun2 <- df2$geo_name
  for (i in 1:nrow(df2)) {
    coun1[i] <- trimws(coun1[i])
    coun2[i] <- trimws(coun2[i])
  }
  df <- data.frame(id = df2$id, longitude = df2$longitude, latitude = df2$latitude, country_determined = df2$geo_name, country_initial = df1$country, equal_name = coun2 == coun1, stringsAsFactors = FALSE)
  df_false <- df[df$equal_name == "FALSE", ]
  write.table(df_false, file = "different_names.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  print(noquote("Please, check the file 'different_names.txt'."))
  print("", quote = FALSE)

  counf_id <- df_false$id
  counf_lon <- df_false$longitude
  counf_lat <- df_false$latitude
  counf1 <- df_false$country_initial
  counf2 <- df_false$country_determined

  err_just_name <- data.frame()
  err_just_tol <- data.frame()
  err_location <- data.frame()

  for (j in 1:nrow(df_false)) {
    c1 <- unlist(strsplit(counf1[j], " "))
    c2 <- unlist(strsplit(counf2[j], " "))
    c1c2 <- intersect(c1, c2)
    # Check the three first letters
    sub12 <- grepl(substr(counf2[j], 1, 3), c1)
    # Check if the name contains 'Exclusive Economic Zone'.
    word2 <- grepl("Exclusive Economic Zone", counf2[j])
    # Probably the country is the same but with just a slighly different name.
    if (!identical(c1c2, character(0))) {
      err_name <- data.frame(id = counf_id[j], longitude = counf_lon[j], latitude = counf_lat[j], country_initial = counf1[j], country_determined = counf2[j])
      err_just_name <- rbind(err_just_name, err_name)
    # Probably the point is in the sea but very cose to the coastline. Its a matter of tolerance.
    } else if (sub12 && word2) {
      err_tol <- data.frame(id = counf_id[j], longitude = counf_lon[j], latitude = counf_lat[j], country_initial = counf1[j], country_determined = counf2[j])
      err_just_tol <- rbind(err_just_tol, err_tol)
    } else {
    # There are realy something wrong with this points.
      err_loc <- data.frame(id = counf_id[j], longitude = counf_lon[j], latitude = counf_lat[j], country_initial = counf1[j], country_determined = counf2[j])
      err_location <- rbind(err_location, err_loc)
    }
  }
  if (nrow(err_just_name) != 0){
    # Plotar os pontos (working on this...)
    write.table(err_just_name, "err_just_name.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    print(noquote("Please, check the file 'err_just_name.txt'."))
    print(noquote("Probabily the country is the same but with just a slighly different name."))
    print("", quote = FALSE)
  }
  if (nrow(err_just_tol) != 0){
    # Plotar os pontos
    write.table(err_just_tol, "err_just_tol.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    print(noquote("Please, check the file 'err_just_tol.txt'."))
    print(noquote("Probably this points are very close to the coastline and the associated positioning error causes them to be effectively located at sea. It's a tolerance issue."))
    print("", quote = FALSE)
  }
  if (nrow(err_location) != 0) {
    # Plotar os pontos
    write.table(err_location, "err_location.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    print(noquote("Please, check the file 'err_location.txt'."))
    print(noquote("This points are probably incorrectly located, consisting in errors that must be corrected."))
    print("", quote = FALSE)
  }
  report <- data.frame(cbind(c("Total of coordinate pairs processed: ", "Equal country names: ", "Different country names: ", "Probable ambiguous country name: ", "Probable tolerance issue: ", "Probable location error: "), c(nrow(df), nrow(df) - nrow(df_false), nrow(df_false), nrow(err_just_name), nrow(err_just_tol), nrow(err_location))))

  write.table(report, "report_compare_country.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
  print(noquote("Please, check the file 'report_compare_country.txt'."))
  print("", quote = FALSE)
  return(report)
}



