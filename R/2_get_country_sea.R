#'
#' Determines the  country names for points in the boundaries of continent with EEZ - Exclusive Economic Zones
#' Determines the  ocean/sea names or the EEZ names for other points in a marine region
#'
library(sp)
library(rgdal)

get_country_sea <- function() {
  
  if (file.exists("marine_regions_na.txt")) {
    coords <- read.table("marine_regions_na.txt", header = TRUE, sep = "\t", quote = "")
    coords_id <- coords$id
    coords$id <- NULL
    coords$marine_region <- NULL
    print("", quote = FALSE)
    print("Processing Country/Sea names...", quote = FALSE)
    print("", quote = FALSE)
    gname_sp <- rgdal::readOGR(dsn = ".", layer = "EEZ_land_v2_201410")
    coords_sp <- sp::SpatialPoints(coords, proj4string = sp::CRS(proj4string(gname_sp))) 
    indices <- sp::over(coords_sp, gname_sp)
    gname <- data.frame(id = coords_id, longitude = coords$longitude, latitude = coords$latitude, country = indices$Country)
    countries <- na.omit(gname)
    countries_na <- gname[is.na(gname$country), ]
    
    #
    #write.table(countries, file = "countries2.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    #
    
    if (nrow(countries_na) == 0) {
      write.table(countries, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      report <- cbind(c("Coordinate pairs processed: ", "Country/Sea/EEZ names returned: ",                              "Country/Sea/EEZ names missing: "), c(nrow(gname), nrow(countries), nrow(countries_na)))
      write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
      print("", quote = FALSE)
      print("Check < countries_seas.txt > and < report_get_country_sea.txt >.", quote = FALSE)
      print("", quote = FALSE)
      return(report)
      
    } else {
      
      #
      #write.table(countries_na, file = "countries2_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      #
      
      coords2_id <- countries_na$id
      countries_na$id <- NULL
      countries_na$country <- NULL
      print("", quote = FALSE)
      print("Processing Sea names...", quote = FALSE)
      print("", quote = FALSE)
      gname2_sp <- rgdal::readOGR(dsn = ".", layer = "World_Seas_IHO_v2")
      coords2_sp <- sp::SpatialPoints(countries_na, proj4string = sp::CRS(proj4string(gname2_sp))) 
      indices <- sp::over(coords2_sp, gname2_sp)
      gname2 <- data.frame(id = coords2_id, longitude = countries_na$longitude, latitude = countries_na$latitude, sea = indices$NAME)
      seas <- na.omit(gname2)
      seas_na <- gname2[is.na(gname2$sea), ]
      
      #
      #write.table(seas, file = "seas2.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
      #
      
      if (nrow(seas_na) == 0) {
        names(countries)[4] <- "country_sea"
        names(seas)[4] <- "country_sea"
        countries_seas <- rbind(countries, seas)
        write.table(countries_seas, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        report <- cbind(c("Coordinate pairs processed: ", "Country/Sea/EEZ names returned: ",                              "Country/Sea/EEZ names missing: "), c(nrow(gname), nrow(countries_seas), nrow(seas_na)))
        write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
        print("", quote = FALSE)
        print("Check < countries_seas.txt > and < report_get_country_sea.txt >.", quote = FALSE)
        print("", quote = FALSE)
        return(report)
        
      } else {
        #
        write.table(seas_na, file = "seas2_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        #
        coords3_id <- seas_na$id
        seas_na$id <- NULL
        seas_na$sea <- NULL
        print("", quote = FALSE)
        print("Processing EEZ names...", quote = FALSE)
        print("", quote = FALSE)
        gname3_sp <- rgdal::readOGR(dsn = ".", layer = "eez")
        coords3_sp <- sp::SpatialPoints(seas_na, proj4string = sp::CRS(proj4string(gname3_sp)))
        indices <- sp::over(coords3_sp, gname3_sp)
        gname3 <- data.frame(id = coords3_id, longitude = seas_na$longitude, latitude = seas_na$latitude, eez = indices$GeoName)
        eezs <- na.omit(gname3)
        eezs_na <- gname3[is.na(gname3$eez), ]
        #
        write.table(eezs, file = "eezs2.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
        #
        
        if (nrow(eezs_na) == 0) {
          names(countries)[4] <- "country_sea"
          names(seas)[4] <- "country_sea"
          names(eezs)[4] <- "country_sea"
          countries_seas <- rbind(countries, seas, eezs)
          write.table(countries_seas, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
          report <- cbind(c("Coordinate pairs processed: ", "Country/Sea/EEZ names returned: ",                              "Country/Sea/EEZ names missing: "), c(nrow(gname), nrow(countries_seas), nrow(eezs_na)))
          write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
          print("", quote = FALSE)
          print("Check < countries_seas.txt > and < report_get_country_sea.txt >.", quote = FALSE)
          print("", quote = FALSE)
          return(report)
          
        } else {
          print("", quote = FALSE)
          print("The output still has coordinate pairs without a name. Please, report the problem.", quote = FALSE)
          print("The points without a name received the indication of < NO_NAME > in the column < country_sea >.", quote = FALSE)
          print("", quote = FALSE)
          write.table(eezs_na, file = "countries_seas_na.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
          names(countries)[4] <- "country_sea"
          names(seas)[4] <- "country_sea"
          names(eezs)[4] <- "country_sea"
          names(eezs_na)[4] <- "country_sea"
          eezs_na$country_sea <- "NO_NAME"
          countries_seas <- rbind(countries, seas, eezs, eezs_na)
          write.table(countries_seas, file = "countries_seas.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)

          report <- cbind(c("Coordinate pairs processed: ", "Country/Sea/EEZ names returned: ",                              "Country/Sea/EEZ names missing: "), c(nrow(gname), nrow(countries_seas), nrow(eezs_na)))
          write.table(report, file = "report_get_country_sea.txt", row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
          print("", quote = FALSE)
          print("Check < countries_seas.txt > and < report_get_country_sea.txt >.", quote = FALSE)
          print("Then run < order_data() > to restore the inicial order of the coordinates.")
          print("", quote = FALSE)
          return(report)
        }
      }
    }
    
  } else {
    print("", quote = FALSE)
    print("Missing the file < marine_regions_na.txt >.", quote = FALSE)
  }

}
