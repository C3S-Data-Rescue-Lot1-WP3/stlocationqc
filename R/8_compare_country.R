#' Compares the Given Country Names with the Determined Country Names.
#'
#' Compares the given country names with the names assigned by the function
#' \code{\link{get_country}}, \code{\link{get_country_shoreline}} or
#' \code{\link{get_sea}}.
#'
#' @details
#' \strong{Input:}
#' \itemize{
#' \item A data frame with the coordinates and the given country names or a text
#' file without header, the longitude in the first column, the latitude in the
#' second column, both in decimal degrees. The given country name goes in the
#' third column. The columns are separated by tabs and missing values must be
#' codified as 'NA' in all fields.
#' \item The output of \code{\link{get_country}} - \strong{'countries'} -,
#' \code{\link{get_country_shoreline}} - \strong{'countries_sh'} -,
#' \code{\link{get_sea}} - \strong{'seas'} and \strong{'miss_seas'} - and
#' \code{\link{get_lon180}} or \code{\link{test_geocoord}} -
#' \strong{'excl_coords'}. However, only the 'countries' data frame is
#' compulsory because all those data frames may not exist.
#' }
#' \strong{Output:}
#' \itemize{
#' \item Several text files, until the maximum of 9, which have the following
#' header: \strong{country_gv |id | lon | lat | country | sovereignt | adm0_a3 |
#' name_de | name_es | name_fr | name_pt}. Those files split the list of points
#' by types of differences between the given and the determinated country name:
#' \itemize{
#' \item Missing given or determined country name
#' \item Equal country name
#' \item Given country name equals to sovereignty
#' \item Given country name in another language
#' \item Given country name in upper case
#' \item Given country name with some equal words - English or another language
#' \item Set of 3 letters equals at the beginning or at the end of the words
#' \item Points that fall into the sea
#' \item Different country name
#' }
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' ## First run
#' test_geocoord(coords = eraclim_uao_fp)
#' ## Then run sequentially until all points have names assigned
#' get_country(icoords = coords_ok)
#' get_country_shoreline(icoords = miss_countries, tol)
#' get_sea(icoords = miss_countries_sh)
#' ## And finally
#' compare_country(countries_gv = eraclim_uao_fp, countries = countries,
#' countries_sh = countries_sh, seas = seas, miss_seas = miss_seas, excl_coords
#' = excl_coords)
#' }
#'
#' @usage
#' compare_country(countries_gv = NULL, countries, countries_sh = NULL, seas =
#' NULL, miss_seas = NULL, excl_coords = NULL)
#'
#' @param countries_gv data frame with the coordinates and the given country
#'   names \strong{lon | lat | country_gv}.
#' @param countries data frame output of \code{\link{get_country}}.
#' @param countries_sh data frame output of \code{\link{get_country_shoreline}}.
#' @param seas data frame output of \code{\link{get_sea}}.
#' @param miss_seas data frame output of \code{\link{get_sea}}.
#' @param excl_coords data frame output of \code{\link{get_lon180}} or
#'   \code{\link{test_geocoord}}.
#'
#' @import stats
#' @import utils
#'
#' @export
#'
compare_country <- function(countries_gv = NULL, countries,
  countries_sh = NULL, seas = NULL, miss_seas = NULL, excl_coords = NULL) {
  if (!is.null(countries_gv)) {
    countries_gv <- countries_gv
    countries_gv <- countries_gv[, 3, drop = FALSE]
  } else {
    cat("\n")
    cat("Please, select the text file with the coordinates ")
    cat("and given country names. \n")
    countries_gv <- read.table(file.choose(), header = FALSE, sep = "\t",
      quote = "", stringsAsFactors = FALSE)
    countries_gv <- countries_gv[, 3, drop = FALSE]
    names(countries_gv) <- c("country_gv")
    countries_gv$country_gv <- as.character(countries_gv$country_gv)
  }
  countries_det <- countries
  if (!is.null(countries_sh)) {
    countries_det <- rbind(countries_det, countries_sh)
  }
  if (!is.null(seas)) {
    names(seas)[4] <- "country"
    seas$sovereignt <- NA_character_
    seas$adm0_a3 <- NA_character_
    seas$name_de <- NA_character_
    seas$name_es <- NA_character_
    seas$name_fr <- NA_character_
    seas$name_pt <- NA_character_
    countries_det <- rbind(countries_det, seas)
  }
  if (!is.null(miss_seas)) {
    miss_seas$country <- "MISSING_NAME"
    miss_seas$sovereignt <- NA_character_
    miss_seas$adm0_a3 <- NA_character_
    miss_seas$name_de <- NA_character_
    miss_seas$name_es <- NA_character_
    miss_seas$name_fr <- NA_character_
    miss_seas$name_pt <- NA_character_
    countries_det <- rbind(countries_det, miss_seas)
  }
  if (!is.null(excl_coords)) {
    excl_coords$country <- "ERRONEOUS-MISSING_COORDS"
    excl_coords$sovereignt <- NA_character_
    excl_coords$adm0_a3 <- NA_character_
    excl_coords$name_de <- NA_character_
    excl_coords$name_es <- NA_character_
    excl_coords$name_fr <- NA_character_
    excl_coords$name_pt <- NA_character_
    countries_det <- rbind(countries_det, excl_coords)
  }
  countries_det <- countries_det[order(countries_det[, 1]), ]
  comp_df <- cbind(countries_gv, countries_det)
  cat("\n")
  cat("Records to compare: \n")
  print(nrow(comp_df))
  cat("\n")
  print(str(comp_df))
  cat("\n")
  # Removing whitespaces
  for (i in 1:nrow(comp_df)) {
    comp_df$country_gv[i] <- trimws(comp_df$country_gv[i])
    comp_df$country[i] <- trimws(comp_df$country[i])
    comp_df$sovereignt[i] <- trimws(comp_df$sovereignt[i])
    comp_df$name_de[i] <- trimws(comp_df$name_de[i])
    comp_df$name_es[i] <- trimws(comp_df$name_es[i])
    comp_df$name_fr[i] <- trimws(comp_df$name_fr[i])
    comp_df$name_pt[i] <- trimws(comp_df$name_pt[i])
  }
  # Missing given country name
  miss_gvname <- comp_df[is.na(comp_df$country_gv), ]
  # Not applicable country name
  miss_detname <- comp_df[comp_df$country == "MISSING_NAME" |
      comp_df$country == "ERRONEOUS-MISSING_COORDS", ]
  miss_name <- unique(rbind(miss_gvname, miss_detname))
  comp_df <- comp_df[!(comp_df$id %in% miss_name$id), ]
  # Equal country name
  eq_name <- comp_df[comp_df$country_gv == comp_df$country, ]
  comp_df <- comp_df[!(comp_df$id %in% eq_name$id), ]
  # Country name equal to sovereignt
  eq_sovergt <- na.omit(comp_df[comp_df$country_gv == comp_df$sovereignt, ])
  comp_df <- comp_df[!(comp_df$id %in% eq_sovergt$id), ]
  # Country name in other languages
  eq_name_de <- na.omit(comp_df[comp_df$country_gv ==
      comp_df$name_de, ])
  comp_df <- comp_df[!(comp_df$id %in% eq_name_de$id), ]
  eq_name_es <- na.omit(comp_df[comp_df$country_gv ==
      comp_df$name_es, ])
  comp_df <- comp_df[!(comp_df$id %in% eq_name_es$id), ]
  eq_name_fr <- na.omit(comp_df[comp_df$country_gv ==
      comp_df$name_fr, ])
  comp_df <- comp_df[!(comp_df$id %in% eq_name_fr$id), ]
  eq_name_pt <- na.omit(comp_df[comp_df$country_gv ==
      comp_df$name_pt, ])
  comp_df <- comp_df[!(comp_df$id %in% eq_name_pt$id), ]
  eq_name_olang <- rbind(eq_name_de, eq_name_es, eq_name_fr, eq_name_pt)
  # Country name in upper case
  eq_upcase <- na.omit(comp_df[comp_df$country_gv ==
      toupper(comp_df$country), ])
  comp_df <- comp_df[!(comp_df$id %in% eq_upcase$id), ]
  # Equal words
  if (nrow(comp_df) != 0){
  eq_word <- data.frame()
  eq_word_olang <- data.frame()
  for (j in 1:nrow(comp_df)) {
    # Some equal words but in upper case and separated by underscore
    eq_upcaseun <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(toupper(comp_df$country[j]), " ")))
    # Some equal words but separated by underscore and/or in another language
    eq_wordun_en <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(comp_df$country[j], " ")))
    eq_wordun_de <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(comp_df$name_de[j], " ")))
    eq_wordun_es <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(comp_df$name_es[j], " ")))
    eq_wordun_fr <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(comp_df$name_fr[j], " ")))
    eq_wordun_pt <- intersect(unlist(strsplit(comp_df$country_gv[j], "_")),
      unlist(strsplit(comp_df$name_pt[j], " ")))
    # Some equal words but separated by space and/or in another language
    eq_wordsp_en <- intersect(unlist(strsplit(comp_df$country_gv[j], " ")),
      unlist(strsplit(comp_df$country[j], " ")))
    eq_wordsp_de <- intersect(unlist(strsplit(comp_df$country_gv[j], " ")),
      unlist(strsplit(comp_df$name_de[j], " ")))
    eq_wordsp_es <- intersect(unlist(strsplit(comp_df$country_gv[j], " ")),
      unlist(strsplit(comp_df$name_es[j], " ")))
    eq_wordsp_fr <- intersect(unlist(strsplit(comp_df$country_gv[j], " ")),
      unlist(strsplit(comp_df$name_fr[j], " ")))
    eq_wordsp_pt <- intersect(unlist(strsplit(comp_df$country_gv[j], " ")),
      unlist(strsplit(comp_df$name_pt[j], " ")))
    # English
    if (!identical(eq_wordun_en, character(0)) |
        !identical(eq_wordsp_en, character(0)) |
        !identical(eq_upcaseun, character(0))) {
      eq_word <- rbind(eq_word, comp_df[j, ])
    }
    # Other languages
    if (!identical(eq_wordun_de, character(0)) |
        !identical(eq_wordun_es, character(0)) |
        !identical(eq_wordun_fr, character(0)) |
        !identical(eq_wordun_pt, character(0)) |
        !identical(eq_wordsp_de, character(0)) |
        !identical(eq_wordsp_es, character(0)) |
        !identical(eq_wordsp_fr, character(0)) |
        !identical(eq_wordsp_pt, character(0))) {
      eq_word_olang <- rbind(eq_word_olang, comp_df[j, ])
    }
  }
  eq_word_name <- unique(rbind(eq_word, eq_word_olang))
  comp_df <- comp_df[!(comp_df$id %in% eq_word_name$id), ]
  }
  # Set of 3 letters equal at the beginning or at the end of the words
  if (nrow(comp_df) != 0){
  eq_word_beg <- data.frame()
  eq_word_end <- data.frame()
  for (k in 1:nrow(comp_df)) {
    eq_beg <- grepl(substr(comp_df$country_gv[k], 1, 3), comp_df$country[k])
    if (eq_beg) {
      eq_word_beg <- rbind(eq_word_beg, comp_df[k, ])

    }
    first <- as.integer(nchar(comp_df$country_gv[k]) - 3)
    lastl <- nchar(comp_df$country_gv[k])
    eq_end <- grepl(substr(comp_df$country_gv[k], first, lastl),
      comp_df$country[k])
    if (eq_end) {
      eq_word_end <- rbind(eq_word_end, comp_df[k, ])
    }
  }
  # eq_subword ok
  eq_subword <- unique(rbind(eq_word_beg, eq_word_end))
  comp_df <- comp_df[!(comp_df$id %in% eq_subword$id), ]
  }
  # Falls into the sea
  if (nrow(comp_df) != 0){
    sep_words <- strsplit(comp_df$country, " ")
    water <- c()
    for (l in 1:length(sep_words)) {
      ocean_cpsl <- sum(unlist(sep_words[l]) == "OCEAN")
      ocean <- sum(unlist(sep_words[l]) == "Ocean")
      sea <- sum(unlist(sep_words[l]) == "Sea")
      gulf <- sum(unlist(sep_words[l]) == "Gulf")
      bay <- sum(unlist(sep_words[l]) == "Bay")
      channel <- sum(unlist(sep_words[l]) == "Channel")
      canal <- sum(unlist(sep_words[l]) == "Canal")
      river <- sum(unlist(sep_words[l]) == "River")
      strait <- sum(unlist(sep_words[l]) == "Strait")
      sound <- sum(unlist(sep_words[l]) == "Sound")
      fjord <- sum(unlist(sep_words[l]) == "Fjord")
      basin <- sum(unlist(sep_words[l]) == "Basin")
      inlet <- sum(unlist(sep_words[l]) == "Inlet")
      lake <- sum(unlist(sep_words[l]) == "Lake")
      water[l] <- sum(ocean_cpsl, ocean, sea, gulf, bay, channel, canal, river,
        strait, sound, fjord, basin, inlet, lake)
    }
    into_the_sea <- comp_df[as.logical(water), ]
    comp_df <- comp_df[!(comp_df$id %in% into_the_sea$id), ]
  }
  # Output directory
  if (!file.exists("txt-compare_country")) {
    dir.create("txt-compare_country")
  }
  # Outputs
  if (nrow(miss_name) != 0) {
    write.table(miss_name,
      file = "txt-compare_country/missing_given_or_determined_country.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  if (nrow(eq_name) != 0) {
    write.table(eq_name,
      file = "txt-compare_country/equal_country.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  if (nrow(eq_sovergt) != 0) {
    write.table(eq_sovergt,
      file = "txt-compare_country/country_equals_sovereignt.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  if (nrow(eq_name_olang) != 0) {
    write.table(eq_name_olang,
      file = "txt-compare_country/equal_country_other_language.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  if (nrow(eq_upcase) != 0) {
    write.table(eq_upcase,
      file = "txt-compare_country/equal_country_upper_case.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  if (exists("eq_word_name") && nrow(eq_word_name) != 0) {
    write.table(eq_word_name,
      file = "txt-compare_country/equal_word_en_or_other_language.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    eqwn <- nrow(eq_word_name)
  } else {
    eqwn <- 0
  }
  if (exists("eq_subword") && nrow(eq_subword) != 0) {
    write.table(eq_subword,
      file = "txt-compare_country/equal_character_set_country.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    eqsw <- nrow(eq_subword)
  } else {
    eqsw <- 0
  }
  if (exists("into_the_sea") && nrow(into_the_sea) != 0) {
    write.table(into_the_sea,
      file = "txt-compare_country/into_the_sea.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    itsea <- nrow(into_the_sea)
  } else {
    itsea <- 0
  }
  # Different country
  if (nrow(comp_df) != 0){
    write.table(comp_df,
      file = "txt-compare_country/different_country.txt",
      row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  }
  report <- cbind(c("Missing given or determined country name: ",
    "Equal country name: ",
    "Given country name equals to sovereignty: ",
    "Given country name in another language: ",
    "Given country name in upper case: ",
    "Given country name with some equal words - English or another language: ",
    "Set of 3 letters equals at the beginning or at the end of the words: ",
    "Points that fall into the sea: ",
    "Different country name: "),
    c(nrow(miss_name), nrow(eq_name), nrow(eq_sovergt), nrow(eq_name_olang),
      nrow(eq_upcase), eqwn, eqsw, itsea, nrow(comp_df)))
  cat("\n")
  cat("Please check the directory \\txt-compare_country.\n\n")
  return(as.table(report))
}

