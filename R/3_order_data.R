#'
#' Reorder the Data by the Coordinate Pairs Inicial Order.
#'
#' Concatenate all the files with country names and/or sea names, i. e, the
#' output of \code{\link{get_country}}, \code{\link{get_sea}} and
#' \code{\link{get_country_sea}} and reorder the data by the coordinate pairs
#' inicial order.
#'
#' @details
#' \itemize{
#' \item The input are the files 'countries.txt', 'seas.txt' and
#' 'countries_seas.txt', created by the functions \code{\link{get_country}},
#' \code{\link{get_sea}} and \code{\link{get_country_sea}}, respectively.
#' However, only requires one or two of the files;
#' \item The function changes the name of the last column of each input file to
#' 'geo_name' and then orders the data by the 'id' column;
#' \item The output is a .txt file named 'geographic_names.txt' with the
#' coordinate pairs by the inicial order. The header is: 'id' 'longitude'
#' 'latitude' 'geo_name'.
#' }
#'
#' @examples
#' \dontrun{
#' ##
#' ## Execute one of the examples of \code{\link{get_country}} and then run the
#' ## three functions below sequentially:
#' get_sea()
#' get_country_sea()
#' order_data()
#' }
#'
#' @export
order_data <- function() {
    df <- data.frame()
    if (file.exists("countries.txt")) {
        df1 <- read.table("countries.txt", header = TRUE, sep = "\t", quote = "")
        names(df1)[4] <- "geo_name"
        df <- rbind(df, df1)
        final_df <- df[order(df[, 1]), ]
    }
    if (file.exists("seas.txt")) {
        df2 <- read.table("seas.txt", header = TRUE, sep = "\t", quote = "")
        names(df2)[4] <- "geo_name"
        df <- rbind(df, df2)
        final_df <- df[order(df[, 1]), ]
    }
    if (file.exists("countries_seas.txt")) {
        df3 <- read.table("countries_seas.txt", header = TRUE, sep = "\t", quote = "")
        names(df3)[4] <- "geo_name"
        df <- rbind(df, df3)
        final_df <- df[order(df[, 1]), ]
    }
    write.table(final_df, file = "geographic_names.txt", row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
    cat("Please, check the output file 'geographic_names.txt'.\n")
}
