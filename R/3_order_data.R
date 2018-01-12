#'
#' Reorder the data by the coordinate pairs inicial order
#'
#' Concatenate all the files with country names and/or marine region names, i.
#' e, the output of \code{\link{get_country}}, \code{\link{get_marine}} and
#' \code{\link{get_country_sea}} and reorder the data by the coordinate pairs
#' inicial order.
#'
order_data <- function() {
    df <- data.frame()
    if (file.exists("countries.txt")) {
        df1 <- read.table("countries.txt", header = TRUE, sep = "\t", quote = "")
        # Change the name of the last column of each data.frame to 'geo_name'
        names(df1)[4] <- "geo_name"
        # colnames(df1) <- c('id', 'longitude', 'latitude', 'geo_name') Concatenate the data.frames by row
        df <- rbind(df, df1)
        # Order the data.frame by the 'id' column
        final_df <- df[order(df[, 1]), ]
    }
    if (file.exists("marine_regions.txt")) {
        df2 <- read.table("marine_regions.txt", header = TRUE, sep = "\t", quote = "")
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
    write.csv(final_df, file = "geographic_names.csv")
    print("Check the output files < geographic_names.txt > and < geographic_names.csv >.")
}
