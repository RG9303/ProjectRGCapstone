
#' Format to location in the database
#'
#' @description The function \code{eq_location_clean} clean earthquake location data.
#'
#' @usage eq_location_clean(location)
#'
#' @param location charater string with location and country followed by a colon.
#'
#' @return This function returns a character string in title case with the country and colon removed.
#'
#' @source This capstone project will be centered around a dataset obtained from the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) on significant earthquakes around the world. This dataset contains
#' information about 5,933 earthquakes over an approximately 4,000 year time span.
#'
#' @importFrom tools toTitleCase
#'
#' @examples \dontrun{eq_location_clean("JAPAN:  TUWANO")}
#'
#' @export
#'
eq_location_clean <- function (location) {
  tools::toTitleCase(trimws(tolower(sub("\\S+: +", "", location))))
}






#' Clean database NOAA
#'
#' @description The function \code{eq_clean_data} read a database and returns a database with special formats of some columns.
#'
#' @usage eq_clean_data(earthquakes)
#'
#' @param earthquakes to enter a database of NOAA.
#'
#' @return if file exists, this function read the file and return a database as a data frame where date, latitude and longitude
#' have special formats.
#'
#' @source This capstone project will be centered around a dataset obtained from the U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) on significant earthquakes around the world. This dataset contains
#' information about 5,933 earthquakes over an approximately 4,000 year time span.
#'
#' @examples \dontrun{eq_clean_data("earthquakes.tsv.gz")}
#'
#' @seealso \code{\link{eq_location_clean}}
#'
#' @export
#'
eq_clean_data <- function (earthquakes) {
  result <- data.frame(earthquakes)
  result$DATE <- base::as.Date(base::mapply(to_date, result$YEAR, result$MONTH,
                                            result$DAY), origin = as.Date("1970-01-01"))
  result$LATITUDE <- as.numeric(result$LATITUDE)
  result$LONGITUDE <- as.numeric(result$LONGITUDE)
  result$EQ_PRIMARY <- as.numeric(result$EQ_PRIMARY)
  result$TOTAL_DEATHS <- as.numeric(result$TOTAL_DEATHS)
  result$LOCATION_NAME <- eq_location_clean(result$LOCATION)
  result
}
