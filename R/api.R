#' Calls the SSL API
#'
#' @param url The url to the SSL API.
#' @param ... Additional queries for the API call.
#' @return A data frame of the data.
#' @export
#' @examples
#' readAPI(
#'    url = "http://143.198.159.1/ssl/leaders",
#'    query = list(league = 1, season = 5)
#' )

readAPI <- function(url, ...){
  require("dplyr")

  temp <-
    url %>%
    # Gets the API information, the ... allows for specific queries with query = list()
    httr::GET(...)

  temp$content %>%
    # Extracts the data
    rawToChar() %>%
    # Converts it from JSON to a data frame
    jsonlite::fromJSON() %>%
    return()
}
