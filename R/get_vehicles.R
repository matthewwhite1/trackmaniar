#' Get all vehicles
#'
#' @return A character list of vehicles. The order starts with the official
#'   vehicles (that have their own environment) by when they were introduced
#'   and afterwards the custom vehicles in descending order of occurrence count
#'
#' @export
get_vehicles <- function() {
  # Define url
  url <- "https://trackmania.exchange/api/meta/vehicles"

  # Send GET request
  response <- httr::GET(url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

  # Return map types
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(data)
}
