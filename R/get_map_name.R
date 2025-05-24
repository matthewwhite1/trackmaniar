#' Get the name of a map from a MapId
#'
#' @param ids Numeric vector: The MapId of the map. Can be a vector of multiple
#'   MapIds
#'
#' @return A character vector containing the map name(s)
#'
#' @examples
#' # Get the the map name from id 1984
#' get_map_name(ids = 1984)
#' @export
get_map_name <- function(ids) {
  names <- character()
  for (id in ids) {
    # Define url
    base_url <- "https://trackmania.exchange/api/maps?"
    id <- list(id = id, fields = "Name")
    full_url <- httr::modify_url(base_url, query = id)

    # Send GET request
    response <- httr::GET(url = full_url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

    # Check the result
    if (httr::status_code(response) == 200) {
      # Parse the JSON content
      data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      name <- as.character(data$Results)
      names <- c(names, name)
    } else {
      warning("Request failed with status code: ", httr::status_code(response))
    }
  }
  return(names)
}
