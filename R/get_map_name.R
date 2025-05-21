#' @export
get_map_name <- function(id) {
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
    return(name)
  } else {
    warning("Request failed with status code: ", httr::status_code(response))
  }
}
