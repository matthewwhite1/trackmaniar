#' @export
get_maptypes <- function() {
  # Define url
  url <- "https://trackmania.exchange/api/meta/maptypes"

  # Send GET request
  response <- httr::GET(url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

  # Return map types
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(data)
}
