#' @export
search_maps <- function(params) {
  # Define url
  base_url <- "https://trackmania.exchange/api/maps?"
  full_url <- httr::modify_url(base_url, query = params)

  # Send GET request
  response <- httr::GET(url = full_url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

  # Check the result
  if (httr::status_code(response) == 200) {
    # Parse the JSON content
    data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    return(data)
  } else {
    warning("Request failed with status code: ", httr::status_code(response))
  }
}
