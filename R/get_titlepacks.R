#' Get all titlepacks
#'
#' @return A character vector of titlepacks in descending order of
#'   occurrence count
#'
#' @export
get_titlepacks <- function() {
  # Define url
  url <- "https://trackmania.exchange/api/meta/titlepacks"

  # Send GET request
  response <- httr::GET(url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

  # Return map types
  data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  return(data)
}
