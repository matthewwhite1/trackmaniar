#' Download a map's image and/or thumbnail
#'
#' @param id Integer: The MapId of the entry to get an image from
#' @param position Numeric vector: The 0-based index of the image. Default is 0.
#'   Can be a vector of multiple image indices
#'
#' @return Downloads map image png to working directory
#'
#' @examples
#' # Download the image from Training - 01
#' download_image(id = 1984)
#' @export
download_image <- function(id, position = 0) {
  for (pos in position) {
    # Define url and output path
    url <- paste0("https://trackmania.exchange/mapimage/", id, "/", pos)
    name <- get_map_name(id)
    output_path <- paste0(name, "_", pos, ".png")

    # Send GET request
    response <- httr::GET(url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

    # Check the result
    if (httr::status_code(response) == 200) {
      writeBin(httr::content(response, "raw"), output_path)
      message("Image downloaded successfully to: ", output_path)
    } else if (httr::status_code(response) == 404) {
      warning("No image or thumbnail found at given position.")
    } else {
      warning("Failed to download map. Status code: ", httr::status_code(response))
    }
  }
}
