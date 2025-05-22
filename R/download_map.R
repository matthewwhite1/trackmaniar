#' @export
download_map <- function(ids) {
  for (id in ids) {
    # Define url and output path
    url <- paste0("https://trackmania.exchange/mapgbx/", id)
    name <- get_map_name(id)
    output_path <- paste0(name, ".Map.Gbx")

    # Send GET request
    response <- httr::GET(url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"),
                         httr::write_disk(output_path, overwrite = TRUE))

    # Check the result
    if (httr::status_code(response) == 200) {
      message("Map downloaded successfully to: ", output_path)
    } else {
      warning("Failed to download map. Status code: ", httr::status_code(response))
    }
  }
}
