library(httr)

# Define the URL and output path
map_id <- 234465
url <- paste0("https://trackmania.exchange/mapgbx/", map_id)
output_path <- paste0("track_", map_id, ".Map.Gbx")  # Change extension if needed

# Send GET request
response <- GET(url, add_headers(`User-Agent` = "YourAppName/1.0"), write_disk(output_path, overwrite = TRUE))

# Check the result
if (status_code(response) == 200) {
  message("Map downloaded successfully to: ", output_path)
} else {
  warning("Failed to download map. Status code: ", status_code(response))
}
