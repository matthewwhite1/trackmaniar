### TODO: Make possible to search for multiple ids - may have to change away from modify_url

#' @export
search_maps <- function(params, fields) {
  # Define fields if not provided
  if (missing(fields)) {
    fields <- "Name,MapId,Authors[],MapType,Difficulty"
    fields <- paste0(fields, ",AwardCount,UploadedAt,OnlineWR[]")
    fields <- paste0(fields, ",DownloadCount,Tags[]")
  }
  params <- as.list(params)
  params$fields <- fields

  # Define url
  base_url <- "https://trackmania.exchange/api/maps?"
  full_url <- httr::modify_url(base_url, query = params)

  # Send GET request
  response <- httr::GET(url = full_url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

  # Check the result
  if (httr::status_code(response) == 200) {
    # Parse the JSON content
    data <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
    df <- as.data.frame(data$Results)
  } else {
    warning("Request failed with status code: ", httr::status_code(response))
  }

  # Split author column up if it exists
  if (stringr::str_detect(fields, "Authors")) {
    df <- df |>
      tibble::add_column(AuthorId = paste(df$Authors[[1]]$User$UserId, collapse = ", "), .after = 1) |>
      tibble::add_column(AuthorName = paste(df$Authors[[1]]$User$Name, collapse = ", "), .after = 2) |>
      tibble::add_column(AuthorRole = paste(df$Authors[[1]]$Role, collapse = ", "), .after = 3) |>
      dplyr::select(-Authors)
  }

  # Edit WR columns
  if (stringr::str_detect(fields, "OnlineWR")) {
    df$WRDisplayName <- df$OnlineWR$DisplayName
    df$WRRecordTime <- df$OnlineWR$RecordTime
    df <- df |>
      dplyr::select(-OnlineWR)
  }

  # Split tags column up if it exists
  if (stringr::str_detect(fields, "Tags")) {
    df$TagId <- paste(df$Tags[[1]]$TagId, collapse = ", ")
    df$TagName <- paste(df$Tags[[1]]$Name, collapse = ", ")
    df$TagColor <- paste(df$Tags[[1]]$Color, collapse = ", ")
    df <- df |>
      dplyr::select(-Tags)
  }

  return(df)
}
