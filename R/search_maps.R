### TODO: Make possible to search for multiple ids - may have to change away from modify_url

#' Search maps
#'
#' @param params A named list of input parameters. The full list of parameters
#'   can be seen in the source section
#' @param fields A string of output parameters. The full list of parameters
#'   can be seen in the source section. See the example for how this should
#'   be structured
#'
#' @details If no fields are provided, the default fields in the output are
#'   Name, MapId, Authors, MapType, Difficulty, AwardCount, UploadedAt,
#'   OnlineWR, DownloadCount, and Tags
#'
#' @return A dataframe with columns being the output parameters listed in
#'   the fields argument
#'
#' @source \url{https://api2.mania.exchange/search?version=2&s=2&q=Search\%20Maps}
#'
#' @examples
#' # Search 50 Ubisoft Nadeo maps
#' params <- list(author = "Ubisoft Nadeo", count = 50)
#' fields <- "Name,MapId,Authors[],AwardCount,Difficulty"
#' search_maps(params = params, fields = fields)
#' @export
search_maps <- function(params, fields) {
  # Define fields if not provided
  if (missing(fields)) {
    fields <- "Name,MapId,Authors[],MapType,Difficulty"
    fields <- paste0(fields, ",AwardCount,UploadedAt,OnlineWR[]")
    fields <- paste0(fields, ",DownloadCount,Tags[]")
  }
  params$fields <- fields

  # Define url
  base_url <- "https://trackmania.exchange/api/maps?"
  param_names <- names(params)
  for (i in seq_along(params)) {
    for (j in seq_along(params[[i]])) {
      base_url <- paste0(base_url, param_names[i], "=", params[[i]][j])
      if (j != length(params[[i]])) {
        base_url <- paste0(base_url, "&")
      }
    }
    if (i != length(params)) {
      base_url <- paste0(base_url, "&")
    }
  }
  base_url <- stringr::str_replace(base_url, " ", "%20")
  print(base_url)

  # Send GET request
  response <- httr::GET(url = base_url, httr::add_headers(`User-Agent` = "trackmaniar/1.0"))

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
