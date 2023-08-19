#' Download data from the Skanderbeg API
#'
#' @param saves A vector of Skanderbeg save strings (ID at the end of the URL)
#' @param api_key A string with your Skanderbeg API key
#' @param values A vector of values from the Skanderbeg getCountryData API
#'
#' @return A dataframe
#' @export
download_game_data <- function(saves, api_key, values) {
  core_api_strings <- "player;was_player;tag;hex;"
  api_formatted_values <- paste0(values, collapse = ";")
  dfs <- list()
  for(i in 1:length(saves)) {
    r <- httr::GET(
      paste0(
        "https://skanderbeg.pm/api.php?key=", api_key,
        "&scope=getCountryData&save=", saves[i],
        "&value=", core_api_strings, api_formatted_values,
        "&format=json"
      )
    )
    d <- httr::content(r) |> 
      purrr::map_df(purrr::flatten_df) |> 
      dplyr::mutate(session = i) |> 
      dplyr::mutate(dplyr::across(all_of(values), as.numeric))
    dfs[[i]] <- d
  }

  final_df <- dplyr::bind_rows(dfs)

  return(final_df)
}
