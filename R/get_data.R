#' Download data of an asset
#'
#' Download responses from the API for a specific asset
#'
#' @inherit get_form
#'
#' @return A data frame with the responses data for ´asset_id´
#'
#' @export get_data

get_data <- function(asset_id,
                     api = api,
                     auth_type = c("client", "token"),
                     auth_key = NULL){
  assert_strings(asset_id)
  auth_type <- rlang::arg_match(auth_type)

  asset_data_path <- paste0("/assets/", asset_id, "/data/")

  asset_data_raw <- kobo_api(asset_data_path,
                             api = api,
                             auth_type = auth_type,
                             auth_key = auth_key)

  asset_data_raw$content$results %>%
    purrr::map(~ purrr::compact(.)) %>% purrr::keep(~length(.) != 0) %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::distinct()
}
