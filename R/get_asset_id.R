#' Get a list of asset IDs for the authenticated account.
#'
#' @inherit kobo_api
#'
#' @return A named list of asset IDs
#'
#' @export get_asset_id_list

get_asset_id_list <- function(api = c("humanitarian_response", "kobotoolbox"),
                              auth_type = c("client", "token"),
                              auth_key = NULL){
  auth_type <- rlang::arg_match(auth_type)

  assets_path <- paste0("/assets")

  assets_raw <- kobo_api(assets_path,
                         api = api,
                         auth_type = auth_type,
                         auth_key = auth_key)

  assets_raw_results <- purrr::map_chr(assets_raw[["content"]][["results"]],  "uid")
  names(assets_raw_results) <- purrr::map_chr(assets_raw[["content"]][["results"]], "name")

  assets_raw_results
}

#' Get a data frame with of assets information for the authenticated account.
#'
#' @inherit kobo_api
#'
#' @return A data frame with asset IDs, names, type, and deployment status.
#'
#' @export get_asset_id_df
get_asset_id_df <- function(api = c("humanitarian_response", "kobotoolbox"),
                            auth_type = c("client", "token"),
                            auth_key = NULL){
  auth_type <- rlang::arg_match(auth_type)

  assets_path <- paste0("/assets")

  assets_raw <- kobo_api(assets_path,
                         api = api,
                         auth_type = auth_type,
                         auth_key = auth_key)
  purrr::map(assets_raw$content$results,`[`, c("uid", "name", "asset_type", "has_deployment")) %>%
    dplyr::bind_rows()
}
