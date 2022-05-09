#' Download form for a specific asset ID
#'
#' @param asset_id (character) The ID of the form to be accessed (as a character string). Must be a KPI-type ID (not api V1 ID). See download_forms_all for more details.
#' @inheritParams kobo_api
#'
#' @return A list of three data frames that correspond to the structure of a XLSform:
#'    - "survey": a data frame with all the questions variables.
#'    - "choices" a data frame with all the choices variables.
#'    - "settings" a data frame with the basic settings for the form.
#'
#' @export get_form
#'
get_form <- function(asset_id,
                     api = c("humanitarian_response", "kobotoolbox"),
                     auth_type = c("client", "token"),
                     auth_key = NULL) {

  assert_strings(asset_id)
  auth_type <- rlang::arg_match(auth_type)

  form_path <- paste0("/assets/", asset_id, "/")

  form_raw <- kobo_api(form_path,
                       api = api,
                       auth_type = auth_type,
                       auth_key = auth_key)

  form_raw_content <- form_raw$content

  languages <- unlist (form_raw_content$content$translations)

  cols_with_multi_languages <- unlist(form_raw_content$content$translated)

  languages_labels <- purrr::map(cols_with_multi_languages, ~paste0(.x, "_", rep(1:length(languages)))) %>%
    unlist()

  names(languages_labels) <-  purrr::map(cols_with_multi_languages, ~paste0(.x, "::", languages)) %>%
    unlist()

  survey <- dplyr::bind_rows(form_raw_content$content$survey) %>%
    tidyr::unnest_wider(dplyr::any_of(cols_with_multi_languages), names_sep = "_") %>%
    dplyr::rename(dplyr::any_of(languages_labels)) %>%
    dplyr::relocate(dplyr::starts_with("$"), .after = dplyr::last_col())

  choices <- dplyr::bind_rows(form_raw_content$content$choices) %>%
    tidyr::unnest_wider(dplyr::any_of(cols_with_multi_languages), names_sep = "_") %>%
    dplyr::rename(dplyr::any_of(languages_labels)) %>%
    dplyr::relocate(dplyr::starts_with("$"), .after = dplyr::last_col())

  settings <- data.frame(form_title = form_raw_content[["name"]],
                         form_id = form_raw_content[["uid"]],
                         version = form_raw_content[["version_id"]],
                         default_language =form_raw_content[["summary"]][["default_translation"]])

  form <- list("survey" = survey, "choices" = choices, "settings" = settings)

  return(form)
}
