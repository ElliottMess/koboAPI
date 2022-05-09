#' Access to kobo-style APIs
#'
#' The function provides a high-level access to APIs based on kobotoolbox. It
#' parses the base URL for the ´api´ and returns the JSON response in a standard
#' format.
#'
#' @param path (character) additional path to based to the base API URL. It should
#'     start with a `/`.
#' @param auth_type (character) Two types of authentication are possible:
#'    - `client`: use the `auth_key` `user:password` to authenticate to the kobo API
#'    - `token`: uses the `token` provided in `auth_key` to authenticate
#' @param auth_key (character) Can either be:
#'    - A combination of the kobo and password in format: `{user}:{password}` (e.g. 'my_user:my_password')
#'    - A kobo API V2 token. See the \href{kobotoolbox document for more information}{https://support.kobotoolbox.org/api.html}
#' @param api (character) The type of API to be used or URL at which the API can be accessed. Can be either:
#'    - `humanitarian_response`: "https:://kobo.humanitarianresponse.info" will be used as the base URL (default)
#'    - `kobotoolbox`: "https:://https://kf.kobotoolbox.org" will be used as the base URL
#'    - `custom`: custom URL. Must at least start with "https://"
#'
#' @return a `kobo_api` object consisting of a list of:
#'     - `content`: parsed content from the request
#'     - `path`:
#'     - `response`: JSON response to request
#'
#' @export kobo_api
kobo_api <- function(path,
                     api = c("humanitarian_response", "kobotoolbox"),
                     auth_type = c("client", "token"),
                     auth_key = NULL) {

  assert_strings(path)
  auth_type <- rlang::arg_match(auth_type)
  api <- assert_api(api)
  api_url <- get_api_url(api)

  ua <- httr::user_agent("http://github.com/elliottmess/koboAPI")

  url <- paste0(url = api_url, path = path)

  resp <- httr::GET(url, authenticate_api(auth_type, auth_key, api),
                    httr::add_headers(Accept = "application/json"))

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "kobo API request failed [%s]\n%s>",
        httr::status_code(resp),
        parsed$detail
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "kobo_api"
  )
}

#' Print function for kobo_api object
#'
#' @param x kobo_api object
#' @param ... additional parameters
#'
#' @noRd
print.kobo_api <- function(x, ...) {
  cat("<kobo_api ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}
