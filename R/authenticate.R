#' Authentication for kobo API
#'
#' Retrieves token if not provided in `auth_token`, then add a `Authorization`
#' header via \code{\link[httr]{add_headers}} to the API call.
#'
#' All authentication is based on token.
#'
#' @inheritParams kobo_api
#'
#' @return `Authorization` header call via \code{\link[httr]{add_headers}}
authenticate_api <- function(auth_type, auth_key = NULL, api) {
  token <- get_raw_token(auth_type, auth_key, api)
  httr::add_headers(Authorization = paste("Token", token, sep = " "))
}

#' Get token for authentication
#'
#' `get_raw_token` retrieves the authentication token based on the information
#' provided in `auth_type` and `auth_key`. Three scenarios are handled:
#'  - `auth_type` is `client` and `auth_key` provides a valid combination of
#'  user and password: '{user}:{password}'
#'  - `auth_type` is `token` and `auth_key` is non-NULL
#'  - `auth_key` is NULL: token is sought in the user's envrionment.
#'
#' @inheritParams kobo_api
#'
#' @return string with client token
#'
#' @noRd
get_raw_token <- function(auth_type, auth_key, api) {
  if(!is.null(auth_key)){
    if (auth_type == "client") {
      token <- get_client_token(auth_key, api)
    } else if (auth_type == "token") {
      token <- auth_key
    }else{
      stop("`auth_type` unsupported. Please use either `client` or `token`")
    }
  }else{
    token <- NULL
  }
  if(is.null(token)){
    refresh_token(auth_type, auth_key, api)
    retrieve_token(api)
  }
}

#' Get token for client information provided in `auth_key`
#'
#' @inheritParams kobo_api
#'
#' @return string with client token
#'
#' @noRd
get_client_token <- function(auth_key, api) {
  user <- stringr::str_extract(auth_key, "^.*(?=:)")

  password <- stringr::str_extract(auth_key, "(?<=:).*$")

  api_url <- get_api_url(api)

  httr::GET(paste(api_url, "token/?format=json", sep = "/"),
            httr::authenticate(user, password)) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON() %>%
    unlist()
}


#' Retrieve token for specified API from the environment
#'
#' @inheritParams kobo_api
#'
#' @return string with client token
#'
#' @noRd
retrieve_token <- function(api) {
  nm <- server_to_token_name(api)
  get(nm, envir = .koboapi_env)
}

#' Refresh token for specified API from the environment
#'
#' @inheritParams kobo_api
#'
#' @return Assigned token in environment
#'
#' @noRd
refresh_token <- function(auth_type, auth_key, api) {
  nm <- server_to_token_name(api)
  valid <- check_koboapi_env(nm)
  if(valid == FALSE){
    token <- get_envir_token(nm)
    assign(nm, token, envir = .koboapi_env)
  }
}

#' Get token from environment
#'
#' @inheritParams kobo_api
#'
#' @param nm name of environment token
#'
#' @noRd
get_envir_token <- function(nm){
  token <- Sys.getenv(nm)
  if(token == "" | is.null(token)){
    stop(sprintf("%s was not found in the enviroment. Please make sure it's present in .Renviron.", nm))
  }
  return(token)
}

#' Get name of the token in environment
#'
#' @inheritParams kobo_api
#'
#' @return string with name of token
#'
#' @noRd
server_to_token_name <- function(api) {
  tokens <- c("HUMANITARIAN_RESPONSE_TOKEN", "KOBOTOOLBOX_TOKEN")
  servers <- c("humanitarian_response", "kobotoolbox")
  tokens[match(api, servers)]
}

#' Check environment for the token variable
#'
#' @inheritParams get_envir_token
#'
#' @noRd
check_koboapi_env <- function(nm) {
  ex <- exists(nm, envir = .koboapi_env, inherits = FALSE)
  if (ex) {
    token <- get(nm, envir = .koboapi_env, inherits = FALSE)
  } else {
    ex
  }
}
