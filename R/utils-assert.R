#' Assert that arguments passed in are length 1 character vectors
#'
#' @param ... Character vectors to check
assert_strings <- function(...) {
  arg_names <- sys.call()[-1]
  args <- list(...)
  classes <- sapply(args, class)
  if (!all(classes == "character")) {
    stop(sprintf(
      "%s must be a character vector of length one, not %s",
      paste(arg_names[!classes == "character"], collapse = ", "),
      paste(classes[!classes == "character"], collapse = ", ")
    ),
    call. = FALSE
    )
  }
  lens <- sapply(args, length)
  if (!all(lens == 1)) {
    stop(sprintf(
      "%s must be of length one, not length %s",
      paste(arg_names[lens != 1], collapse = ", "),
      paste(lens[lens != 1], collapse = ", ")
    ),
    call. = FALSE
    )
  }
}

#' Assert that the api is valid
#'
#' @param api (character) api to check

assert_api <- function(api){
  if(length(api) > 1){
    api <- api[1]
    warning(sprintf(
      "`api` has length > 1. The first element will be used: %s",
      api),
      call.= FALSE
    )
  }
  convenience_api_names <- c("humanitarian_response", "kobotoolbox")
  if (class(api) != "character") {
    stop(
      sprintf("%s must be a character vector of length one, not %s", api, class(api)),
      call. = FALSE
    )
  }
  if(!stringr::str_starts(api, "https://") & !api %in% convenience_api_names){
    stop(
      sprintf("'api' must either one of %s or starts with 'https:://', not %s",
              paste0("`", convenience_api_names, "`", collapse = " or "), api),
      call. = FALSE
    )
  }
  return(api)
}
