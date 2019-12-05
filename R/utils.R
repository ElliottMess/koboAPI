#' @name nullToNA
#' @rdname nullToNA
#' @title  Replaces NULL values by NAs
#' @description Replaces NULL values by NAs
#' @param x Vector to be treated
#' @return Returns the vector with NULL replaced
#' @author Elliott Messeiller
#'
#' @export AddstartCol_SelectMultiple

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

replace_x <- function(x, replacement = NA_character_) {
  if (length(x) == 0 || length(x[[1]]) == 0) {
    replacement
  } else {
    x
  }
}
