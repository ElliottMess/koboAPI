#' Get URL of api
#'
#' @inheritParams get_form
#'
#' @return character vector with the URL of the api
get_api_url <- function(api){
  assert_api(api)
  convenience_api_names <- c("humanitarian_response", "kobotoolbox")
  if(api == "humanitarian_response"){
    "https://kobo.humanitarianresponse.info/api/v2"
  }else if(api == "kobotoolbox"){
    "https://kf.kobotoolbox.org/api/v2"
  }else{
    api
  }
}
