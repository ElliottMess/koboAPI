#' @name download_form
#' @rdname download_form
#' @title  Download form from the platform
#'
#' @description Download form from the platform
#'
#' @param formid The ID of the form to be accessed (as a character string). Must be a KPI-type ID (not api V1 ID). See download_forms_all for more details.
#' @param user Optional. A single string indicating the username
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#'
#' @return A list with two objects: The "survey" sheet as a dataframe with all the questions variables, and the "choices" sheet as a dataframe with all the choices variables.
#'
#' @author Elliott Messeiller
#'
#' @export download_form
#'

download_form <- function(formid, user, pwd =NULL, api="https://kobo.humanitarianresponse.info") {
  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  url_form <- paste0(api, "/assets/", formid, "/")
  raw_form <- GET(url_form, authenticate(user, pwd), progress())
  raw_form_text <- content(raw_form, "text", encoding = "UTF-8")
  raw_form_text_json <- fromJSON(raw_form_text)

  languages <- as.vector(raw_form_text_json$content$translations)
  languages_labels <- paste0("label::", languages)


  choices <- as.data.frame(raw_form_text_json$content$choices)%>%
    purrr::modify_depth(2, replace_x, replacement = c(rep("NA", length(languages_labels))))%>%
    dplyr::mutate(label = purrr::map(label, setNames, languages_labels))%>%
    unnest_wider(label)

  survey <- as.data.frame(raw_form_text_json$content$survey)%>%
    purrr::modify_depth(2, replace_x, replacement = c(rep("NA", length(languages_labels))))%>%
    dplyr::mutate(label = purrr::map(label, setNames, languages_labels))%>%
    unnest_wider(label)

  form <- list("survey" = survey, "choices" = choices)
  return(form)
}

#' @name download_forms_all
#' @rdname download_forms_all
#' @title  Returns a dataframe with all forms available for the user
#'
#' @description Download form from the platform
#'
#' @param user Optional. A single string indicating the username
#' @param pwd Password of the Kobo account to use
#' @param api The URL at which the API can be accessed. Default to "https://kobo.humanitarianresponse.info". "https://kf.kobotoolbox.org" is also supported.
#'
#' @return Dataframe with all forms available for the user.
#'
#' @author Elliott Messeiller
#'
#' @export download_forms_all


download_forms_all <- function(user,pwd, api = "https://kobo.humanitarianresponse.info"){
  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")
  api_type <- api_type(api)

  if(pwd == "") stop("No password entered.")

  download_forms_all <- GET(paste0(api, "/assets/"), authenticate(user, pwd), progress())
  if(download_forms_all$status_code == 403){
    stop('Error 403 Forbidden: probably wrong username or password'
    )
  }
  download_forms_all <- download_forms_all%>%
    content("text", encoding = "UTF-8")%>%
    fromJSON()

  download_forms_all_df <-  as.data.frame(download_forms_all$results)%>%
    filter(has_deployment == TRUE)%>%
    select(name, uid, date_created, date_modified, deployment__submission_count)

  if(api_type == "kobotoolbox.org"){
    old_api <- "https://kc.kobotoolbox.org/api/v1/data"
  }
  if(api_type == "humanitarianresponse.info"){
    old_api <- "https://kc.humanitarianresponse.info/api/v1/data"
  }
  download_forms_all_old <- GET(old_api, authenticate(user, pwd), progress())
  download_forms_all_old <- content(download_forms_all_old, "text", encoding = "UTF-8")
  download_forms_all_old <- fromJSON(download_forms_all_old)
  download_forms_all_old <- as.data.frame(download_forms_all_old)%>%
    mutate(old_id = id)%>%
    select(old_id, id_string)

  download_forms_all_df <- left_join(download_forms_all_df, download_forms_all_old, by = c("uid" = "id_string"))


  return(download_forms_all_df)
}

#' @name all_exports
#' @rdname all_exports
#' @title  Downloads forms from KPI
#'
#' @description Returns a dataframe with all the exports available.
#'
#' @param user Optional. A single string indicating the username
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#' @param pwd Password of the Kobo account to use.
#' @return Returns a dataframe with all the exports available.
#'
#' @author Elliott Messeiller
#'
#' @export all_exports
#'


all_exports <- function(user,pwd="", api="https://kobo.humanitarianresponse.info") {
  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  all_exports <- GET(paste0(api, "/exports/"), authenticate(user, pwd), progress())
  all_expors_text <- content(all_exports, "text", encoding = "UTF-8")
  all_exports_text_json <- fromJSON(all_expors_text)

  all_exports_df <-  as.data.frame(all_exports_text_json$results$data)%>%
    bind_cols(data.frame("url_export" = all_exports_text_json$results$url))%>%
    bind_cols(data.frame("date_created"= ymd_hms(all_exports_text_json$results$date_created)))%>%
    mutate(uid_form = str_replace(source, paste0(api, "\\/assets\\/"),""),
           uid_form = str_replace(uid_form, "\\/$",""))

  return(all_exports_df)
}

#' @name download_data
#' @rdname download_data
#' @title  Download data from the platform
#'
#' @description Download data from the platform.
#'
#' @param formid The ID of the form to be accessed (as a character string)
#' @param user Optional. A single string indicating the username
#' @param pwd Password of the Kobo account to use
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#' @param seperator Separator used between select_multiple questions and their choices. Must be a regex expression. Default to forward slash
#' @return A dataframe containing the data, with no group names.
#'
#' @author Elliott Messeiller
#'
#' @export download_data
#'


download_data <- function(formid, user,pwd, api="https://kobo.humanitarianresponse.info", seperator="\\/") {
  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")
  download_forms_all <- download_forms_all(user, pwd, api)

  old_id <- as.character(download_forms_all%>%filter(uid == formid)%>%select(old_id))
  old_id <- as.character(download_forms_all[download_forms_all$uid == formid, 'old_id'])

  type_api <- api_type(api)
  if(type_api == 'humanitarianresponse.info'){
    api_old <- gsub("kobo", "kc", api)
  }else  if(type_api == 'kf.kobotoolbox.org'){
    api_old <- gsub("kf", "kc", api)
  }else{
    stop("API not supported. Please use kf.kobotoolbox.org, or kobo.humanitarianresponse.info API.")
  }

  form <- download_form(formid = formid, user = user, pwd = pwd)

  url_data <- paste0(api_old, "/api/v1/data/",old_id, ".csv")
  raw_data <- GET(url_data, authenticate(user, pwd), progress())
  raw_data <- content(raw_data, "raw", encoding = "UTF-8")
  raw_data <- read_csv(raw_data, na = c("", "NA", "n/a"))

  raw_data <- remove_GroupeHeaders(raw_data, formid, pwd, user, api)

  raw_data <- addStartCols_sm(raw_data, form )

  return(raw_data)

}

#' @name remove_GroupeHeaders
#' @rdname remove_GroupeHeaders
#' @title  Remove groupes from dataframe header
#'
#' @description Remove groupes from dataframe header
#'
#' @param data The dataframe to be treated.
#' @param formid The ID of the form to be accessed (as a character string)
#' @param user Optional. A single string indicating the username
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#' @param seperator Separator used between select_multiple questions and their choices. Must be a regex expression. Default to forward slash
#' @return A dataframe without groups in headers.
#'
#' @author Elliott Messeiller
#'
#' @export remove_GroupeHeaders

remove_GroupeHeaders <- function(data,formid, pwd, user, api="https://kobo.humanitarianresponse.info", seperator = "\\/") {

  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  form <-download_form(formid, user, pwd)
  survey_sheet <- form$survey

  data <- noGroupsHeader(data, survey_sheet, seperator)

  return(data)
}

#' @name api_type
#' @rdname api_type
#' @title  Check the API type
#'
#' @description Check the API type
#' @param api The URL at which the API can be accessed.
#'
#' @return API type
#'
#' @author Elliott Messeiller
#'
#' @export api_type
#'

api_type <- function(api="https://kobo.humanitarianresponse.info"){
  if(grepl("kf.kobotoolbox.org", api)){
    api_type <- "kobotoolbox.org"
  }else if(grepl("kobo.humanitarianresponse.info", api)){
    api_type <- "humanitarianresponse.info"
  }else{ stop("API not supported. Please use kf.kobotoolbox.org, or kobo.humanitarianresponse.info API. ")}

  return(api_type)

}

#' @name create_export
#' @rdname create_export
#' @title  Create exports through API
#' @description Create exports (endpoint) through API for specified form. Exports allow to download the data via the new KPI.
#'
#' @param asset_uid UID (ID) of the asset (form) for which an export is created.
#' @param user Username of the Kobo account to use.
#' @param pw Password of the Kobo account to use.
#' @param type Type of exports to create: can be "csv" or "xls". Defaults to "csv"
#' @param lang Language to be used for the export. Defaults to "xml"
#' @param fields_from_all_versions Include or not all versions of the form. Logical string: "true" or "false". Defaults to "true"
#' @param hierarchy_in_labels Group names are displayed or not in labels of the dataframe. Logical string: "true" or "false". Defaults to "false"
#' @param group_sep Type of exports to create: can be "csv" or "xls".
#' @param api The URL at which the API can be accessed.
#'
#' @author Punya Prasad Sapkota (https://github.com/ppsapkota/), Elliott Messeiller
#'
#' @export create_export

create_export<-function(asset_uid, user, pwd ="", api="https://kobo.humanitarianresponse.info", type ="csv",lang="xml",fields_from_all_versions = "true",hierarchy_in_labels ="false",group_sep = "/"){
  if(!exists('pwd') || (pwd == "" || is.null(pwd))){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")


  api_url_export<-paste0(api,"/exports/")
  api_url_asset<-paste0(api,"/assets/",asset_uid,"/")
  api_url_export_asset<-paste0(api,"/exports/",asset_uid,"/")
  #
  d<-list(source=api_url_asset,
          type=type,
          lang=lang,
          fields_from_all_versions=fields_from_all_versions,
          hierarchy_in_labels=hierarchy_in_labels,
          group_sep=group_sep)
  #fetch data
  result<-httr::POST (url=api_url_export,
                      body=d,
                      authenticate(user,pw),
                      progress()
  )
  return(result$status_code)
}
