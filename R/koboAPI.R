#' @name kobo_form
#' @rdname kobo_form
#' @title  Download form from the platform
#'
#' @description Download form from the platform
#'
#' @param formid The ID of the form to be accessed (as a character string). Must be a KPI-type ID (not api V1 ID). See kobo_all_forms for more details.
#' @param user Optional. A single string indicating the username
#' @param api The URL at which the API can be accessed. Default to "kobo.humanitarianresponse.info"
#'
#' @return A list with two objects: The "survey" sheet as a dataframe with all the questions variables, and the "choices" sheet as a dataframe with all the choices variables.
#'
#' @author Elliott Messeiller
#'
#' @export kobo_form
#'

kobo_form <- function(formid, user, api="https://kobo.humanitarianresponse.info") {
  if(pwd == ""){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  url_form <- paste0(api, "/assets/", formid, "/")
  raw_form <- GET(url_form, authenticate(user, pwd))
  raw_form_text <- content(raw_form, "text", encoding = "UTF-8")
  raw_form_text_json <- fromJSON(raw_form_text)

  languages <- as.vector(raw_form_text_json$content$translations)
  languages_labels <- paste0("label::", languages)
  choices <- as.data.frame(raw_form_text_json$content$choices)
  survey <- as.data.frame(raw_form_text_json$content$survey)
  survey$label <- nullToNA(survey$label)

  if(length(languages)>1){
    choices_labels <- as.data.frame(do.call(rbind, choices$label))
    names(choices_labels) <- languages_labels
    choices <- cbind(choices, choices_labels) %>%
      select(-label)

    survey_labels <- as.data.frame(do.call(rbind, survey$label))
    names(survey_labels) <- languages_labels
    survey <- cbind(survey, survey_labels) %>%
      select(-label)
  }

  form <- list("survey" = survey, "choices" = choices)

  return(form)
}

#' @name kobo_all_forms
#' @rdname kobo_all_forms
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
#' @export kobo_all_forms
#'
#'
kobo_all_forms <- function(user,pwd, api = "https://kobo.humanitarianresponse.info"){
  if(pwd == ""){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")
  api_type <- kobo_api_type(api)

  if(pwd == "") stop("No password entered.")

  all_forms <- GET(paste0(api, "/assets/"), authenticate(user, pwd))
  all_forms_text <- content(all_forms, "text", encoding = "UTF-8")
  all_forms_text_json <- fromJSON(all_forms_text)

  all_forms_df <-  as.data.frame(all_forms_text_json$results)%>%
    filter(has_deployment == TRUE)%>%
    select(name, uid, date_created, date_modified, deployment__submission_count)

  if(api_type == "kobotoolbox.org"){
    old_api <- "https://kc.kobotoolbox.org/api/v1/data"
  }
  if(api_type == "humanitarianresponse.info"){
    old_api <- "https://kc.humanitarianresponse.info/api/v1/data"
  }
  all_forms_old <- GET(old_api, authenticate(user, pwd))
  all_forms_old <- content(all_forms_old, "text", encoding = "UTF-8")
  all_forms_old <- fromJSON(all_forms_old)
  all_forms_old <- as.data.frame(all_forms_old)%>%
    mutate(old_id = id)%>%
    select(old_id, id_string)

  all_forms_df <- left_join(all_forms_df, all_forms_old, by = c("uid" = "id_string"))


  return(all_forms_df)
}

#' @name kobo_all_exports
#' @rdname kobo_all_exports
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
#' @export kobo_all_exports
#'


kobo_all_exports <- function(user,pwd="", api="https://kobo.humanitarianresponse.info") {
  if(pwd == ""){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  all_exports <- GET(paste0(api, "/exports/"), authenticate(user, pwd))
  all_expors_text <- content(all_exports, "text", encoding = "UTF-8")
  all_exports_text_json <- fromJSON(all_expors_text)

  all_exports_df <-  as.data.frame(all_exports_text_json$results$data)%>%
    bind_cols(data.frame("url_export" = all_exports_text_json$results$url))%>%
    bind_cols(data.frame("date_created"= ymd_hms(all_exports_text_json$results$date_created)))%>%
    mutate(uid_form = str_replace(source, paste0(api, "\\/assets\\/"),""),
           uid_form = str_replace(uid_form, "\\/$",""))

  return(all_exports_df)
}

#' @name kobo_data
#' @rdname kobo_data
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
#' @export kobo_data
#'


kobo_data <- function(formid, user,pwd, api="https://kobo.humanitarianresponse.info", seperator="\\/") {
  if(pwd == ""){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")
  all_forms <- kobo_all_forms(user, pwd, api)

  old_id <- as.character(all_forms%>%filter(uid == formid)%>%select(old_id))

  api_old <- gsub("kobo", "kc", api)
  url_data <- paste0(api_old, "/api/v1/data/",old_id, ".csv")
  raw_data <- GET(url_data, authenticate(user, pwd))
  raw_data <- content(raw_data, "raw", encoding = "UTF-8")
  raw_data <- read_csv(raw_data, na = c("", "NA", "n/a"))

  raw_data <- kobo_noGroupsHeader(raw_data, formid, pwd, user, api)

  raw_data <- kobo_AddstartSelectMultiple(raw_data)
  names(raw_data) <- gsub("\\.","\\/", names(raw_data))


  return(raw_data)

}

#' @name kobo_noGroupsHeader
#' @rdname kobo_noGroupsHeader
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
#' @export kobo_noGroupsHeader
#'


kobo_noGroupsHeader <- function(data,formid, pwd, user, api="https://kobo.humanitarianresponse.info", seperator = "\\/") {

  if(pwd == ""){pwd <- readline("Enter password:")}
  if(pwd == "") stop("No password entered.")

  form <-kobo_form(formid, user , api)
  survey_sheet <- form$survey
  groups <- paste0(as.list(survey_sheet%>%filter(type %in% c("begin_group", "begin group"))%>% select(name))[[1]],seperator)
  groups <- c(groups, paste0("meta", seperator))
  collapse_groups <- str_c(groups, collapse = "|")
  groups_removed <- map(names(data), str_remove, collapse_groups)
  names(data) <- groups_removed
  return(data)

}

#' @name kobo_api_type
#' @rdname kobo_api_type
#' @title  Check the API type
#'
#' @description Check the API type
#' @param api The URL at which the API can be accessed.
#'
#' @return API type
#'
#' @author Elliott Messeiller
#'
#' @export kobo_api_type
#'

kobo_api_type <- function(api="https://kobo.humanitarianresponse.info"){
  if(grepl("kf.kobotoolbox.org", api)){
    api_type <- "kobotoolbox.org"
  }else if(grepl("kobo.humanitarianresponse.info", api)){
    api_type <- "humanitarianresponse.info"
  }else{ stop("API not supported. Please use kf.kobotoolbox.org, or kobo.humanitarianresponse.info API. ")}

  return(api_type)

}

#' @name kobo_create_export
#' @rdname kobo_create_export
#' @title  Create exports through API
#' @description Create exports (endpoint) through API for specified form. Exports allow to download the data via the new KPI.
#'
#' @param asset_uid UID (ID) of the asset (form) for which an export is created.
#' @param kobo_user Username of the Kobo account to use.
#' @param Kobo_pw Password of the Kobo account to use.
#' @param type Type of exports to create: can be "csv" or "xls". Defaults to "csv"
#' @param lang Language to be used for the export. Defaults to "xml"
#' @param fields_from_all_versions Include or not all versions of the form. Logical string: "true" or "false". Defaults to "true"
#' @param hierarchy_in_labels Group names are displayed or not in labels of the dataframe. Logical string: "true" or "false". Defaults to "false"
#' @param group_sep Type of exports to create: can be "csv" or "xls".
#' @param api The URL at which the API can be accessed.
#'
#' @author Punya Prasad Sapkota (https://github.com/ppsapkota/), Elliott Messeiller
#'
#' @export kobo_create_export

kobo_create_export<-function(asset_uid, kobo_user, Kobo_pw ="", api="https://kobo.humanitarianresponse.info", type ="csv",lang="xml",fields_from_all_versions = "true",hierarchy_in_labels ="false",group_sep = "/"){
  if(Kobo_pw == ""){Kobo_pw <- readline("Enter password:")}
  if(Kobo_pw == "") stop("No password entered.")


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
                      authenticate(kobo_user,Kobo_pw),
                      progress()
  )
  return(result$status_code)
}

#' @name kobo_AddstartCol_SelectMultiple
#' @rdname kobo_AddstartCol_SelectMultiple
#' @title  Adds a list column with the choices selected at the beggining of select_multiple questions
#' @description Adds a list column with the choices selected at the beggining of select_multiple questions
#' @param data The dataframe to be treated.
#' @param form A list with two objects: The "survey" sheet as a dataframe with all the questions variables, and the "choices" sheet as a dataframe with all the choices variables. See kobo_form()
#' @param seperator Separator used between select_multiple questions and their choices. Must be a regex expression. Default to forward slash
#' @return Returns data with the additional columns
#' @author Elliott Messeiller
#'
#' @export kobo_AddstartCol_SelectMultiple



kobo_AddstartCol_SelectMultiple<- function(data, form, seperator = "\\/"){
  survey <- form$survey
  all_selectMultiple <- survey[survey$type == "select_multiple","name"]

  if(length(all_selectMultiple)==0){warning(paste0("No select_multiple question found with. Please double check that you have select_multiple qustions in your form."))}

  expr_firstCol <- paste0(all_selectMultiple, seperator, ".*?$")



  potential_selectm <- tibble(name = names(data[,grepl(paste0("\\."),names(data))]))

  if(nrow(potential_selectm)==0){warning(paste0("No choices found with. Please double check that you have select_multiple qustions in your form."))}

  choices_split <- str_split(potential_selectm$name, "\\.", simplify = TRUE)
  if(ncol(choices_split)>3){stop("You probably have a forward slash in a choice (e.g. 'N/A'). Please remove it from the raw data.")}

  questions <- data.frame(name=unique(choices_split[,1]), stringsAsFactors = FALSE)
  q_frame = data.frame(q=rep(0, nrow(data)))
  for(i in 1:nrow(questions)){
    questions[i, "first_occ"] <- grep(paste0("^", questions[i, "name"], "\\..*?"),names(data))[1]
    names(q_frame) <- questions[i, "name"]
    data <- as.data.frame(append(data, q_frame, after = questions[i, "first_occ"]-1), make.names=FALSE)
    questions$first_occ <- questions$first_occ+1
  }

  return(data)



  return(data)

}


nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
