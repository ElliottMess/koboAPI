#' @name noGroupsHeader
#' @rdname noGroupsHeader
#' @title  Remove groupes from dataframe header
#'
#' @description Remove groupes from dataframe header
#'
#' @param data The dataframe to be treated.
#' @param questions A dataframe with questions from a XLSform. Typically the 'survey' sheet.
#' @param separator Separator used between select_multiple questions and their choices. Must be a regex expression. Default to forward slash
#' @return A dataframe without groups in headers.
#'
#' @author Elliott Messeiller
#'
#' @export noGroupsHeader
#'


noGroupsHeader <- function(data,questions,  separator = "\\/") {
  questions <- as.data.frame(questions)
  groups <- paste0(as.list(questions%>%filter(type %in% c("begin_group", "begin group"))%>% select(name))[[1]],separator)
  groups <- c(groups, paste0("meta", separator))
  collapse_groups <- str_c(groups, collapse = "|")
  groups_removed <- unlist(map(names(data), str_remove, collapse_groups))
  names(data) <- groups_removed
  return(data)
}

#' @name default_label
#' @rdname default_label
#' @title  Find default labels in kobo data frames if there is multiple languages.
#'
#' @description Find default labels in kobo data frames if there is multiple languages.
#'
#' @param questions A dataframe with questions from a XLSform. Typically the 'survey' sheet.
#' @param language The language to use as default if known. Default to english
#' @return A dataframe without groups in headers.
#'
#' @author Elliott Messeiller
#'
#' @export default_label
#'


default_label <- function(questions, language = 'english') {

  questions <- as.data.frame(questions)

  label_cols <- names(select(questions, contains('label')))

  if(length(label_cols) == 1){
    return(label_cols)
  }else{
    if('label' %in% label_cols){
      default_label <- label_cols[grep('^label$', label_cols)]
    }else{
      default_label <- label_cols[grep(language, tolower(label_cols))]
      if(length(default_label)==0){
        stop('Default Label not found. Check if default language is right.')
      }else{
        return(default_label)
      }
    }
  }
}

#' @name extract_Loops
#' @rdname extract_Loops
#' @title  Extract repeat loops from the main dataset.
#'
#' @description Extract repeat loops from the main dataset.
#'
#' @param data The dataframe to be treated.
#' @param questions A dataframe with questions from a XLSform. Typically the 'survey' sheet.

#' @return A dataframe with nested dataframes for all loops.
#'
#' @author Elliott Messeiller
#'
#' @export extract_Loops

extract_Loops <- function(data, questions,...) {

  if(!exists(language)){
    language <- 'english'
  }

  label_default <- default_label(questions = questions, language = language)

  data <- as.data.frame(data)

  data <- normaliseColNames(data)

  questions <- as.data.frame(questions)

  loops_ques <- (questions[questions$type %in% c('begin_repeat', 'begin repeat'),c('name', label_default )])
  findLoops <- c(names(data[,grepl('\\[[0-9]*?\\]', names(data))]), 'uuid')
  findLoops_cols <- data[findLoops]

  new_names <- map(names(findLoops_cols), str_replace, '\\[', '_')
  new_names <- unlist(map(new_names, str_replace, '\\]', ''))
  names(findLoops_cols) <- new_names

  splitted_frames <- map(set_names(loops_ques$name),~select(findLoops_cols,contains(.x), uuid))%>%
    map(format_splitted_frames)

  return(splitted_frames)
}

format_splitted_frames <- function(list_df){
  list_df%>%
    gather(value = 'value', key = 'Letter', -uuid)%>%
    separate(Letter, into = c('grp', 'question'), sep = '\\/')%>%
    separate(grp, into = c('grp', 'parent_index'), sep = '_')%>%
    mutate(name = paste(grp, iquestion, sep = '/'))%>%
    select(name, value, uuid, parent_index)%>%
    pivot_wider(names_from = name, values_from = value)
}


#' @name normaliseColNames
#' @rdname normaliseColNames
#' @title  Normalise column names of dataframe to make them R friendly
#'
#' @description Normalise column names of dataframe to make them R friendly by removing underscores at beggining of variables
#'
#' @param data The dataframe to be treated.
#' @param questions A dataframe with questions from a XLSform. Typically the 'survey' sheet.

#' @return A dataframe without groups in headers.
#'
#' @author Elliott Messeiller
#'
#' @export normaliseColNames

normaliseColNames <- function(data) {

  data <- as.data.frame(data)

  names(data) <- str_remove(names(data), '^_')

  return(data)

}


#' @name addStartCols_sm
#' @rdname addStartCols_sm
#' @title  Adds a list column with the choices selected at the beggining of select_multiple questions
#' @description Adds a list column with the choices selected at the beggining of select_multiple questions
#' @param data The dataframe to be treated.
#' @param form A list with two objects: The "survey" sheet as a dataframe with all the questions variables, and the "choices" sheet as a dataframe with all the choices variables. See form()
#' @param separator Separator used between select_multiple questions and their choices. Must be a regex expression. Default to forward slash
#' @return Returns data with the additional columns
#' @author Elliott Messeiller
#'
#' @export addStartCols_sm
addStartCols_sm<- function(data, form, separator = "\\/"){
  survey <- form$survey
  all_selectMultiple <- (survey[survey$type == "select_multiple", "name"])

  if(length(all_selectMultiple)==0){warning(paste0("No select_multiple question found with. Please double check that you have select_multiple qustions in your form."))}

  indices <- all_selectMultiple%>%
    rowwise()%>%
    mutate(index = min(grep(name, names(data))),
           col_name_before = names(data)[index])


  for(i in 1:nrow(indices)){
      data <- add_oneColumn(data, indices[i,])
  }

  return(data)
}

add_oneColumn <- function(data, indices_vect, separator = "\\/"){
  content_col <- fill_startCol_sm(data, indices_vect[[1]], separator)
  add_column(data,!!(indices_vect[[1]]):= content_col, .before = indices_vect[[3]])
}


#### FILLING STARTCOLUMN
fill_startCol_sm <- function(data, startCol, separator = "\\/"){
  stopifnot(is.character(startCol))

  columns <- data[,grep(paste0(startCol, separator), names(data))]%>%
    select_if(isTRUE)

  col_names <- str_remove( names(columns), paste0(startCol, separator))

  fill_content <- paste(col_names)

  if(length(fill_content) == 0){
    fill_content <- rep(NA, nrow(data))
  }

  return(fill_content)
}
