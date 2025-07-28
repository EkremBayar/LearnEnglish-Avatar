# functions
find_structure <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word))
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0(paste0("\\b", word_pattern, "\\b"),collapse = "|")))
}

find_participles <- function(text_column, type = c("ing", "ed")){

  if(type == "ing" | type == "ed"){
    pattern <- paste0("\\b.*",type,"\\b")
  }else{
    stop("type argument must be ing or ed!")
  }
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = pattern))
}

find_detect <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word))
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0(paste0(word_pattern),collapse = "|")))
}


datatable_header_ui <- function(datatable_id){
  res <- tagList(
    tags$style(type = "text/css",".noUi-connect {background: #39a642;}"), #lacivert 002749
    tags$style(HTML(paste0("#", datatable_id, ' table.dataTable tbody tr.selected>* {box-shadow: inset 0 0 0 9999px #C6E0B4; color: black;}'))),
    tags$head(tags$style(paste0("#", datatable_id," thead th{background-color: #002749; color: white;}"))),
    #tags$head(tags$style("#responses_table tbody td {border-top: 0.1px solid #002749;border-left: 0.1px solid #002749;border-right: 0.1px solid #002749;}")),
    tags$head(tags$style("#", datatable_id, " .dataTables_length {float:right;}"))
  )
  return(res)
}
