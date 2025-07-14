run_transcript_app <- function() {
  app_dir <- fs::path_package("LearnEnglishWithAvatar", "shiny")

  if (!dir.exists(app_dir)) {
    stop("Shiny app directory not found. Please ensure the package is installed correctly.")
  }

  # Shiny uygulamasını çalıştır
  shiny::runApp(app_dir, display.mode = "normal")
}

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

export_transcripts <- function(){
  df <- transcripts_atla %>% dplyr::select(book_num, book, chapter_num, chapter, character, full_text, character_words)

  # Each Book
  for(i in unique(df$book_num)){

    wb <- openxlsx::createWorkbook()

    book <- df %>% dplyr::filter(book_num == i, character != "Scene Description")

    for(j in unique(book$chapter_num)){
      openxlsx::addWorksheet(wb, sheetName = j)
      x <- (book %>% dplyr::filter(chapter_num == j) %>% dplyr::select(chapter, character, character_words))
      openxlsx::writeData(wb, sheet = j, x)

      # Style
      openxlsx::addStyle(wb, sheet = j, rows = 1:(nrow(x)+1), cols = 1, style = openxlsx::createStyle(valign = "top"))
      openxlsx::addStyle(wb, sheet = j, rows = 1:(nrow(x)+1), cols = 2, style = openxlsx::createStyle(valign = "top"))
      openxlsx::addStyle(wb, sheet = j, rows = 1:(nrow(x)+1), cols = 3, style = openxlsx::createStyle(valign = "top", wrapText = T))

      openxlsx::setColWidths(wb, sheet = j, cols = 3, widths = 66)
    }

    openxlsx::saveWorkbook(wb, paste0(unique(book$book),".xlsx"), overwrite = T)

  }
}
