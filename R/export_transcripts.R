#' Export Transcripts
#'
#' @param text_column chracter
#' @param word chracter
#'
#' @return chracter
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @import openxlsx
#'
#' @examples \dontrun{
#'

#' }
#'
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




