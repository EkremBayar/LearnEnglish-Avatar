#' Find Structure
#'
#' @param text_column chracter
#' @param word chracter
#'
#' @return chracter
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#'
#' @examples \dontrun{
#'
#' # Of
#' target_of <- df %>% filter(find_structure(character_words, word = "of"))
#'
#' # IF Conditions & Unless
#' target_if <- df %>% filter(find_structure(character_words, word = c("if", "unless")))
#'
#' # Which
#' target_which <- df %>% filter(find_structure(character_words, word = "which"))
#' }
#'
find_structure <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word), collapse = "|")
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0("\\b", word_pattern, "\\b")))
}
