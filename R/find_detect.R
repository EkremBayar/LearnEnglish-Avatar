#' Find Detect
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
#' }
#'
find_detect <- function(text_column, word){
  if(!is.character(word)){
    stop("word argument must be a character or character vector!")
  }
  word_pattern <- paste0(tolower(word))
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = paste0(paste0(word_pattern),collapse = "|")))
}
df
