#' Find Participles
#'
#' @param text_column character
#' @param type character
#'
#' @return character
#' @export
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_to_lower
#'
#' @examples \dontrun{
#'
#' # -ing
#' target_ing <- df %>% filter(find_participles(character_words, type = "ing))
#'
#' # -ed
#' target_ed <- df %>% filter(find_participles(character_words, type = "ed"))
#' }
#'
find_participles <- function(text_column, type = c("ing", "ed")){

  if(type == "ing" | type == "ed"){
    pattern <- paste0("\\b.*",type,"\\b")
  }else{
    stop("type argument must be ing or ed!")
  }
  return(stringr::str_detect(stringr::str_to_lower(text_column), pattern = pattern))
}
