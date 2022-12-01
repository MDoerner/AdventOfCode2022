


#' Split Blocks of Lines
#'
#' @param text A character vector of length 1  
#'
#' @return A character vector containing the blocks in `text` separated by an empty line
split_line_separated_blocks <- function(text){
  block_list <- stringr::str_split(text, "(\r?\n){2,}")
  unlist(block_list)
}


#' Split Lines
#'
#' @param text A character vector of length 1
#' @param remove_empty A logical vector of length 1 specifying whether empty lines should be removed
#'
#' @return A character vector containing the line contents of each line of `text`.
split_lines <- function(text, remove_empty = TRUE){
  lines_list <- stringr::str_split(text, "\r?\n")
  lines <- unlist(lines_list)
  if (remove_empty) {
    lines[lines != ""]
  } else {
    lines
  }
}