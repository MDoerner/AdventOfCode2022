

#' Split Blocks of Lines
#'
#' @param text A character vector of length 1  
#'
#' @return A character vector containing the blocks in `text` separated by an empty line
split_line_separated_blocks <- function(text){
  block_list <- stringr::str_split(text, "(\r?\n){2,}")
  unlist(block_list)
}
modules::export("split_line_separated_blocks")


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
modules::export("split_lines")

#' Split Characters
#'
#' @param text A character vector of length 1
#'
#' @return A character vector containing each individual character
split_characters <- function(text){
  stringr::str_split(text, pattern = "") %>%
    unlist()
} 
modules::export("split_characters")


#' To Character Grid
#'
#' @param text A character vector of length 1 whose lines have the same length
#'
#' @return A tibble with one row per character, excluding newlines, with an `Value` column containing the character, an `X` column containing the character position in the row and a `Y` column containing the line number of the character
#' @importFrom magrittr %>%
#' @importFrom rlang .data
to_character_grid <- function(text){
  line_characters <- text %>% 
    split_lines() %>%
    purrr::map(split_characters)
  y_coordinates <- seq_along(line_characters)
  names(line_characters) <- paste0("Y", y_coordinates)
  tibble::as_tibble(line_characters) %>%
    dplyr::mutate(
      X = seq_along(.data$Y1)
    ) %>% tidyr::pivot_longer(
      cols = dplyr::starts_with("Y"),
      names_prefix = "Y",
      names_to = "Y",
      values_to = "Value"
    ) %>% 
    dplyr::mutate(
      Y = readr::parse_integer(.data$Y)
    )
}
modules::export("to_character_grid")