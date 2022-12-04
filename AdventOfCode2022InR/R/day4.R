


parsed_pair <- function(pair_line) {
  matches <- stringr::str_match(pair_line, pattern = "^(\\d+)-(\\d+),(\\d+)-(\\d+)$")
  sections <- purrr::map(matches[2:5], readr::parse_integer)
  list(
    c(sections[[1]], sections[[2]]),
    c(sections[[3]], sections[[4]])
  )
}



#' Parsed Input to Day4 of Advent Of Code 2022
#'
#' @param input_string Input string to day 4 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day4_input <- function(input_string) {
  input_string %>%
    split_lines() %>%
    purrr::map(parsed_pair)
}





range_contains <- function(range1, range2) {
  range1[[1]] <= range2[[1]] && range1[[2]] >= range2[[2]]
}

are_completely_overlapping <- function(range1, range2) {
  range_contains(range1, range2) || range_contains(range2, range1)
}

is_completely_overlapping_pair <- function(range_pair) {
  are_completely_overlapping(range_pair[[1]], range_pair[[2]])
}


#' Solution to Day4 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day4_part1 <- function(input) {
  input %>%
    purrr::map(is_completely_overlapping_pair) %>%
    unlist() %>%
    sum() %>% # TRUE coerces to 1 and FALSE to 0
    as.character()
}


is_in_range <- function(number, range){
  range[[1]] <= number && range[[2]] >= number
}

are_overlapping <- function(range1, range2) {
  is_in_range(range2[[1]], range1) || 
    is_in_range(range2[[2]], range1) || 
    is_in_range(range1[[1]], range2) || 
    is_in_range(range1[[2]], range2)
}

is_overlapping_pair <- function(range_pair) {
  are_overlapping(range_pair[[1]], range_pair[[2]])
}


#' Solution to Day4 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day4_part2 <- function(input) {
  input %>%
    purrr::map(is_overlapping_pair) %>%
    unlist() %>%
    sum() %>% # TRUE coerces to 1 and FALSE to 0
    as.character()
}
