
#' Module containing the implementation for Day25 of Advent Of Code 2022
#' @noRd
day25 <- modules::module(
  {
    modules::export("parse_input")
    parse_input <- function(input_string) {
      stop("To Be Implemented")
    }
    
    
    modules::export("solve_part1")
    solve_part1 <- function(input) {
      stop("To Be Implemented")
    }
  }
)


#' Parsed Input to Day25 of Advent Of Code 2022
#'
#' @param input_string Input string to day 25 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day25_input <- function(input_string) {
  day25$parse_input(input_string)
}

#' Solution to Day25 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day25_part1 <- function(input) {
  day25$solve_part1(input)
}
