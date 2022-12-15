
#' Module containing the implementation for Day16 of Advent Of Code 2022
#' @noRd
day16 <- modules::module(
  {
    modules::export("parse_input")
    parse_input <- function(input_string) {
      stop("To Be Implemented")
    }
    
    
    modules::export("solve_part1")
    solve_part1 <- function(input) {
      stop("To Be Implemented")
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(input) {
      stop("To Be Implemented")
    }
  }
)


#' Parsed Input to Day16 of Advent Of Code 2022
#'
#' @param input_string Input string to day 16 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day16_input <- function(input_string) {
  day16$parse_input(input_string)
}

#' Solution to Day16 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day16_part1 <- function(input) {
  day16$solve_part1(input)
}

#' Solution to Day16 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day16_part2 <- function(input) {
  day16$solve_part2(input)
}
