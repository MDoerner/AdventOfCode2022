
#' Module containing the implementation for Day19 of Advent Of Code 2022
#' @noRd
day19 <- modules::module(
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


#' Parsed Input to Day19 of Advent Of Code 2022
#'
#' @param input_string Input string to day 19 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day19_input <- function(input_string) {
  day19$parse_input(input_string)
}

#' Solution to Day19 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day19_part1 <- function(input) {
  day19$solve_part1(input)
}

#' Solution to Day19 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day19_part2 <- function(input) {
  day19$solve_part2(input)
}
