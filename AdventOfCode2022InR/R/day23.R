
#' Module containing the implementation for Day23 of Advent Of Code 2022
#' @noRd
day23 <- modules::module(
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


#' Parsed Input to Day23 of Advent Of Code 2022
#'
#' @param input_string Input string to day 23 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day23_input <- function(input_string) {
  day23$parse_input(input_string)
}

#' Solution to Day23 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day23_part1 <- function(input) {
  day23$solve_part1(input)
}

#' Solution to Day23 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day23_part2 <- function(input) {
  day23$solve_part2(input)
}
