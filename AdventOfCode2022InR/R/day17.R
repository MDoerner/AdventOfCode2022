
#' Module containing the implementation for Day17 of Advent Of Code 2022
#' @noRd
day17 <- modules::module(
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


#' Parsed Input to Day17 of Advent Of Code 2022
#'
#' @param input_string Input string to day 17 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day17_input <- function(input_string) {
  day17$parse_input(input_string)
}

#' Solution to Day17 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day17_part1 <- function(input) {
  day17$solve_part1(input)
}

#' Solution to Day17 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day17_part2 <- function(input) {
  day17$solve_part2(input)
}
