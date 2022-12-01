
#' Validate AdventOfCode Problem Specification
#'
#' Raises an error if the provided values do not form a valid selection of an AdventOfCode problem.
#'
#' @param day An integer specifying the day of AdventOfCode
#' @param part An integer specifying the day of AdventOfCode
#'
#' @return None
validate_problem_selection <- function(day, part) {
  if (!is.numeric(day)) {
    cli::cli_abort(c(
      "The {.var day} of AdventOfCode must be an integer between 1 and 25.",
      "x" = "You have supplied a {.cls {class(day)}} vector."
    ))
  } else if (length(day) != 1) {
    cli::cli_abort(c(
      "The {.var day} of AdventOfCode must be an integer between 1 and 25.",
      "x" = "You have supplied a vector of length {length(day)}."
    ))
  } else if (day < 1) {
    cli::cli_abort(c(
      "The {.var day} of AdventOfCode must be an integer between 1 and 25.",
      "x" = "The value you have supplied is {day}."
    ))
  } else if (day > 25) {
    cli::cli_abort(c(
      "There are only 25 days of AdventOfCode.",
      "x" = "The {.var day} you have supplied is {day}."
    ))
  } else if (!is.numeric(part)) {
    cli::cli_abort(c(
      "The {.var part} of an AdventOfCode day must be the integer 1 or 2.",
      "x" = "You have supplied a {.cls {class(part)}} vector."
    ))
  } else if (length(part) != 1) {
    cli::cli_abort(c(
      "The {.var part} of an AdventOfCode day must be the integer 1 or 2.",
      "x" = "You have supplied a vector of length {length(part)}."
    ))
  } else if (part != 1 && part != 2) {
    cli::cli_abort(c(
      "There are only part 1 and 2 for a day of AdventOfCode.",
      "x" = "The {.var part} you have supplied is {part}."
    ))
  } else if (day == 25 && part == 2) {
    cli::cli_abort(c(
      "There is no part 2 of day 25 of AdventOfCode."
    ))
  }
}


#' Validate AdventOfCode Input String
#'
#' Basic validation for the input string to AdventOfCode problems.
#' Raises an error if the input is ot a character vector of length 1.
#'
#' @param input_string A string containing the input to an AdventOfCode problem.
#'
#' @return None
validate_input_string <- function(input_string) {
  if (!is.character(input_string)) {
    cli::cli_abort(c(
      "The {.var input_string} for AdventOfCode must be a character vector of length 1.",
      "x" = "You have supplied a {.cls {class(input_string)}} vector."
    ))
  } else if (length(input_string) != 1) {
    cli::cli_abort(c(
      "The {.var input_string} for AdventOfCode must be a character vector of length 1.",
      "x" = "You have supplied a vector of length {length(input_string)}."
    ))
  }
}


#' Name of the parse function for a specific day of AdventOfCode
#'
#' @inheritParams solve_day
#'
#' @return Name of the input parse function for the specified `day`
input_parse_function_name <- function(day) {
  glue::glue("parse_day{day}_input")
}


#' @inherit parse_day_input
parse_day_input_wo_validation <- function(input_string, day) {
  do.call(input_parse_function_name(day), list(input_string))
}



#' Parse Advent of Code Input
#'
#' Parses the `input_string` into a data structure suitable to solve the specified problem in AdventOfCode.
#'
#' @inheritParams solve_day
#'
#' @return Some data structure suitable as input to the specified problem in AdventOfCode
#' @export
#'
#' @examples
#' parse_day_input("1000\n 2000\n 3000\n\n 4000", day = 1, part = 1)
parse_day_input <- function(input_string, day, part = 1) {
  validate_problem_selection(day, part)
  parse_day_input_wo_validation(input_string, day)
}


#' Name of the function solving the specified problem in AdventOfCode
#'
#' The named function is the function solving based on parsed input.
#'
#' @inheritParams solve_day
#'
#' @return Name of the function solving the specified problem in AdventOfCode
solve_day_function_name <- function(day, part) {
  glue::glue("solve_day{day}_part{part}")
}


#' @inherit solve_day
solve_day_wo_validation <- function(input_string, day, part) {
  structured_input <- parse_day_input_wo_validation(input_string, day)
  do.call(solve_day_function_name(day, part), list(structured_input))
}



#' Solve a Challenge from Advent of Code 2022
#'
#' @param input_string A character vector of length 1 containing the input for the specified problem in AdventOfCode
#' @param day An integer between 1 and 25 specifying the day of AdventOfCode
#' @param part The integer 1 or 2 specifying the part of the `day` of AdventOfCode
#'
#' @return A string containing the solution
#' @export
#'
#' @examples
#' solve_day("1000\n 2000\n 3000\n\n 4000", day = 1, part = 1)
solve_day <- function(input_string, day, part) {
  validate_problem_selection(day, part)
  validate_input_string(input_string)
  solve_day_wo_validation(input_string, day, part)
}
