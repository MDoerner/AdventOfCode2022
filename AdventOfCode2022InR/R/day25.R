
#' Module containing the implementation for Day25 of Advent Of Code 2022
#' @noRd
day25 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      parsing$split_lines(input_string)
    }
    
    
    modules::export("snafu_to_decimal")
    snafu_to_decimal <- function(snafu) {
      digits <- snafu %>% 
        parsing$split_characters() %>%
        rev()
      conversion_result <- purrr::reduce(
        .x = digits,
        .f = function(prior, digit) {
          increment <- if (digit == "=") {
            -2 * prior$multiplier
          } else if (digit == "-") {
            -prior$multiplier
          } else if (digit == "0") {
            0
          } else if (digit == "1") {
            prior$multiplier
          } else if (digit == "2") {
            2 * prior$multiplier
          } else {
            cli::cli_abort(c(
              "Possitble digits are '=', '-', '0', '1', '2'.",
              "x" = "Unknown digit {digit}"
            ))
          }
          
          list(
            value = prior$value + increment,
            multiplier = prior$multiplier * 5
          )
        },
        .init = list(
          value = 0,
          multiplier = 1
        )
      )
      conversion_result$value
    }
    
    
    modules::export("decimal_to_snafu")
    decimal_to_snafu <- function(decimal) {
      remaining <- decimal
      snafu_digits <- character()
      while (remaining != 0) {
        base_5_digit <- remaining %% 5
        snafu <- if (base_5_digit == 0) {
          list(
            digit = "0",
            shift = 0
          )
        } else if (base_5_digit == 1) {
          list(
            digit = "1",
            shift = 0
          )
        } else if (base_5_digit == 2) {
          list(
            digit = "2",
            shift = 0
          )
        } else if (base_5_digit == 3) {
          list(
            digit = "=",
            shift = 1
          )
        } else if (base_5_digit == 4) {
          list(
            digit = "-",
            shift = 1
          )
        } else {
          cli::cli_abort(c(
            "Digits in base-5 are integers between 0 and 4.",
            "x" = "Unexpected digit {remaining_bit}"
          ))
        }
        snafu_digits <- c(snafu$digit, snafu_digits)
        remaining <- floor(remaining / 5) + snafu$shift
      }
      paste0(snafu_digits, collapse = "")
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(fuel_requirements) {
      fuel_requirements %>%
        purrr::map_dbl(snafu_to_decimal) %>%
        sum() %>%
        decimal_to_snafu()
    }
  }
)


#' Parsed Input to Day25 of Advent Of Code 2022
#'
#' @param input_string Input string to day 25 of AdventOfCode
#'
#' @return A list of fuel requirements encoded in SNAFU
#' @importFrom magrittr %>%
parse_day25_input <- function(input_string) {
  day25$parse_input(input_string)
}

#' Solution to Day25 Part1 of Advent of Code 2022
#'
#' @param fuel_requirements A list of fuel fuel_requirements encoded in SNAFU
#'
#' @return The total fuel required encoded in SNAFU
#' @importFrom magrittr %>%
solve_day25_part1 <- function(fuel_requirements) {
  day25$solve_part1(fuel_requirements)
}
