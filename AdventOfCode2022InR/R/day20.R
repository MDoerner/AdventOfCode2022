
#' Module containing the implementation for Day20 of Advent Of Code 2022
#' @noRd
day20 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      input_string %>%
        parsing$split_lines() %>%
        purrr::map_int(readr::parse_integer)
    }
    
    get_original_indices <- function(indices) {
      match(seq_along(indices), indices)
    }
    
    move_index <- function(indices, values, move_index, input_length) {
      move_value <- values[[move_index]]
      if (move_value == 0) {
        return(indices)
      }
      move_from_index <- indices[move_index[]]
      move_to_index <- (move_from_index + move_value  - 1L) %% (input_length - 1L) + 1L
      if (move_to_index < move_from_index) {
        item_move <- as.integer(indices < move_from_index & indices >= move_to_index)
        item_move[[move_index]] <- move_to_index - move_from_index
        indices <- indices + item_move
      } else if (move_to_index > move_from_index) {
        item_move <- -as.integer(indices > move_from_index & indices <= move_to_index)
        item_move[[move_index]] <- move_to_index - move_from_index
        indices <- indices + item_move
      }

      indices
    }
    
    move_round <- function(indices, values, input_length) {
      purrr::reduce(
        .x = seq_along(values),
        .f = purrr::partial(
          move_index,
          values = values,
          input_length = input_length
        ),
        .init = indices
      )
    }
    
    move_rounds <- function(sequence, rounds) {
      indices <- seq_along(sequence)
      
      for (dummy in 1:rounds) {
        indices <- move_round(indices, sequence, length(sequence)) 
      }
      
      original_by_index <- get_original_indices(indices)
      
      sequence[original_by_index]
    } 
    
    modules::export("solve_part1")
    solve_part1 <- function(sequence) {
      new_sequence <- move_rounds(sequence, 1L)
      offsets <- c(1000L, 2000L, 3000L)
      zero_index <- match(0L, new_sequence)
      indices <- (zero_index + offsets - 1L) %% length(sequence) + 1L
      results <- new_sequence[indices]
      results %>%
        sum() %>%
        as.character()
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(sequence) {
      adjusted_sequence <- 811589153 * sequence
      new_sequence <- move_rounds(adjusted_sequence, 10L)
      offsets <- c(1000L, 2000L, 3000L)
      zero_index <- match(0L, new_sequence)
      indices <- (zero_index + offsets - 1L) %% length(sequence) + 1L
      results <- new_sequence[indices]
      results %>%
        sum() %>%
        as.character()
    }
  }
)


#' Parsed Input to Day20 of Advent Of Code 2022
#'
#' @param input_string Input string to day 20 of AdventOfCode
#'
#' @return An integer vector
#' @importFrom magrittr %>%
parse_day20_input <- function(input_string) {
  day20$parse_input(input_string)
}

#' Solution to Day20 Part1 of Advent of Code 2022
#'
#' @param sequence An integer vector
#'
#' @return The sum of the 1000th, 2000th and 3000th element after 0 after shifting one round
#' @importFrom magrittr %>%
solve_day20_part1 <- function(sequence) {
  day20$solve_part1(sequence)
}

#' Solution to Day20 Part2 of Advent of Code 2022
#'
#' @param sequence An integer vector
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day20_part2 <- function(sequence) {
  day20$solve_part2(sequence)
}
