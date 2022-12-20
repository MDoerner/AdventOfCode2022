
#' Module containing the implementation for Day20 of Advent Of Code 2022
#' @noRd
day20 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      input_string %>%
        parsing$split_lines() %>%
        purrr::map_int(readr::parse_integer)
    }
    
    store_index <- function(index_store, original_index, index) {
      hashmap$set(index_store, as.character(original_index), index)
    }
    
    get_index <- function(index_store, original_index) {
      hashmap$get(index_store, as.character(original_index))
    }
    
    move_index <- function(index_stores, values, move_index) {
      move_value <- values[[move_index]]
      move_from_index <- get_index(index_stores$index_by_original, move_index)
      move_to_index <- (move_from_index + move_value  - 1L) %% (length(values) - 1L) + 1L
      if (move_to_index < move_from_index) {
        for (index in rev(move_to_index:(move_from_index - 1L))) {
          original_index <- get_index(index_stores$original_by_index, index)
          new_index <- index + 1L
          index_stores$index_by_original <- store_index(index_stores$index_by_original, original_index, new_index)
          index_stores$original_by_index <- store_index(index_stores$original_by_index, new_index, original_index)
        }
        index_stores$index_by_original <- store_index(index_stores$index_by_original, move_index, move_to_index)
        index_stores$original_by_index <- store_index(index_stores$original_by_index, move_to_index, move_index)
      } else if (move_to_index > move_from_index) {
        for (index in (move_from_index + 1L):move_to_index) {
          original_index <- get_index(index_stores$original_by_index, index)
          new_index <- index - 1L
          index_stores$index_by_original <- store_index(index_stores$index_by_original, original_index, new_index)
          index_stores$original_by_index <- store_index(index_stores$original_by_index, new_index, original_index)
        }
        index_stores$index_by_original <- store_index(index_stores$index_by_original, move_index, move_to_index)
        index_stores$original_by_index <- store_index(index_stores$original_by_index, move_to_index, move_index)
      }
      index_stores
    }
    
    move_round <- function(index_stores, values) {
      purrr::reduce(
        .x = seq_along(values),
        .f = purrr::partial(
          move_index,
          values = values
        ),
        .init = index_stores
      )
    }
    
    move_rounds <- function(sequence, rounds) {
      index_store <- list(
        index_by_original = identity_mapping(seq_along(sequence)),
        original_by_index = identity_mapping(seq_along(sequence))
      )
      
      for (dummy in 1:rounds) {
        index_store <- move_round(index_store, sequence) 
      }
      
      original_by_index <- purrr::map_int(
        .x = seq_along(sequence),
        .f = function(index) get_index(index_store$original_by_index, index)
      )
      
      sequence[original_by_index]
    }
    
    identity_mapping <- function(unique_items) {
      purrr::reduce(
        .x = unique_items,
        .f = function(mapping, item) {
          hashmap$set(mapping, as.character(item), item)
        },
        .init = hashmap$empty_hashmap()
      )  
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
