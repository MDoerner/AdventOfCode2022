
#' Module containing the implementation for Day6 of Advent Of Code 2022
#' @noRd
day6 <- modules::module(
  {
    modules::export("parse_input")
    parse_input <- function(input_string) {
      stringr::str_split(input_string, "") %>% unlist()
    }
    
    
    
    signal_reader <- modules::module({
     
      queue <- modules::as.module("R/data_structures/queue.R")
      count_store <- modules::as.module("R/data_structures/count_store.R")
      
      modules::export("initialte")
      initialte <- function(initial_items) {
        list(
          "count_store" = purrr::reduce(
            .x = initial_items,
            .f = count_store$add,
            .init = count_store$empty_count_store()
          ),
          "read_queue" = purrr::reduce(
            .x = initial_items,
            .f = queue$enque,
            .init = queue$empty_queue()
          ),
          "head_position" = length(initial_items)
        )
      }
      
      modules::export("feed")
      feed <- function(signal_reader, item) {
        item_dropping_out <- queue$peek_queue(signal_reader$read_queue)
        list(
          "count_store" = signal_reader$count_store %>%
            count_store$remove(item_dropping_out) %>%
            count_store$add(item),
          "read_queue" = signal_reader$read_queue %>%
            queue$dequeue() %>%
            queue$enque(item),
          "head_position" = signal_reader$head_position + 1
        )
      }
      
      modules::export("max_signal")
      max_signal <- function(signal_reader) {
        count_store$max_count(signal_reader$count_store)
      }
    })
    
    feed_signal_reader_till_there_is_no_signal <- function(reader, signal) {
      while (signal_reader$max_signal(reader) > 1) {
        reader <- signal_reader$feed(
          reader,
          signal[[reader$head_position + 1]]
        )
      }
      reader
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(signal) {
      reader <- signal_reader$initialte(signal[1:4])
      reader <- feed_signal_reader_till_there_is_no_signal(reader, signal)
      as.character(reader$head_position)
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(signal) {
      reader <- signal_reader$initialte(signal[1:14])
      reader <- feed_signal_reader_till_there_is_no_signal(reader, signal)
      as.character(reader$head_position)
    }
  }
)


#' Parsed Input to Day6 of Advent Of Code 2022
#'
#' @param input_string Input string to day 6 of AdventOfCode
#'
#' @return A vector of single characters representing the signal
#' @importFrom magrittr %>%
parse_day6_input <- function(input_string) {
  day6$parse_input(input_string)
}

#' Solution to Day6 Part1 of Advent of Code 2022
#'
#' @param signal A vector of single characters representing the signal
#'
#' @return Position of the last input in the first run of 4 distinct items 
#' @importFrom magrittr %>%
solve_day6_part1 <- function(signal) {
  day6$solve_part1(signal)
}

#' Solution to Day6 Part2 of Advent of Code 2022
#'
#' @param signal A vector of single characters representing the signal
#'
#' @return Position of the last input in the first run of 14 distinct items 
#' @importFrom magrittr %>%
solve_day6_part2 <- function(signal) {
  day6$solve_part2(signal)
}
