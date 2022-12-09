



#' Parsed Input to Day6 of Advent Of Code 2022
#'
#' @param input_string Input string to day 6 of AdventOfCode
#'
#' @return A vector of single characters representing the signal
#' @importFrom magrittr %>%
parse_day6_input <- function(input_string) {
  stringr::str_split(input_string, "") %>% unlist()
}


## Queue Without Safety Checks

get_empty_queue <- function() {
  c()
}

enque <- function(queue, item) {
  c(item, queue)
}

peek_queue <- function(queue) {
  queue[[length(queue)]]
}

dequeue <- function(queue) {
  queue[1:(length(queue) - 1)]
}


## Signal Reader

initialte_signal_reader <- function(initial_items) {
  list(
    "count_store" = purrr::reduce(
      .x = initial_items,
      .f = add_to_count_store,
      .init = get_empty_count_store()
    ),
    "read_queue" = purrr::reduce(
      .x = initial_items,
      .f = enque,
      .init = get_empty_queue()
    ),
    "head_position" = length(initial_items)
  )
}

feed_signal_reader <- function(signal_reader, item) {
  item_dropping_out <- peek_queue(signal_reader$read_queue)
  list(
    "count_store" = signal_reader$count_store %>%
      remove_from_count_store(item_dropping_out) %>%
      add_to_count_store(item),
    "read_queue" = signal_reader$read_queue %>%
      dequeue() %>%
      enque(item),
    "head_position" = signal_reader$head_position + 1
  )
}

#' Solution to Day6 Part1 of Advent of Code 2022
#'
#' @param signal A vector of single characters representing the signal
#'
#' @return Position of the last input in the first run of 4 distinct items 
#' @importFrom magrittr %>%
solve_day6_part1 <- function(signal) {
  signal_reader <- initialte_signal_reader(signal[1:4])
  while (max_count(signal_reader$count_store) > 1) {
    signal_reader <- feed_signal_reader(
      signal_reader,
      signal[[signal_reader$head_position + 1]]
    )
  }
  as.character(signal_reader$head_position)
}


#' Solution to Day6 Part2 of Advent of Code 2022
#'
#' @param signal A vector of single characters representing the signal
#'
#' @return Position of the last input in the first run of 14 distinct items 
#' @importFrom magrittr %>%
solve_day6_part2 <- function(signal) {
  signal_reader <- initialte_signal_reader(signal[1:14])
  while (max_count(signal_reader$count_store) > 1) {
    signal_reader <- feed_signal_reader(
      signal_reader,
      signal[[signal_reader$head_position + 1]]
    )
  }
  as.character(signal_reader$head_position)
}
