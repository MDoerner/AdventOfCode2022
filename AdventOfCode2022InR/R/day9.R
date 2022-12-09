
parse_move_line <- function(line) {
  # Format is '<direction character> <distance>'
  list(
    direction = stringr::str_sub(line, start = 1, end = 1),
    distance = line %>%
      stringr::str_sub(start = 3) %>%
      readr::parse_integer()
  )
}

#' Parsed Input to Day9 of Advent Of Code 2022
#'
#' @param input_string Input string to day 9 of AdventOfCode
#'
#' @return A list of moves, each being a list with elements 'direction' out of ('U', 'D', 'L', 'R') and 'distance', an integer  
#' @importFrom magrittr %>%
parse_day9_input <- function(input_string) {
  input_string %>%
    split_lines() %>%
    purrr::map(parse_move_line)
}


direction_vectors <- list(
  "R" = c(1, 0),
  "L" = c(-1, 0),
  "U" = c(0, -1),
  "D" = c(0, 1)
)

move_in_direction <- function(point, direction) {
  move_vector <- direction_vectors[[direction]]
  point + move_vector
}

are_touching <- function(point1, point2) {
  distance <- point1 - point2
  (-1 <= distance[[1]] && 
    -1 <= distance[[2]] &&
    1 >= distance[[1]] && 
    1 >= distance[[2]])
}

drag_trailing_part <- function(leading_part, trailing_part) {
  if (are_touching(leading_part, trailing_part)) {
    trailing_part
  } else {
    offset <- leading_part - trailing_part
    offset_direction <- dplyr::if_else(
      condition = offset == 0,
      true = 0,
      false = offset / abs(offset)
    )
    trailing_part + offset_direction
  }
}


add_to_ticks_recorder <- function(ticks_at_places, point){
  add_to_count_store(ticks_at_places, encode_point(point))
}

move_head_in_direction <- function(rope, direction) {
  new_first_part <- move_in_direction(rope[[1]], direction)
  new_rope <- purrr::reduce(
    .x = rope[2:length(rope)],
    .f = function(new_rope_start, old_next_part) {
      prior_new_part <- new_rope_start[[length(new_rope_start)]]
      new_next_part = drag_trailing_part(prior_new_part, old_next_part)
      append(new_rope_start, list(new_next_part))
    },
    .init = list(new_first_part)
  )
  names(new_rope) <- names(rope)
  new_rope
}


move_head_in_direction_and_record_tail <- function(rope, direction, ticks_at_places) {
  new_rope <- move_head_in_direction(rope, direction)
  add_to_ticks_recorder(ticks_at_places, new_rope$tail)
  new_rope
}


execute_move <- function(rope, move, ticks_at_places) {
  move_once_with_recording <- purrr::partial(
    move_head_in_direction_and_record_tail,
    ticks_at_places = ticks_at_places
  )
  expanded_move <- rep(move$direction, times = move$distance)
  purrr::reduce(
    .x = expanded_move,
    .f = move_once_with_recording,
    .init = rope
  )
}

execute_move_series <- function(rope, moves, ticks_at_places) {
  move_with_recording <- purrr::partial(
    execute_move,
    ticks_at_places = ticks_at_places
  )
  purrr::reduce(
    .x = moves,
    .f = move_with_recording,
    .init = rope
  )
}



#' Solution to Day9 Part1 of Advent of Code 2022
#'
#' @param moves A list of moves, each being a list with elements 'direction' out of ('U', 'D', 'L', 'R') and 'distance', an integer
#'
#' @return The number of points the tail of an 11 link rope touches if the head moves according to the input
#' @importFrom magrittr %>%
solve_day9_part1 <- function(moves) {
  starting_rope <- list(
    head = c(0, 0),
    tail = c(0, 0)
  )
  ticks_at_places <- get_empty_count_store()
  add_to_ticks_recorder(ticks_at_places, starting_rope$tail)
  execute_move_series(starting_rope, moves, ticks_at_places)
  ticks_at_places %>%
    items_with_non_zero_count() %>%
    length() %>%
    as.character()
}


#' Solution to Day9 Part2 of Advent of Code 2022
#'
#' @param moves A list of moves, each being a list with elements 'direction' out of ('U', 'D', 'L', 'R') and 'distance', an integer
#'
#' @return The number of points the tail of an 11 link rope touches if the head moves according to the input
#' @importFrom magrittr %>%
solve_day9_part2 <- function(moves) {
  starting_rope <- list(
    head = c(0, 0),
    link_1 = c(0, 0),
    link_2 = c(0, 0),
    link_3 = c(0, 0),
    link_4 = c(0, 0),
    link_5 = c(0, 0),
    link_6 = c(0, 0),
    link_7 = c(0, 0),
    link_8 = c(0, 0),
    tail = c(0, 0)
  )
  ticks_at_places <- get_empty_count_store()
  add_to_ticks_recorder(ticks_at_places, starting_rope$tail)
  execute_move_series(starting_rope, moves, ticks_at_places)
  ticks_at_places %>%
    items_with_non_zero_count() %>%
    length() %>%
    as.character()
}
