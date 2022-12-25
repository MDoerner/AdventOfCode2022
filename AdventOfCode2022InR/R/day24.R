
#' Module containing the implementation for Day24 of Advent Of Code 2022
#' @noRd
day24 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    point_encoder <- modules::use("R/utility/point_utils.R")
    queue <- modules::use("R/data_structures/queue.R") 
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      lines <- parsing$split_lines(input_string)
      height <- length(lines) - 2L
      width <- nchar(lines[[1]]) - 2L
      blizzards <- list()
      directions <- list()
      for (y in seq_along(lines)) {
        if (y %in% c(1, length(lines))) {
          next
        }
        chars <- parsing$split_characters(lines[[y]])
        for (x in seq_along(chars)) {
          c <- chars[[x]]
          if (c == "<") {
            # This is not efficient, but good enough here. 
            blizzards <- append(blizzards, list(c(x - 2L, y - 2L)))
            directions <- append(directions, list(c(-1L, 0L)))
          } else if (c == ">") {
            # This is not efficient, but good enough here. 
            blizzards <- append(blizzards, list(c(x - 2L, y - 2L)))
            directions <- append(directions, list(c(1L, 0L)))
          } else if (c == "^") {
            # This is not efficient, but good enough here. 
            blizzards <- append(blizzards, list(c(x - 2L, y - 2L)))
            directions <- append(directions, list(c(0L, -1L)))
          } else if (c == "v") {
            # This is not efficient, but good enough here. 
            blizzards <- append(blizzards, list(c(x - 2L, y - 2L)))
            directions <- append(directions, list(c(0L, 1L)))
          } 
        }
      }
      list(
        height = height,
        width = width,
        blizzards = blizzards,
        directions = directions
      )
    }
    
    move_blizzards <- function(blizzards, directions, height, width) {
      purrr::map2(
        .x = blizzards,
        .y = directions,
        .f = function(blizzard, direction) {
          (blizzard + direction) %% c(width, height)
        }
      )
    }
    
    move_directions <- list(
      c(0L, 1L),
      c(1L, 0L),
      c(0L, 0L),
      c(-1L, 0L),
      c(0L, -1L)
    )
    
    in_point_list <- function(point_list, point) {
      Position(function(other_point) all(other_point == point), point_list, nomatch = 0L) > 0L
    }
    
    possible_next_moves <- function(position, blizzards, height, width) {
      move_directions %>%
        purrr::map(
          function(direction) {
            direction + position
          }
        ) %>% 
        purrr::discard(
          function(point) {
            if (all(point == c(0L, -1L)) 
                || all(point == c(width - 1L, height))) {
              # start- and end-point are OK
              return(FALSE)
            }
            if (any(point < 0L) 
                || any(point >= c(width, height))) {
              # out of bounds
              return(TRUE)
            }
            # in a blizzard
            in_point_list(blizzards, point)
          }
        )
    }
    
    peek_point <- function(point_queue) {
      point_encoder$decode_point(queue$peek(point_queue))
    }
    
    enqueue_point <- function(point_queue, point) {
      queue$enqueue(point_queue, point_encoder$encode_point(point))
    }
    
    traverse_blizzard_field <- function(traverse_down, blizzards, directions, height, width) {
      start_point <- if (traverse_down) c(0L, -1L) else c(width - 1L, height)
      end_point <- if (traverse_down) c(width - 1L, height) else c(0L, -1L)
      current_blizzards <- blizzards
      minutes_passed <- 0L
      current_positions <- queue$empty_queue() %>%
        enqueue_point(start_point)
      while (!queue$is_empty(current_positions)) {
        current_blizzards <- move_blizzards(
          current_blizzards, 
          directions, 
          height, 
          width
        )
        minutes_passed <- minutes_passed + 1L
        
        next_positions <- queue$empty_queue()
        while (!queue$is_empty(current_positions)) {
          position <- peek_point(current_positions)
          
          if (all(position == end_point)) {
            return(minutes_passed)
          }
          
          next_from_here <- possible_next_moves(
            position, 
            current_blizzards, 
            height, 
            width
          )
          
          for (move_target in next_from_here) {
            if (all(move_target == end_point)) {
              return(list(
                minutes_passed = minutes_passed,
                blizzards = current_blizzards
              ))
            } else {
              next_positions <- enqueue_point(next_positions, move_target)
            }
          }
          current_positions <- queue$dequeue(current_positions)
        }
        current_positions <- queue$deduplicate(next_positions)
      }
      NA
    }
    
    
    modules::export("solve_part1")
    solve_part1 <- function(blizzard_field) {
      result <- traverse_blizzard_field(
        traverse_down = TRUE,
        blizzards = blizzard_field$blizzards,
        directions = blizzard_field$directions,
        height = blizzard_field$height,
        width = blizzard_field$width
      )
      as.character(result$minutes_passed)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(blizzard_field) {
      first_result <- traverse_blizzard_field(
        traverse_down = TRUE,
        blizzards = blizzard_field$blizzards,
        directions = blizzard_field$directions,
        height = blizzard_field$height,
        width = blizzard_field$width
      )
      back_result <- traverse_blizzard_field(
        traverse_down = FALSE,
        blizzards = first_result$blizzards,
        directions = blizzard_field$directions,
        height = blizzard_field$height,
        width = blizzard_field$width
      )
      second_result <- traverse_blizzard_field(
        traverse_down = TRUE,
        blizzards = back_result$blizzards,
        directions = blizzard_field$directions,
        height = blizzard_field$height,
        width = blizzard_field$width
      )
      total_minutes_passed <- first_result$minutes_passed + back_result$minutes_passed + second_result$minutes_passed
      as.character(total_minutes_passed)
    }
  }
)


#' Parsed Input to Day24 of Advent Of Code 2022
#'
#' @param input_string Input string to day 24 of AdventOfCode
#'
#' @return A list consisting of a list the valley's height, its width, the initial blizzard positions and their directions
#' @importFrom magrittr %>%
parse_day24_input <- function(input_string) {
  day24$parse_input(input_string)
}

#' Solution to Day24 Part1 of Advent of Code 2022
#'
#' @param blizzard_field A list consisting of a list the valley's height, its width, the initial blizzard positions and their directions
#'
#' @return The minimum time to traverse the valley dodging all blizzards
#' @importFrom magrittr %>%
solve_day24_part1 <- function(blizzard_field) {
  day24$solve_part1(blizzard_field)
}

#' Solution to Day24 Part2 of Advent of Code 2022
#'
#' @param blizzard_field A list consisting of a list the valley's height, its width, the initial blizzard positions and their directions
#'
#' @return The minimum time to traverse the valley, ho back to the start and then traverse it again dodging all blizzards
#' @importFrom magrittr %>%
solve_day24_part2 <- function(blizzard_field) {
  day24$solve_part2(blizzard_field)
}
