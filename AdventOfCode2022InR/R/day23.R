
#' Module containing the implementation for Day23 of Advent Of Code 2022
#' @noRd
day23 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    point_encoder <- modules::use("R/utility/point_utils.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    count_store <- modules::use("R/data_structures/count_store.R")
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      converter <- function(c) if (c == "#") TRUE else NULL
      area_map <- parsing$to_hashmap(
        text = input_string, 
        converter = converter,
        ignore_null = TRUE
      )
      positions <- area_map %>%
        hashmap$as_list() %>%
        names() %>%
        purrr::map(point_encoder$decode_point)
      list(
        map = area_map,
        positions = positions
      )
    }
    
    
    is_occupied <- function(area_map, point) {
      hashmap$has(area_map, point_encoder$encode_point(point))
    }
    
    free_position <- function(area_map, point) {
      hashmap$remove(area_map, point_encoder$encode_point(point))
    }
    
    mark_as_occupied <- function(area_map, point) {
      hashmap$set(area_map, point_encoder$encode_point(point), TRUE)
    }
    
    declare_move_target <- function(move_target_store, point) {
      count_store$add(move_target_store, point_encoder$encode_point(point))
    }
    
    number_of_elves_moving_to <- function(move_target_store, point) {
      count_store$count(move_target_store, point_encoder$encode_point(point))
    }
    
    empty_move_target_store <- function() {
      count_store$empty_count_store()
    }
    
    directions <- list(
      NW = c(-1L, -1L),
      N = c(0L, -1L),
      NE = c(1L, -1L),
      E = c(1L, 0L),
      SE = c(1L, 1L),
      S = c(0L, 1L),
      SW = c(-1L, 1L),
      W = c(-1L, 0L)
    )
    
    
    will_try_to_move <- function(point, area_map) {
      has_neighbour <- directions %>%
        purrr::map_lgl(
          function(direction) {
            potential_neighbour = point + direction
            occupied <- is_occupied(area_map, potential_neighbour)
            occupied
          }
        ) %>%
        any()
      has_neighbour
    }
    
    move_checks <- list(
      list(
        direction = "N",
        checks = c("NW", "N", "NE")
      ),
      list(
        direction = "S",
        checks = c("SW", "S", "SE")
      ),
      list(
        direction = "W",
        checks = c("NW", "W", "SW")
      ),
      list(
        direction = "E",
        checks = c("NE", "E", "SE")
      )
    ) 
    
    desired_move_direction <- function(point, start_move_index, area_map) {
      for (index_offset in 0:(length(move_checks) - 1)) {
        move_index <- (start_move_index + index_offset - 1) %% length(move_checks) + 1L
        move_check <- move_checks[[move_index]]
        test_position_occupied <- move_check$checks %>%
          purrr::map_lgl(
            function(direction_id) {
              position <- point + directions[[direction_id]]
              is_occupied(area_map, position)
            }
          )  
        if (!any(test_position_occupied)) {
          return(move_check$direction)
        }
      }
      NA
    }
    
    move_one_round <- function(positions, area_map, start_move_index) {
      total_elves_count <- length(positions)
      attempts_to_move <- purrr::map_lgl(
        .x = positions,
        .f = purrr::partial(
          will_try_to_move,
          area_map = area_map
        )
      )
      elves_not_attempting_to_move <- positions[!attempts_to_move]
      elves_attempting_to_move <- positions[attempts_to_move]
      elves_attempting_to_move_count <- length(elves_attempting_to_move)
      
      # print(paste0("attemptingto move: ", elves_attempting_to_move_count))
      # print(paste0("not attempting to move: ", length(elves_not_attempting_to_move)))
      # browser()
      
      desired_directions <- purrr::map_chr(
        .x = elves_attempting_to_move,
        .f = purrr::partial(
          desired_move_direction,
          area_map = area_map,
          start_move_index = start_move_index
        )
      )
      chooses_to_move = !is.na(desired_directions)
      elves_not_choosing_to_move <- append(elves_not_attempting_to_move, elves_attempting_to_move[!chooses_to_move])
      elves_choosing_to_move <- elves_attempting_to_move[chooses_to_move]
      elves_choosing_to_move_count <- length(elves_choosing_to_move)
      chosen_directions <- desired_directions[chooses_to_move]
      chosen_destinations <- purrr::map2(
        .x = elves_choosing_to_move,
        .y = chosen_directions,
        .f = function(elf, direction_id) {
          elf + directions[[direction_id]]
        }
      )
      
      # print(paste0("choosing to move: ", elves_choosing_to_move_count))
      # print(paste0("choosing not to move: ", length(elves_not_choosing_to_move)))
      # browser()
      
      move_target_store <- purrr::reduce(
        .x = chosen_destinations,
        .f = declare_move_target,
        .init = empty_move_target_store()
      )
      is_blocked <- purrr::map_lgl(
        .x = chosen_destinations,
        .f = function(destination) {
          number_of_elves_moving_to(move_target_store, destination) > 1
        }
      )
      elves_not_moving <- append(elves_not_choosing_to_move, elves_choosing_to_move[is_blocked])
      elves_moving <- elves_choosing_to_move[!is_blocked]
      moving_elves_count <- length(elves_moving)
      used_destinations <- chosen_destinations[!is_blocked]
      
      # print(paste0("moving: ", moving_elves_count))
      # print(paste0("not moving: ", length(elves_not_moving)))
      # browser()
      
      new_positions <- append(elves_not_moving, used_destinations)
      new_area_map <- purrr::reduce(
        .x = seq_along(elves_moving),
        .f = function(current_map, index) {
          current_map <- mark_as_occupied(current_map, used_destinations[[index]])
          free_position(current_map, elves_moving[[index]])
        },
        .init = area_map
      )
      
      list(
        total = total_elves_count,
        attempting = elves_attempting_to_move_count,
        choosing = elves_choosing_to_move_count,
        moving = moving_elves_count,
        positions = new_positions,
        map = new_area_map
      )
    }
    
    move_rounds <- function(start_positions, area_map, rounds = NULL) {
      start_move_index <- 1L
      positions <- start_positions
      round <- 1L
      stability_reached <- FALSE
      
      # writeLines(draw_enclosing_space(positions))
      # browser()
      
      while (!stability_reached && (is.null(rounds) || round <= rounds)) {
        move_result <- move_one_round(positions, area_map, start_move_index)
        positions <- move_result$positions
        area_map <- move_result$map
        stability_reached <- move_result$attempting == 0L
        start_move_index <- start_move_index %% length(move_checks) + 1L
        round <- round + 1L
        
        # writeLines(draw_enclosing_space(positions))
        # browser()
      }
      list(
        rounds = round - 1L,
        positions = positions,
        map = area_map
      )
    }
    
    covered_area <- function(positions) {
      x_coords_used <- purrr::map_dbl(
        .x = positions,
        .f = function(point) {
          point[[1]]
        }
      )
      y_coords_used <- purrr::map_dbl(
        .x = positions,
        .f = function(point) {
          point[[2]]
        }
      )
      width <- max(x_coords_used) - min(x_coords_used) + 1L
      height <- max(y_coords_used) - min(y_coords_used) + 1L
      width * height
    }
    
    in_point_list <- function(point_list, point) {
      Position(function(other_point) all(other_point == point), point_list, nomatch = 0L) > 0L
    }
    
    draw_enclosing_space <- function(positions) {
      x_coords_used <- purrr::map_dbl(
        .x = positions,
        .f = function(point) {
          point[[1]]
        }
      )
      y_coords_used <- purrr::map_dbl(
        .x = positions,
        .f = function(point) {
          point[[2]]
        }
      )
      x_coords <- (min(x_coords_used) - 1L):(max(x_coords_used) + 1L)
      y_coords <- (min(y_coords_used) - 1L):(max(y_coords_used) + 1L)
      lines <- y_coords %>%
        purrr::map(
          function(y) {
            chars <- x_coords %>%
              purrr::map(
                function(x) {
                  if (in_point_list(positions, c(x, y))) {
                    "#"
                  } else {
                    "."
                  }
                }
              )
            paste0(chars, collapse = "")
          }
        )
      paste0(lines, collapse = "\n")
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(input) {
      move_result <- move_rounds(
        start_positions = input$positions, 
        area_map = input$map, 
        rounds = 10L
      )
      end_positions <- move_result$positions
      # writeLines(draw_enclosing_space(end_positions))
      # browser()
      result <- covered_area(end_positions) - length(end_positions)
      as.character(result)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(input) {
      move_result <- move_rounds(
        start_positions = input$positions, 
        area_map = input$map
      )
      # end_positions <- move_result$positions
      # writeLines(draw_enclosing_space(end_positions))
      # browser()
      result <- move_result$rounds
      as.character(result)
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
