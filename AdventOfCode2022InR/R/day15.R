
#' Module containing the implementation for Day15 of Advent Of Code 2022
#' @noRd
day15 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    parse_sensor <- function(line) {
      matches <- stringr::str_match(
        string = line, 
        pattern = "^Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)$"
      )
      coords <- matches[2:5] %>%
        purrr::map_int(readr::parse_integer)
      list(
        position = coords[1:2],
        closest_beacon = coords[3:4]
      )
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      list(
        test_row = 2000000L,
        coord_limit = 4000000L,
        sensors = input_string %>%
          parsing$split_lines() %>%
          purrr::map(parse_sensor)
      )
    }
    
    
    manhatten_distance <- function(point, other_point) {
      sum(abs(point - other_point))
    }
    
    covered_distance <- function(sensor) {
      manhatten_distance(sensor$position, sensor$closest_beacon)
    }
    
    covered_horizontal_range <- function(sensor, y) {
      range_at_row <- covered_distance(sensor) - abs(y - sensor$position[[2]])
      if (range_at_row < 0L) {
        NA
      } else {
        c(sensor$position[[1]] - range_at_row, sensor$position[[1]] + range_at_row)
      }
    }
    
    merge_ranges <- function(ranges) {
      ordering <- order(
        purrr::map_int(
          .x = ranges, 
          .f = function(range) range[[1]]
        )
      )
      sorted_ranges <- ranges[ordering]
      merged_ranges <- list()
      current_range <- sorted_ranges[[1]]
      for (range in sorted_ranges) {
        if (range[[1]] <= current_range[[2]]) {
          if (range[[2]] > current_range[[2]]) {
            current_range[[2]] <- range[[2]]
          }
        } else {
          merged_ranges <- append(merged_ranges, list(current_range))
          current_range <- range 
        }
      }
      append(merged_ranges, list(current_range))
    }
    
    non_beacon_places_on_row <- function(sensors, y) {
      covered_ranges <- purrr::map(
        .x = sensors, 
        .f = purrr::partial(
          covered_horizontal_range,
          y = y
        )
      )
      covered_ranges <- covered_ranges[!is.na(covered_ranges)]
      
      if (length(covered_ranges) == 0L) {
        return(0L)
      }
      
      merged_ranges <- merge_ranges(covered_ranges)
      covered_points <- merged_ranges %>%
        purrr::map_int(function(range) abs(range[[1]] - range[[2]]) + 1L) %>%
        sum()
      
      beacons_at_row <- sensors %>%
        purrr::map(function(sensor) sensor$closest_beacon) %>%
        unique %>%
        purrr::map_int( 
          function(beacon) {
            y == beacon[[2]]
          }
        ) %>%
        sum()
      
      covered_points - beacons_at_row
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(input) {
      non_beacon_places_on_row(input$sensors, input$test_row) %>%
        as.character()
    }
    
    
    detection_border_edges <- function(sensor) {
      dist <- covered_distance(sensor) + 1L
      upward <- list(
        list(
          start = sensor$position + c(-dist, 0L),
          end = sensor$position + c(0L, dist)
        ),
        list(
          start = sensor$position + c(0L, -dist),
          end = sensor$position + c(dist, 0L)
        )
      )
      downward <- list(
        list(
          start = sensor$position + c(-dist, 0L),
          end = sensor$position + c(0L, -dist)
        ),
        list(
          start = sensor$position + c(0L, dist),
          end = sensor$position + c(dist, 0L)
        )
      )
      list(
        upward = upward,
        downward = downward
      )
    }
    
    intersection_of_diagonals <- function(upward_diagonal, downward_diagonal) {
      start_diff <- downward_diagonal$start - upward_diagonal$start
      alpha <- start_diff[[1]] + start_diff[[2]]
      if (alpha %% 2L == 1L) {
        # no intersection on integer coordinates possible
        return(NA)
      }
      if (alpha < 0L) {
        # intersection would be beyond the left end of the upward diagonal
        return(NA)
      }
      
      candidate <- upward_diagonal$start + alpha / 2 * c(1L, 1L)
      
      if (candidate[[1]] < downward_diagonal$start[[1]]) {
        # intersection would be beyond the left end of the downward diagonal
        return(NA)
      }
      if (candidate[[1]] > downward_diagonal$end[[1]]) {
        # intersection would be beyond the right end of the downward diagonal
        return(NA)
      }
      if (candidate[[1]] > upward_diagonal$end[[1]]) {
        # intersection would be beyond the right end of the upward diagonal
        return(NA)
      }
      
      candidate
    }
    
    intersection_with_row_up <- function(upward_diagonal, y) {
      if (upward_diagonal$start[[2]] > y ||  upward_diagonal$end[[2]] < y) {
        return(NA)
      }
      upward_diagonal$start + (y - upward_diagonal$start[[2]]) * c(1L, 1L)
    }
    
    intersection_with_row_down <- function(downward_diagonal, y) {
      if (downward_diagonal$start[[2]] < y ||  downward_diagonal$end[[2]] > y) {
        return(NA)
      }
      downward_diagonal$start + (downward_diagonal$start[[2]] - y) * c(1L, -1L)
    }
    
    intersection_with_column <- function(diagonal, x) {
      if (diagonal$start[[1]] > x ||  diagonal$end[[2]] < x) {
        return(NA)
      }
      diagonal$start + (x - diagonal$start[[1]]) * c(1L, sign(diagonal$end[[2]] - diagonal$start[[2]]))
    }
    
    is_in_bounds <- function(point, coord_limit) {
      all(point >= 0) && all(point <= coord_limit)
    }
    
    is_not_in_sensor_range <- function(point, sensors) {
      sensors %>%
        purrr::map_lgl(
          function(sensor) {
            manhatten_distance(sensor$position, point) > covered_distance(sensor)
          }
        ) %>%
        all()
    }
    
    intersections_of_diagonals <- function(upward_diagonals, downward_diagonals) {
      intersections <- upward_diagonals %>% 
        purrr::map(
          function(upward_diagonal) {
            purrr::map(
              .x = downward_diagonals,
              .f = purrr::partial(
                intersection_of_diagonals,
                upward_diagonal = upward_diagonal
              )
            )
          }
        ) %>% 
        unlist(recursive = FALSE) 
      intersections[!is.na(intersections)] 
    }
    
    not_covered_point_in_range <- function(sensors, coord_limit) {
      sensor_range_boundaries <- purrr::map(
        .x = sensors,
        .f = detection_border_edges
      )
      upward_edges <- purrr::map(
        .x = sensor_range_boundaries,
        .f = function(boundary) boundary$upward
      ) %>%
        unlist(recursive = FALSE)
      downward_edges <- purrr::map(
        .x = sensor_range_boundaries,
        .f = function(boundary) boundary$downward
      ) %>%
        unlist(recursive = FALSE)
      
      # inside the boundary
      inner_candidates <- intersections_of_diagonals(upward_edges, downward_edges) %>%
        relevant_points(sensors, coord_limit)
      
      if (length(inner_candidates) > 0L) {
        return(inner_candidates[[1]])
      }
      
      # upward edges and boundary
      upper_candidates_up <- upward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_row_up,
          y = coord_limit
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
        
      if (length(upper_candidates_up) > 0L) {
        return(upper_candidates_up[[1]])
      }
      
      
      lower_candidates_up <- upward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_row_up,
          y = 0L
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(lower_candidates_up) > 0L) {
        return(lower_candidates_up[[1]])
      }
      
      
      right_candidates_up <- upward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_column,
          x = coord_limit
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(right_candidates_up) > 0L) {
        return(right_candidates_up[[1]])
      }
      
      
      left_candidates_up <- upward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_column,
          x = 0L
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(left_candidates_up) > 0L) {
        return(left_candidates_up[[1]])
      }
      
      
      # downward edges and boundary
      upper_candidates_down <- downward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_row_down,
          y = coord_limit
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(upper_candidates_down) > 0L) {
        return(upper_candidates_down[[1]])
      }
      
      
      lower_candidates_down <- downward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_row_down,
          y = 0L
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(lower_candidates_down) > 0L) {
        return(lower_candidates_down[[1]])
      }
      
      
      right_candidates_down <- downward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_column,
          x = coord_limit
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(right_candidates_down) > 0L) {
        return(right_candidates_down[[1]])
      }
      
      
      left_candidates_down <- downward_edges %>%
        purrr::map(purrr::partial(
          intersection_with_column,
          x = 0L
        )) %>%
        non_na_items() %>%
        relevant_points(sensors, coord_limit)
      
      if (length(left_candidates_down) > 0L) {
        return(left_candidates_down[[1]])
      }
      
      # there cannot be a non_covered point inside the limit.
      NA
    }
    
    non_na_items <- function(items) {
      items[!is.na(items)]
    }
    
    relevant_points <- function(points, sensors, coord_limit) {
      in_bounds <- purrr::partial(
        is_in_bounds,
        coord_limit = coord_limit
      )
      
      not_in_sensor_range <- purrr::partial(
        is_not_in_sensor_range,
        sensors = sensors
      )
      
      is_relevant <- purrr::map_lgl(points, in_bounds) & 
        purrr::map_lgl(points, not_in_sensor_range)
      points[is_relevant]
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(input) {
      beacon_position <- not_covered_point_in_range(input$sensors, input$coord_limit)
      tuning_frequency <- 4000000 * beacon_position[[1]] + beacon_position[[2]]
      as.character(tuning_frequency)
    }
  }
)


#' Parsed Input to Day15 of Advent Of Code 2022
#'
#' @param input_string Input string to day 15 of AdventOfCode
#'
#' @return A list with a test row, a coordinate limit and a list of sensors, each with its position and the one of the closest beacon
#' @importFrom magrittr %>%
parse_day15_input <- function(input_string) {
  day15$parse_input(input_string)
}

#' Solution to Day15 Part1 of Advent of Code 2022
#'
#' @param input A list with a test row, a coordinate limit and a list of sensors, each with its position and the one of the closest beacon
#'
#' @return Points on the test row that cannot contain a beacon
#' @importFrom magrittr %>%
solve_day15_part1 <- function(input) {
  day15$solve_part1(input)
}

#' Solution to Day15 Part2 of Advent of Code 2022
#'
#' @param input A list with a test row, a coordinate limit and a list of sensors, each with its position and the one of the closest beacon
#'
#' @return Tuning frequency of the unique point in range not covered by the sensors
#' @importFrom magrittr %>%
solve_day15_part2 <- function(input) {
  day15$solve_part2(input)
}
