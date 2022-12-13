
#' Module containing the implementation for Day12 of Advent Of Code 2022
#' @noRd
day12 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    coordinates_in_input <- function(input_string, c) {
      index <- stringr::str_locate(input_string, c)[[1]]
      line_length <- stringr::str_locate(input_string, "\n")[[1]]
      c(
        index %% line_length,
        ceiling(index / line_length)
      )
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      converter <- function(c) {
        if (c == "S") {
          0L
        } else if (c == "E") {
          25L
        } else {
          utf8ToInt(c) - 97L # 'a' has the code point 97
        }
      }
      height_map <- parsing$to_hashmap(input_string, converter)
      start <- coordinates_in_input(input_string, "S")
      end <- coordinates_in_input(input_string, "E")
      list(
        start = start,
        end = end,
        height_map = height_map
      )
    }
    
    dijkstra <- modules::use("R/algorithms/dijkstra.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    point_encoder <- modules::use("R/utility/point_utils.R")
    
    connections_of_fct <- function(height_map, heights_compatible) {
      map <- height_map$height_map
      function(point) {
        candidates <- list(
          c(-1, 0) + point,
          c(0, 1) + point,
          c(1, 0) + point,
          c(0, -1) + point
        )
        
        on_map <- purrr::map_lgl(
          candidates,
          function(p) hashmap$has(map, point_encoder$encode_point(p))
        ) 
        candidates <- candidates[on_map]
        
        current_height <- hashmap$get(map, point_encoder$encode_point(point))
        heights <- on_map <- purrr::map_int(
          candidates,
          function(p) hashmap$get(map, point_encoder$encode_point(p))
        )
        candidates <- candidates[heights_compatible(current_height, heights)]
        
        purrr::map(
          candidates,
          function(p) list(
            destination = p,
            distance = 1L
          )
        )
      }
    }
    
    
    modules::export("solve_part1")
    solve_part1 <- function(height_map) {
      heights_compatible <- function(current_height, other_height) {
        current_height + 1 >= other_height
      }
      connections_of <- connections_of_fct(height_map, heights_compatible)
      min_distance <- dijkstra$shortest_path_distance(height_map$start, height_map$end, connections_of)
      as.character(min_distance)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(height_map) {
      heights_compatible <- function(current_height, other_height) {
        current_height - 1 <= other_height
      }
      connections_of <- connections_of_fct(height_map, heights_compatible)
      end_condition <- function(current_location) {
        hashmap$get(height_map$height_map, point_encoder$encode_point(current_location)) == 0L
      }
      min_distance <- dijkstra$shortest_path_distance_until(height_map$end, end_condition, connections_of)
      as.character(min_distance)
    }
  }
)


#' Parsed Input to Day12 of Advent Of Code 2022
#'
#' @param input_string Input string to day 12 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day12_input <- function(input_string) {
  day12$parse_input(input_string)
}

#' Solution to Day12 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day12_part1 <- function(height_map) {
  day12$solve_part1(height_map)
}

#' Solution to Day12 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day12_part2 <- function(height_map) {
  day12$solve_part2(height_map)
}
