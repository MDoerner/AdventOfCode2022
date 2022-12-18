
#' Module containing the implementation for Day18 of Advent Of Code 2022
#' @noRd
day18 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    point_encoder <- modules::use("R/utility/point_utils.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    queue <- modules::use("R/data_structures/queue.R") 
    
    parse_point <- function(text) {
      text %>%
        stringr::str_split(pattern = ",") %>%
        unlist() %>%
        purrr::map_int(readr::parse_integer)
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      input_string %>%
        parsing$split_lines() %>%
        purrr::map(parse_point)
    }
    
    store_point <- function(memory, point) {
      hashmap$set(memory, point_encoder$encode_point(point), TRUE)
    }
    
    has_point <- function(memory, point) {
      hashmap$has(memory, point_encoder$encode_point(point))
    }
    
    face_directions <- list(
      c(1L, 0L, 0L),
      c(0L, 1L, 0L),
      c(0L, 0L, 1L),
      c(-1L, 0L, 0L),
      c(0L, -1L, 0L),
      c(0L, 0L, -1L)
    )
    
    number_of_open_faces <- function(pixel_store, point) {
      face_directions %>%
        purrr::map_lgl(
          function(direction) {
            !has_point(pixel_store, point + direction)
          }
        ) %>% 
        sum()
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(pixels) {
      pixel_store <- purrr::reduce(
        .x = pixels,
        .f = store_point,
        .init = hashmap$empty_hashmap()
      )
      open_face_count <- purrr::partial(
        number_of_open_faces,
        pixel_store = pixel_store
      )
      face_count <- pixels %>%
        purrr::map_int(open_face_count) %>%
        sum()
      as.character(face_count)
    }
    
    enqueue_point <- function(point_queue, point) {
      queue$enqueue(point_queue, point_encoder$encode_point(point))
    }
    
    get_next_point <- function(point_queue) {
      point_encoder$decode_point(queue$peek(point_queue))
    }
    
    dequeue <- function(point_queue) {
      queue$dequeue(point_queue)
    }
    
    is_empty <- function(point_queue) {
      queue$is_empty(point_queue)
    }
    
    empty_queue <- function() {
      queue$empty_queue()
    }
    
    is_in_bounds <- function(point, enclosing_box) {
      all(enclosing_box$lower_bounds <= point) && 
        all(enclosing_box$upper_bounds >= point)
    }
    
    number_of_outside_faces <- function(pixel_store, enclosing_box) {
      encountered_faces <- 0L
      start_point <- enclosing_box$lower_bounds
      visited_points <- store_point(hashmap$empty_hashmap(), start_point)
      # Using a heap, because my naive queue implementation in R has bad enqueue and dequeue complexity
      # and I do not see the performance need to implement a proper queue.
      points_to_visit <- enqueue_point(empty_queue(), start_point) 
      while (!is_empty(points_to_visit)) {
        point <- get_next_point(points_to_visit)
        points_to_visit <- dequeue(points_to_visit)
        
        for (direction in face_directions) {
          candidate <- point + direction
          if (is_in_bounds(candidate, enclosing_box) && 
              !has_point(visited_points, candidate)) {
            if (has_point(pixel_store, candidate)) {
              encountered_faces <- encountered_faces + 1L
            } else {
              points_to_visit <- enqueue_point(points_to_visit, candidate)
              visited_points <- store_point(visited_points, candidate)
            }
          }
        }
      }
      encountered_faces
    }
    
    enclosing_box_for <- function(pixels) {
      x_coords <- purrr::map_int(
        .x = pixels,
        .f = function(point) point[[1]]
      )
      y_coords <- purrr::map_int(
        .x = pixels,
        .f = function(point) point[[2]]
      )
      z_coords <- purrr::map_int(
        .x = pixels,
        .f = function(point) point[[3]]
      )
      
      list(
        lower_bounds = c(min(x_coords) - 1L, min(y_coords) - 1L, min(z_coords) - 1L),
        upper_bounds = c(max(x_coords) + 1L, max(y_coords) + 1L, max(z_coords) + 1L)
      )
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(pixels) {
      pixel_store <- purrr::reduce(
        .x = pixels,
        .f = store_point,
        .init = hashmap$empty_hashmap()
      )
      enclosing_box <- enclosing_box_for(pixels)
      face_count <- number_of_outside_faces(pixel_store, enclosing_box)
      as.character(face_count)
    }
  }
)


#' Parsed Input to Day18 of Advent Of Code 2022
#'
#' @param input_string Input string to day 18 of AdventOfCode
#'
#' @return A list of points in three dimensions representing occupied pixels
#' @importFrom magrittr %>%
parse_day18_input <- function(input_string) {
  day18$parse_input(input_string)
}

#' Solution to Day18 Part1 of Advent of Code 2022
#'
#' @param pixels A list of points in three dimensions
#'
#' @return The number of the pixels' faces that do not immediately touch another pixel
#' @importFrom magrittr %>%
solve_day18_part1 <- function(pixels) {
  day18$solve_part1(pixels)
}

#' Solution to Day18 Part2 of Advent of Code 2022
#'
#' @param pixels A list of points in three dimensions
#'
#' @return The number of the pixels' faces accessible from the outside
#' @importFrom magrittr %>%
solve_day18_part2 <- function(pixels) {
  day18$solve_part2(pixels)
}
