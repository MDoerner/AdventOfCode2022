
#' Module containing the implementation for Day14 of Advent Of Code 2022
#' @noRd
day14 <- modules::module(
  {
    
    modules::export("cave")
    cave <- modules::module({
      
      hashmap <- modules::use("R/data_structures/hashmap.R")
      point_encoder <- modules::use("R/utility/point_utils.R")
      
      modules::export("empty_cave")
      empty_cave <- function() {
        hashmap$empty_hashmap()
      }
      
      
      has_segment <- function(cave, x) {
        hashmap$has(cave, as.character(x))
      }
      
      get_segments <- function(cave, x) {
        hashmap$get_or_default(cave, as.character(x), list())
      }
      
      set_segments <- function(cave, x, segments) {
        hashmap$set(cave, as.character(x), segments)
      }
      
      # Sorts first and then second; assumes no section longer than 9999
      sort_segments <- function(segments) {
        sort_order <- segments %>%
          purrr::map_int(function(range) range[[1]] * 10000L + (range[[2]] - range[[1]])) %>%
          order()
        segments[sort_order]
      }
      
      index_of_first_below <- function(segments, start) {
        index <- 1L
        while (index <= length(segments) && segments[[index]][[1]] <= start) {
          index <- index + 1L
        }
        if (index > length(segments)) {
          NA_integer_
        } else {
          index
        }
      }
      
      modules::export("first_occupied_below")
      first_occupied_below <- function(cave, point) {
        x <- point[[1]]
        if (!has_segment(cave, x)) {
          return(NA)
        } 
        vertical_segments <- get_segments(cave, x)
        y_index <- index_of_first_below(vertical_segments, point[[2]])
        if (is.na(y_index)) {
          return(NA)
        } 
        y <- vertical_segments[[y_index]][[1]]
        c(x, y)
      }
      
      modules::export("is_occupied")
      is_occupied <- function(cave, point) {
        x <- point[[1]]
        y <- point[[2]]
        if (!has_segment(cave, x)) {
          return(FALSE)
        } 
        vertical_segments <- get_segments(cave, x)
        any(
          purrr::map_lgl(
            .x = vertical_segments,
            .f = function(segment) {
              segment[[1]] <= y && y <= segment[[2]]
            }
          )
        )
      }
      
      modules::export("add_vertical_segment")
      add_vertical_segment <- function(cave, x, vertical_range) {
        segments <- get_segments(cave, x) %>%
          append(list(vertical_range)) %>%
          sort_segments()
        set_segments(cave, x, segments)
      }
      
      modules::export("add_horizontal_segment")
      add_horizontal_segment <- function(cave, y, horizontal_range) {
        x_coords <- horizontal_range[[1]]:horizontal_range[[2]]
        purrr::reduce(
          .x = x_coords,
          .f = purrr::partial(
            add_vertical_segment,
            vertical_range = c(y, y)
          ),
          .init = cave
        )
      }
      
      modules::export("extend_vertical_segment")
      extend_vertical_segment <- function(cave, point) {
        x <- point[[1]]
        y <- point[[2]]
        if (!has_segment(cave, x)) {
          cli::cli_abort(c(
            "Vertical segments can only be extended with points immediately above.",
            "x" = "There is no vertical segment at x = {x}."))
        }
        
        vertical_segments <- get_segments(cave, x)
        y_index <- index_of_first_below(vertical_segments, y)
        
        if (is.na(y_index)) {
          cli::cli_abort(c(
            "Vertical segments can only be extended with points immediately above.",
            "x" = "There is no vertical segment at x = {x} below y = {y}."))
        } 
        
        segment <- vertical_segments[[y_index]]
        if (segment[[1]] != 1L + y) {
          cli::cli_abort(c(
            "Vertical segments can only be extended with points immediately above.",
            "x" = "There is no vertical segment at x = {x} below y = {y + 1}.",
            "x" = "The next segment starts at y = {segment[[1]]}."
            ))
        }
        
        segment[[1]] <- y
        vertical_segments[[y_index]] <- segment
        set_segments(cave, x, vertical_segments)
      }
      
      modules::export("as_list")
      as_list <- function(cave_map) {
        hashmap$as_list(cave_map)
      }
      
      modules::export("draw")
      draw <- function(cave_map) {
        map_list <- as_list(cave_map)
        x_coords_used <- names(map_list) %>%
          purrr::map_int(readr::parse_integer)
        x_coords <- min(x_coords_used):max(x_coords_used)
        y_coords_used <- c(unlist(map_list), 0L)
        y_coords <- min(y_coords_used):max(y_coords_used)
        
        lines <- y_coords %>%
          purrr::map(
            function(y) {
              chars <- x_coords %>%
                purrr::map(
                  function(x) {
                    if (x == 500L && y == 0L) {
                      "+"
                    } else if (is_occupied(cave_map, c(x, y))) {
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
    })
    
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    modules::export("parse_segment_specifiers")
    parse_segment_specifiers <- function(text) {
      text %>%
        parsing$split_lines() %>%
        purrr::map(function(line) {
          line %>%
            stringr::str_split(pattern = " -> ") %>%
            unlist() %>%
            purrr::map(function(point_text) {
              point_text %>% 
                stringr::str_split(pattern = ",") %>%
                unlist() %>%
                purrr::map_int(readr::parse_integer)
            })
        })
    }
    
    cave_from_segments <- function(segment_specifiers) {
      sand_cave <- cave$empty_cave()
      for (wall in segment_specifiers) {
        section_start_indices <- 1:(length(wall) - 1)
        for (index in section_start_indices) {
          section_start <- wall[[index]]
          section_end <- wall[[index + 1]]
          if (section_start[[1]] == section_end[[1]]) {
            sand_cave <- cave$add_vertical_segment(
              sand_cave, 
              section_start[[1]], 
              sort(c(section_start[[2]], section_end[[2]]))
            )
          } else {
            sand_cave <- cave$add_horizontal_segment(
              sand_cave, 
              section_start[[2]], 
              sort(c(section_start[[1]], section_end[[1]]))
            )
          }
        }
      }
      sand_cave
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      input_string %>%
        parse_segment_specifiers() %>%
        cave_from_segments()
    }
    
    
    
    sand_destination <- function(sand_cave, drop_point) {
      blocked <- FALSE
      current_point <- drop_point
      while (!blocked) {
        first_obstacle <- cave$first_occupied_below(sand_cave, current_point)
        if (any(is.na(first_obstacle))) {
          return(NA)
        }
        left_candidate <- first_obstacle + c(-1L, 0L)
        if (!cave$is_occupied(sand_cave, left_candidate)) {
          current_point <- left_candidate
        } else {
          right_candidate <- first_obstacle + c(1L, 0L)
          if (!cave$is_occupied(sand_cave, right_candidate)) {
            current_point <- right_candidate
          } else {
            current_point <- first_obstacle + c(0L, -1L)
            blocked <- TRUE
          }
        }
      }
      current_point
    }
    
    drop_sand <- function(sand_cave, drop_point) {
      sand_dropped <- 0L
      last_sand_resting_spot <- sand_destination(sand_cave, drop_point)
      while (!any(is.na(last_sand_resting_spot)) && any(last_sand_resting_spot != drop_point)) {
        sand_cave <- cave$extend_vertical_segment(sand_cave, last_sand_resting_spot)
        sand_dropped <- sand_dropped + 1L
        last_sand_resting_spot <- sand_destination(sand_cave, drop_point)
      }
      if (!any(is.na(last_sand_resting_spot))) {
        sand_dropped <- sand_dropped + 1L
      }
      list(
        sand_cave = sand_cave,
        sand_dropped = sand_dropped
      )
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(sand_cave) {
      final_state <- drop_sand(sand_cave, c(500L, 0L))
      as.character(final_state$sand_dropped)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(sand_cave) {
      max_y <- cave$as_list(sand_cave) %>%
        unlist() %>%
        max()
      sand_cave <- cave$add_horizontal_segment(sand_cave, max_y + 2L, c(500L - max_y - 5L, 500L + max_y + 5L))
      final_state <- drop_sand(sand_cave, c(500L, 0L))
      as.character(final_state$sand_dropped)
    }
  }
)


#' Parsed Input to Day14 of Advent Of Code 2022
#'
#' @param input_string Input string to day 13 of AdventOfCode
#'
#' @return A cave data structure containing the rock formations
#' @importFrom magrittr %>%
parse_day14_input <- function(input_string) {
  day14$parse_input(input_string)
}

#' Solution to Day14 Part1 of Advent of Code 2022
#'
#' @param sand_cave A cave data structure containing the rock formations
#'
#' @return The number of sand packets coming to rest before one falls to the ground
#' @importFrom magrittr %>%
solve_day14_part1 <- function(sand_cave) {
  day14$solve_part1(sand_cave)
}

#' Solution to Day14 Part2 of Advent of Code 2022
#'
#' @param sand_cave A cave data structure containing the rock formations
#'
#' @return The number of sand packets coming to rest before one seals the drop spot
#' @importFrom magrittr %>%
solve_day14_part2 <- function(sand_cave) {
  day14$solve_part2(sand_cave)
}
