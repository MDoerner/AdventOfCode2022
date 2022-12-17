
#' Module containing the implementation for Day17 of Advent Of Code 2022
#' @noRd
day17 <- modules::module({
  parsing <- modules::use("R/utility/parsing_utils.R")
  hashmap <- modules::use("R/data_structures/hashmap.R")
  point_encoder <- modules::use("R/utility/point_utils.R")

  modules::export("parse_input")
  parse_input <- function(input_string) {
    input_string %>%
      parsing$split_characters() %>%
      purrr::map_lgl(
        function(c) {
          if (c == ">") {
            TRUE
          } else if (c == "<") {
            FALSE
          } else {
            cli::cli_abort(c(
              "The valid input characters are '<' and '>'.",
              "x" = "Encountered unexpected character '{c}'."
            ))
          }
        }
      )
  }

  
  shape_height <- function(shape_id = shape_index) {
    if (shape_id == 0L) {
      1L
    } else if (shape_id == 1L) {
      3L
    } else if (shape_id == 2L) {
      3L
    } else if (shape_id == 3L) {
      4L
    } else if (shape_id == 4L) {
      2L
    } else {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
  }
  
  shape_width <- function(shape_id = shape_index) {
    if (shape_id == 0L) {
      4L
    } else if (shape_id == 1L) {
      3L
    } else if (shape_id == 2L) {
      3L
    } else if (shape_id == 3L) {
      1L
    } else if (shape_id == 4L) {
      2L
    } else {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
  }

  shapes <- list(
    list(
      c(0L, 0L), c(1L, 0L), c(2L, 0L), c(3L, 0L)
    ),
    list(
      c(1L, 0L), c(0L, 1L), c(1L, 1L), c(2L, 1L), c(1L, 2L)
    ),
    list(
      c(0L, 0L), c(1L, 0L), c(2L, 0L), c(2L, 1L), c(2L, 2L)
    ),
    list(
      c(0L, 0L), c(0L, 1L), c(0L, 2L), c(0L, 3L)
    ),
    list(
      c(0L, 0L), c(1L, 0L), c(0L, 1L), c(1L, 1L)
    )
  )
  
  right_of_shapes <- list(
    list(
      c(4L, 0L)
    ),
    list(
      c(2L, 0L), c(3L, 1L), c(2L, 2L)
    ),
    list(
      c(3L, 0L), c(3L, 1L), c(3L, 2L)
    ),
    list(
      c(1L, 0L), c(1L, 1L), c(1L, 2L), c(1L, 3L)
    ),
    list(
      c(2L, 0L), c(2L, 1L)
    )
  )
  
  left_of_shapes <- list(
    list(
      c(-1L, 0L)
    ),
    list(
      c(0L, 0L), c(-1L, 1L), c(0L, 2L)
    ),
    list(
      c(-1L, 0L), c(1L, 1L), c(1L, 2L)
    ),
    list(
      c(-1L, 0L), c(-1L, 1L), c(-1L, 2L), c(-1L, 3L)
    ),
    list(
      c(-1L, 0L), c(-1L, 1L)
    )
  )
  
  below_shapes <- list(
    list(
      c(0L, -1L), c(1L, -1L), c(2L, -1L), c(3L, -1L)
    ),
    list(
      c(0L, 0L), c(1L, -1L), c(2L, 0L)
    ),
    list(
      c(0L, -1L), c(1L, -1L), c(2L, -1L)
    ),
    list(
      c(0L, -1L)
    ),
    list(
      c(0L, -1L), c(1L, -1L)
    )
  )
  
  get_shape <- function(shape_id) {
    if (shape_id < 0L || shape_id > 4L) {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
    shapes[[shape_id + 1L]]
  }
  
  get_right_of_shape <- function(shape_id) {
    if (shape_id < 0L || shape_id > 4L) {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
    right_of_shapes[[shape_id + 1L]]
  }
  
  get_left_of_shape <- function(shape_id) {
    if (shape_id < 0L || shape_id > 4L) {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
    left_of_shapes[[shape_id + 1L]]
  }
  
  get_below_shape <- function(shape_id) {
    if (shape_id < 0L || shape_id > 4L) {
      cli::cli_abort(c(
        "Shape ids are integers between 0 and 4.",
        "x" = "Unknown shape id {shape_id}."
      ))
    }
    below_shapes[[shape_id + 1L]]
  }
  
  is_occupied <- function(occupied_spaces, point) {
    hashmap$has(occupied_spaces, point_encoder$encode_point(point))
  }
  
  mark_as_occupied <- function(occupied_spaces, point) {
    hashmap$set(occupied_spaces, point_encoder$encode_point(point), TRUE)
  }
  
  mark_final_resting_place <- function(
    reference_point = reference_point,
    shape_id = shape_index,
    occupied_spaces = occupied_spaces
  ) {
    shape <- get_shape(shape_id)
    for (offset in shape) {
      occupied_spaces <- mark_as_occupied(
        occupied_spaces, 
        reference_point + offset
      )
    }
    occupied_spaces
  }

  move_horizontally <- function(
    reference_point,
    move_right,
    shape_id,
    occupied_spaces
  ) {
    if ((!move_right && reference_point[[1]] <= 1L) ||
        (move_right && reference_point[[1]] + shape_width(shape_id) > 7L)) {
      return(reference_point)
    }
    move_to_offsets <- if (move_right) {
      get_right_of_shape(shape_id)
    } else {
      get_left_of_shape(shape_id)
    }
    is_blocked <- move_to_offsets %>%
      purrr::map_lgl(
        function(offset) {
          is_occupied(occupied_spaces, reference_point + offset)
        }
      ) %>%
      any()
    if (is_blocked) {
      reference_point
    } else {
      reference_point + if (move_right)  c(1L, 0L) else c(-1L, 0L)
    }
  }
  
  try_move_down <- function(
    reference_point,
    shape_id,
    occupied_spaces
  ){
    if (reference_point[[2]] == 1L) {
      return(
        list(
          blocked = TRUE,
          reference_point = reference_point
        )
      )
    }
    
    move_to_offsets <- get_below_shape(shape_id)
    is_blocked <- move_to_offsets %>%
      purrr::map_lgl(
        function(offset) {
          is_occupied(occupied_spaces, reference_point + offset)
        }
      ) %>%
      any()
    if (is_blocked) {
      list(
        blocked = TRUE,
        reference_point = reference_point
      )
    } else {
      list(
        blocked = FALSE,
        reference_point = reference_point + c(0L, -1L)
      )
    }
  }

  drop_shapes <- function(n, jet_pattern) {
    occupied_spaces <- hashmap$empty_hashmap()
    jet_index <- 0L
    shape_count <- 0L
    shape_index <- 0L
    start_height <- 4L
    while (shape_count < n) {
      reference_point <- c(3L, start_height)
      blocked <- FALSE
      while (!blocked) {
        reference_point <- move_horizontally(
          reference_point = reference_point,
          move_right = jet_pattern[[jet_index + 1L]],
          shape_id = shape_index,
          occupied_spaces = occupied_spaces
        )
        jet_index <- (jet_index + 1L) %% length(jet_pattern)

        down_result <- try_move_down(
          reference_point = reference_point,
          shape_id = shape_index,
          occupied_spaces = occupied_spaces
        )
        reference_point <- down_result$reference_point
        blocked <- down_result$blocked
      }
      occupied_spaces <- mark_final_resting_place(
        reference_point = reference_point,
        shape_id = shape_index,
        occupied_spaces = occupied_spaces
      )
      start_height <- max(
        start_height,
        reference_point[[2]] + shape_height(shape_index) + 3L
      )
      shape_count <- shape_count + 1L
      shape_index <- shape_count %% 5L
    }

    start_height - 4L
  }

  draw_shaft <- function(occupied_spaces) {
    x_coords <- 1:7
    occupied_points <- occupied_spaces %>%
      hashmap$as_list() %>%
      names() %>%
      purrr::map(point_encoder$decode_point)
    y_coords_used <- purrr::map_dbl(
      .x = occupied_points,
      .f = function(point) {
        point[[2]]
      }
    )
    y_coords <- rev(1:(max(y_coords_used) + 3L))
    
    lines <- y_coords %>%
      purrr::map(
        function(y) {
          chars <- x_coords %>%
            purrr::map(
              function(x) {
                if (is_occupied(occupied_spaces, c(x, y))) {
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
  solve_part1 <- function(jet_pattern) {
    height_after <- drop_shapes(2022L, jet_pattern)
    as.character(height_after)
  }
  
  drop_round <- function(start_height, start_jet_index, jet_pattern, occupied_spaces){
    drop_height <- start_height + 4L
    jet_index <- start_jet_index
    for (shape_index in 0:4) {
      reference_point <- c(3L, drop_height)
      blocked <- FALSE
      while (!blocked) {
        reference_point <- move_horizontally(
          reference_point = reference_point,
          move_right = jet_pattern[[jet_index + 1L]],
          shape_id = shape_index,
          occupied_spaces = occupied_spaces
        )
        jet_index <- (jet_index + 1L) %% length(jet_pattern)
        
        down_result <- try_move_down(
          reference_point = reference_point,
          shape_id = shape_index,
          occupied_spaces = occupied_spaces
        )
        reference_point <- down_result$reference_point
        blocked <- down_result$blocked
      }
      occupied_spaces <- mark_final_resting_place(
        reference_point = reference_point,
        shape_id = shape_index,
        occupied_spaces = occupied_spaces
      )
      drop_height <- max(
        drop_height,
        reference_point[[2]] + shape_height(shape_index) + 3L
      )
    }
    list(
      jet_index = jet_index,
      occupied_spaces = occupied_spaces,
      height = drop_height - 4L
    )
  }

  encode_configuration <- function(jet_index, outline) {
    encoded_outline <- outline %>%
      purrr::map_chr(point_encoder$encode_point) %>%
      paste0(collapse = "_")
    paste(jet_index, encoded_outline, sep = "_")
  }
  
  save_configuration_index <- function(visited_configurations, jet_index, outline, shapes_dropped) {
    hashmap$set(visited_configurations, encode_configuration(jet_index, outline), shapes_dropped)
  }
  
  get_first_seen_index <- function(visited_configurations, jet_index, outline) {
    hashmap$get(visited_configurations, encode_configuration(jet_index, outline))
  }
  
  has_been_encountered <- function(visited_configurations, jet_index, outline) {
    hashmap$has(visited_configurations, encode_configuration(jet_index, outline))
  }
  
  save_height <- function(height_by_drops, shapes_dropped, height) {
    hashmap$set(height_by_drops, as.character(shapes_dropped), height)
  }
  
  get_height <- function(height_by_drops, shapes_dropped) {
    hashmap$get(height_by_drops, as.character(shapes_dropped))
  }
  
  outline_search_order <- list(
    c(1L, 1L), c(1L, 0L), c(1L, -1L), c(0L, -1L), c(-1L, -1L), c(-1L, 0L), c(-1L, 1L), c(0L, 1L)
  )
  
  get_outline <- function(occupied_spaces, height) {
    base_height <- height
    while (!is_occupied(occupied_spaces, c(1L , base_height))) {
      base_height <- base_height - 1
    }
    
    outline <- list(c(0L, 0L))
    current_point <- c(0L, 0L)
    previous_direction_index <- 2L 
    while (current_point[[1]] < 6L) {
      candidate_direction_index <- (previous_direction_index + 6L) %% 8L
      candidate <- current_point + outline_search_order[[candidate_direction_index + 1L]]
      while (!is_occupied(occupied_spaces, candidate + c(1L, base_height))) {
        candidate_direction_index <- (candidate_direction_index + 1L) %% 8L
        candidate <- current_point + outline_search_order[[candidate_direction_index + 1L]]
      }
      previous_direction_index <- candidate_direction_index
      current_point <- candidate
      outline <- append(outline, list(current_point))
    }
    outline
  }
  
  # n must be a multiple of 5
  drop_shapes_advanced <- function(n, jet_pattern) {
    occupied_spaces <- hashmap$empty_hashmap()
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(1L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(2L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(3L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(4L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(5L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(6L, 0L))
    occupied_spaces <- mark_as_occupied(occupied_spaces, c(7L, 0L))
    jet_index <- 0L
    shape_count <- 0
    height <- 0L
    outline <- list(c(0L, 0L), c(1L,0L), c(2L,0L), c(3L,0L), c(4L,0L), c(5L,0L), c(6L,0L))
    visited_configurations <- hashmap$empty_hashmap()
    height_by_drops <- hashmap$empty_hashmap()
    while (!has_been_encountered(visited_configurations, jet_index, outline) && shape_count < n) {
      visited_configurations <- save_configuration_index(visited_configurations, jet_index, outline, shape_count)
      height_by_drops <- save_height(height_by_drops, shape_count, height)
      
      drop_result <- drop_round(
        start_height = height, 
        start_jet_index = jet_index, 
        jet_pattern = jet_pattern, 
        occupied_spaces = occupied_spaces
      )
      
      jet_index <- drop_result$jet_index
      occupied_spaces <- drop_result$occupied_spaces
      height <- drop_result$height
      
      outline <- get_outline(occupied_spaces, height)
      
      shape_count <- shape_count + 5
    }
    
    if (shape_count >= n) {
      return(height)
    }
    
    first_seen_shape_count <- get_first_seen_index(visited_configurations, jet_index, outline)
    first_seen_height <- get_height(height_by_drops, first_seen_shape_count)
    cycle_height <- height - first_seen_height
    cycle_length <- shape_count - first_seen_shape_count
    cylces_to_perform <- floor((n - first_seen_shape_count) / cycle_length)
    extra_shapes <- (n - first_seen_shape_count) %% cycle_length
    extra_height <- get_height(height_by_drops, first_seen_shape_count + extra_shapes) - first_seen_height
    
    first_seen_height + cylces_to_perform * cycle_height + extra_height
  }
  
  draw_outline <- function(outline) {
    x_coords <- 0:6
    y_coords_used <- purrr::map_dbl(
      .x = outline,
      .f = function(point) {
        point[[2]]
      }
    )
    y_coords <- rev((min(y_coords_used) - 1L):(max(y_coords_used) + 1L))
    coords <- hashmap$empty_hashmap()
    for (point in outline) {
      coords <- hashmap$set(coords, point_encoder$encode_point(point), TRUE)
    }
    
    lines <- y_coords %>%
      purrr::map(
        function(y) {
          chars <- x_coords %>%
            purrr::map(
              function(x) {
                if (hashmap$has(coords, point_encoder$encode_point(c(x, y)))) {
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

  modules::export("solve_part2")
  solve_part2 <- function(jet_pattern) {
    height_after <- drop_shapes_advanced(1000000000000, jet_pattern)
    as.character(height_after)
  }
})


#' Parsed Input to Day17 of Advent Of Code 2022
#'
#' @param input_string Input string to day 17 of AdventOfCode
#'
#' @return A logical vector representing a jet pattern; TRUE means to the right and FALSE to the left
#' @importFrom magrittr %>%
parse_day17_input <- function(input_string) {
  day17$parse_input(input_string)
}

#' Solution to Day17 Part1 of Advent of Code 2022
#'
#' @param jet_pattern A logical vector representing a jet pattern; TRUE means to the right and FALSE to the left
#'
#' @return The height of the tower of the first 2022 fallen rocks
#' @importFrom magrittr %>%
solve_day17_part1 <- function(jet_pattern) {
  day17$solve_part1(jet_pattern)
}

#' Solution to Day17 Part2 of Advent of Code 2022
#'
#' @param jet_pattern A logical vector representing a jet pattern; TRUE means to the right and FALSE to the left
#'
#' @return The height of the tower of the first 1,000,000,000,000 fallen rocks
#' @importFrom magrittr %>%
solve_day17_part2 <- function(jet_pattern) {
  day17$solve_part2(jet_pattern)
}
