
#' Module containing the implementation for Day22 of Advent Of Code 2022
#' @noRd
day22 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    is_in_map <- function(area_map, x, y) {
      !(area_map[[y, x]] %in% c(" ", ""))
    }
    
    is_wall <- function(area_map, x, y) {
      area_map[[y, x]] == "#"
    }
    
    parse_map <- function(text) {
      area_map <- parsing$to_character_matrix(text)
      walls_by_row <- vector(mode = "list", length = nrow(area_map))
      left_border <- vector(mode = "integer", length = nrow(area_map))
      right_border <- vector(mode = "integer", length = nrow(area_map))
      for (y in 1:nrow(area_map)) {
        walls <- c()
        in_map <- FALSE
        for (x in 1:ncol(area_map)) {
          if (!in_map && is_in_map(area_map, x, y)) {
            in_map <- TRUE
            left_border[[y]] <- x
          } else if (in_map && !is_in_map(area_map, x, y)) {
            in_map <- FALSE
            right_border[[y]] <- x - 1L
          }
          if (is_wall(area_map, x, y)) {
            walls <- c(walls, x)
          }
        }
        if (in_map) {
          right_border[[y]] <- ncol(area_map)
        }
        walls_by_row[[y]] <- if (length(walls) > 0) walls else NA_integer_
      }
      
      walls_by_column <- vector(mode = "list", length = ncol(area_map))
      top_border <- vector(mode = "integer", length = ncol(area_map))
      bottom_border <- vector(mode = "integer", length = ncol(area_map))
      for (x in 1:ncol(area_map)) {
        walls <- c()
        in_map <- FALSE
        for (y in 1:nrow(area_map)) {
          if (!in_map && is_in_map(area_map, x, y)) {
            in_map <- TRUE
            top_border[[x]] <- y
          } else if (in_map && !is_in_map(area_map, x, y)) {
            in_map <- FALSE
            bottom_border[[x]] <- y - 1L
          }
          if (is_wall(area_map, x, y)) {
            walls <- c(walls, y)
          }
        }
        if (in_map) {
          bottom_border[[x]] <- nrow(area_map)
        }
        walls_by_column[[x]] <- if (length(walls) > 0) walls else NA_integer_
      }
      
      list(
        borders = list(
          top = top_border,
          bottom = bottom_border,
          left = left_border,
          right = right_border
        ),
        walls = list(
          by_column = walls_by_column,
          by_row = walls_by_row
        )
      )
    }
    
    parse_moves <- function(text) {
      main_matches <- stringr::str_match_all(text, pattern = "(\\d+)([RL])")[[1]]
      last_match <- stringr::str_match(text, pattern = "[RL](\\d+)$")
      distances <- c(
        purrr::map_int(main_matches[,2], readr::parse_integer),
        readr::parse_integer(last_match[[2]])
      )
      rotations <- main_matches[,3]
      list(
        distances = distances,
        rotations = rotations
      )
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      parts <- parsing$split_line_separated_blocks(input_string)
      moves <- parse_moves(parts[[2]])
      area_map <- parse_map(parts[[1]])
      list(
        borders = area_map$borders,
        walls = area_map$walls,
        moves = moves
      )
    }
    
    find_next_wall <- function(position, walls) {
      if (length(walls) == 1L && is.na(walls)) {
        return(NA)
      }
      # binary search is probably faster for large numbers of walls, but for the small number at hand, this is good enough.
      for (wall in walls) {
        if (wall > position) {
          return(wall)
        }
      }
      NA
    }
    
    find_previous_wall <- function(position, walls) {
      if (length(walls) == 1L && is.na(walls)) {
        return(NA)
      }
      # binary search is probably faster for large numbers of walls, but for the small number at hand, this is good enough.
      for (wall in rev(walls)) {
        if (wall < position) {
          return(wall)
        }
      }
      NA
    }
    
    rotate_orientation <- function(orientation, rotation_direction) {
      if (rotation_direction == "L") {
        c(orientation[[2]], -orientation[[1]])
      } else if (rotation_direction == "R") {
        c(-orientation[[2]], orientation[[1]])
      } else if (rotation_direction == "T") {
        -orientation
      } else {
        cli::cli_abort(c(
          "The known rotation directions are 'L', 'R' and 'T'.",
          "x" = "Rotation direction {orientation} not recognized."
        ))
      }
    }
    
    facing_value <- function(orientation) {
      if (all(orientation == c(1L, 0L))) {
        0L
      } else if (all(orientation == c(0L, 1L))) {
        1L
      } else if (all(orientation == c(-1L, 0L))) {
        2L
      } else if (all(orientation == c(0L, -1L))) {
        3L
      } else {
        cli::cli_abort(c(
          "The known orientations are (1,0), (0,1), (-1,0) and (0, -1).",
          "x" = "Orientation {orientation} not recognized."
        ))
      }
    }
    
    encode_position <- function(point, orientation) {
      1000 * point[[2]] + 4 * point[[1]] + facing_value(orientation) 
    }
    
    perform_loop_transition <- function(position, orientation, borders) {
      x <- position[[1]]
      y <- position[[2]]
      if (orientation[[1]] > 0) {
        list(
          position = c(borders$left[[y]], y),
          orientation = orientation
        )
      } else if (orientation[[1]] < 0L) {
        list(
          position = c(borders$right[[y]], y),
          orientation = orientation
        )
      } else if (orientation[[2]] > 0L) {
        list(
          position = c(x, borders$top[[x]]),
          orientation = orientation
        )
      } else if (orientation[[2]] < 0L) {
        list(
          position = c(x, borders$bottom[[x]]),
          orientation = orientation
        )
      } else {
        cli::cli_abort(c(
          "The known orientations are (1,0), (0,1), (-1,0) and (0, -1).",
          "x" = "Orientation {orientation} not recognized."
        ))
      }
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(input) {
      start_point <- c(input$borders$left[[1]], 1L)
      start_orientation <- c(1L, 0L)
      transition_function <- purrr::partial(
        perform_loop_transition,
        borders = input$borders
      )
      final_position <- apply_moves(start_point, start_orientation, input$borders, input$walls, transition_function, input$moves)
      result <- encode_position(final_position$position, final_position$orientation)
      as.character(result)
    }
    
    
    move_in_coord_direction <- function(position, distance, min_border, max_border, walls) {
      next_wall <- find_next_wall(position, walls)
      if (!is.na(next_wall)) {
        # There is a wall before we have to wrap around.
        new_position <- min(position + distance, next_wall - 1L)
        return(list(
            position = new_position,
            remaining_distance = 0L
        ))
      }
      # There is no wall before wrapping around.
      
      if (position + distance <= max_border) {
        # We do not wrap around because we do not move that far.
        return(list(
          position = position + distance,
          remaining_distance = 0L
        ))
      }
      # We have to move so far that we wrap around.
      
      remaining_distance <- position + distance - max_border
      list(
        position = max_border,
        remaining_distance = remaining_distance
      )
    }
    
    move_against_coord_direction <- function(position, distance, min_border, max_border, walls) {
      previous_wall <- find_previous_wall(position, walls)
      if (!is.na(previous_wall)) {
        # There is a wall before we have to wrap around.
        new_position <- max(position - distance, previous_wall + 1L)
        return(list(
          position = new_position,
          remaining_distance = 0L
        ))
      }
      # There is no wall before wrapping around.
      
      if (position - distance >= min_border) {
        # We do not wrap around because we do not move that far.
        return(list(
          position = position - distance,
          remaining_distance = 0L
        ))
      }
      # We have to move so far, that we wrap around.
      
      remaining_distance <- min_border - position + distance
      list(
        position = min_border,
        remaining_distance = remaining_distance
      )
    }
    
    move_right <- function(position, distance, borders, walls) {
      y <- position[[2]]
      move_result <- move_in_coord_direction(position[[1]], distance, borders$left[[y]], borders$right[[y]], walls$by_row[[y]])
      list(
        position = c(move_result$position, y),
        remaining_distance = move_result$remaining_distance
      )
    }
    
    move_left <- function(position, distance, borders, walls) {
      y <- position[[2]]
      move_result <- move_against_coord_direction(position[[1]], distance, borders$left[[y]], borders$right[[y]], walls$by_row[[y]])
      list(
        position = c(move_result$position, y),
        remaining_distance = move_result$remaining_distance
      )
    }
    
    move_up <- function(position, distance, borders, walls) {
      x <- position[[1]]
      move_result <- move_against_coord_direction(position[[2]], distance, borders$top[[x]], borders$bottom[[x]], walls$by_column[[x]])
      list(
        position = c(x, move_result$position),
        remaining_distance = move_result$remaining_distance
      )
    }
    
    move_down <- function(position, distance, borders, walls) {
      x <- position[[1]]
      move_result <- move_in_coord_direction(position[[2]], distance, borders$top[[x]], borders$bottom[[x]], walls$by_column[[x]])
      list(
        position = c(x, move_result$position),
        remaining_distance = move_result$remaining_distance
      )
    }
    
    right_cube_transition <- function(position, cube_config, width) {
      x <- position[[1]]
      y <- position[[2]]
      cube_x <- floor((x - 1L) / width) + 1L
      cube_y <- floor((y - 1L) / width) + 1L
      # browser()
      # right y + 1
      if (cube_x < ncol(cube_config) && 
          cube_y < nrow(cube_config) && 
          cube_config[[cube_y + 1, cube_x + 1]]) {
        new_position <- c(
          cube_x * width + cube_y * width - y + 1L,
          cube_y * width + 1L
        )
        new_orientation <- c(0L, 1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # right y - 1
      } else if (cube_x < ncol(cube_config) && 
                 cube_y > 1 && 
                 cube_config[[cube_y - 1, cube_x + 1]]) {
        new_position <- c(
          cube_x * width + y - (cube_y - 1L) * width,
          (cube_y - 1L) * width
        )
        new_orientation <- c(0L, -1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # right y + 2
      } else if (cube_x < ncol(cube_config) && 
                 cube_y + 1L < nrow(cube_config) && 
                 cube_config[[cube_y + 2, cube_x + 1]]) {
        new_position <- c(
          (cube_x + 1L) * width,
          (cube_y + 1L) * width + cube_y * width + 1L - y
        )
        new_orientation <- c(-1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # right y - 2
      } else if (cube_x < ncol(cube_config) && 
                 cube_y > 2 && 
                 cube_config[[cube_y - 2, cube_x + 1]]) {
        new_position <- c(
          (cube_x + 1L) * width,
          (cube_y - 3L) * width + (cube_y * width + 1L - y)
        )
        new_orientation <- c(-1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # right y + 3
      } else if (cube_x < ncol(cube_config) && 
                 cube_y + 3L < nrow(cube_config) && 
                 cube_config[[cube_y + 4, cube_x + 1]]) {
        new_position <- c(
          cube_x * width + y - (cube_y - 1L) * width,
          (cube_y + 3L) * width
        ) 
        new_orientation <- c(0L, -1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # right y - 3
      } else if (cube_x < ncol(cube_config) && 
                  cube_y > 3L && 
                  cube_config[[cube_y - 3L, cube_x + 1]]) {
        new_position <- c(
          cube_x * width + 1L + cube_y * width - y,
          (cube_y - 4L) * width + 1L
        )
        new_orientation <- c(0L, 1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # left straight
      } else if (cube_x > 3L &&
                 cube_config[[cube_y, cube_x - 3]]) {
        new_position <- c(1L +  (cube_x - 4) * width, y)
        new_orientation <- c(1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 1 down (dlll)
      } else if (cube_x > 3L && 
                 cube_y < nrow(cube_config) && 
                 cube_config[[cube_y + 1, cube_x - 3]] &&
                 cube_config[[cube_y + 1, cube_x]]) {
        new_position <- c(
          (cube_x - 4L) * width + 1L + cube_y * width - y,
          cube_y * width + 1L
        )
        new_orientation <- c(0L, -1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 6 down (lldl)
      } else if (cube_x > 3L && 
                 cube_y < nrow(cube_config) && 
                 cube_config[[cube_y + 1, cube_x - 3]] && 
                 !cube_config[[cube_y + 1, cube_x]] && 
                 !cube_config[[cube_y + 1, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 4L) * width + y - (cube_y - 1L) * width,
          (cube_y + 1L) * width
        )
        new_orientation <- c(0L, -1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 1 up (ulll)
      } else if (cube_x > 3L && 
                 cube_y > 1L && 
                 cube_config[[cube_y - 1, cube_x - 3]] && 
                 cube_config[[cube_y - 1, cube_x]]) {
        new_position <- c(
          (cube_x - 4L) * width + y - (cube_y - 1L) * width,
          (cube_y - 1L) * width
        )
        new_orientation <- c(0L, 1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # 6 up (llul)
      } else if (cube_x > 3L && 
                 cube_y > 1L && 
                 cube_config[[cube_y - 1, cube_x - 3]] && 
                 !cube_config[[cube_y - 1, cube_x]] && 
                 !cube_config[[cube_y - 1, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 4L) * width + 1L + cube_y * width - y,
          (cube_y - 2L) * width + 1L
        )
        new_orientation <- c(0L, 1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 2 down (dlldl) and shape 3 down (ldlld)
      } else if (cube_x > 3L && 
                 cube_y + 1 < nrow(cube_config) && 
                 cube_config[[cube_y + 2, cube_x - 3]]) {
        new_position <- c(
          (cube_x - 4L) * width + 1L,
          y + 2 * width
        )
        new_orientation <- c(1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 2 up (ullul) and shape 3 up (lullu)
      } else if (cube_x > 3L && 
                 cube_y > 2L && 
                 cube_config[[cube_y - 2, cube_x - 3]]) {
        new_position <- c(
          (cube_x - 4L) * width + 1L,
          y - 2 * width
        )
        new_orientation <- c(1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 7 down (ddldd)
      } else if (cube_x > 1L && 
                 cube_y + 3L < nrow(cube_config) && 
                 cube_config[[cube_y + 4, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 2L) * width + 1L,
          y + 4L * width
        )
        new_orientation <- c(1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 7 up (uuluu)
      } else if (cube_x > 1L && 
                 cube_y > 4L && 
                 cube_config[[cube_y - 4, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 2L) * width + 1L,
          y - 4L * width
        )
        new_orientation <- c(1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 5 down (dldd)
      } else if (cube_x > 1L && 
                 cube_y + 2L < nrow(cube_config) && 
                 cube_config[[cube_y + 3, cube_x - 1]] &&
                 !cube_config[[cube_y, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 2L) * width + y - (cube_y - 1L) * width,
          (cube_y + 3L) * width
        )
        new_orientation <- c(0L, -1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 5 up (uluu)
      } else if (cube_x > 1L && 
                 cube_y > 3L && 
                 cube_config[[cube_y - 3, cube_x - 1]] &&
                 !cube_config[[cube_y, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 2L) * width + cube_y * width + 1L - y,
          (cube_y - 4L) * width + 1L
        )
        new_orientation <- c(0L, 1L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 4 down (ldd)
      } else if (cube_x > 1L && 
                 cube_y + 1L < nrow(cube_config) && 
                 cube_config[[cube_y + 2, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 1L) * width,
          (cube_y + 1L) * width + cube_y * width + 1L - y
        )
        new_orientation <- c(-1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) # shape 4 down (luu)
      } else if (cube_x > 1L && 
                 cube_y > 2L && 
                 cube_config[[cube_y - 2, cube_x - 1]]) {
        new_position <- c(
          (cube_x - 1L) * width,
          (cube_y - 3L) * width + cube_y * width + 1L - y
        )
        new_orientation <- c(-1L, 0L)
        list(
          position = new_position,
          orientation = new_orientation
        ) 
      } else {
        stop("Not Implemented!")
      }
    }
    
    rotate_position <- function(position, rotation_direction, width) {
      if (rotation_direction == "L") {
        c(
          position[[2]], 
          5L * width - position[[1]] + 1L
        )
      } else if (rotation_direction == "R") {
        c(
          5L * width - position[[2]] + 1L, 
          position[[1]]
        )
      } else if (rotation_direction == "T") {
        c(
          5L * width - position[[1]] + 1L, 
          5L * width - position[[2]] + 1L
        )
      } else {
        cli::cli_abort(c(
          "The known rotation directions are 'L', 'R' and 'T'.",
          "x" = "Rotation direction {orientation} not recognized."
        ))
      }
    }
    
    rotate_cube_config <- function(cube_config, rotation_direction) {
      back_rotation_direction <- if (rotation_direction == "R") {
        "L"
      } else if (rotation_direction == "L") {
        "R"
      } else {
        rotation_direction
      }
      seq_along(cube_config) %>%
        purrr::map(
          function(index) {
            x <- floor((index - 1L) / 5L) + 1L
            y <- ((index - 1L) %% 5L) + 1L
            old_position <- rotate_position(
              position = c(x, y), 
              rotation_direction = back_rotation_direction,
              width = 1L
            )
            cube_config[[old_position[[2]], old_position[[1]]]]
          }
        ) %>%  
        matrix(ncol = 5L)
    }
    
    perform_cube_transition <- function(position, orientation, cube_config, width) {
      if (orientation[[1]] > 0) {
        right_cube_transition(position, cube_config, width)
      } else if (orientation[[1]] < 0L) {
        rotated_position <- rotate_position(position, "T", width)
        rotated_config <- rotate_cube_config(cube_config, "T")
        transition_result <- right_cube_transition(rotated_position, rotated_config, width)
        new_position <- rotate_position(transition_result$position, "T", width)
        new_orientation <- rotate_orientation(transition_result$orientation, "T")
        list(
          position = new_position,
          orientation = new_orientation
        )
      } else if (orientation[[2]] > 0L) {
        rotated_position <- rotate_position(position, "L", width)
        rotated_config <- rotate_cube_config(cube_config, "L")
        transition_result <- right_cube_transition(rotated_position, rotated_config, width)
        new_position <- rotate_position(transition_result$position, "R", width)
        new_orientation <- rotate_orientation(transition_result$orientation, "R")
        list(
          position = new_position,
          orientation = new_orientation
        )
      } else if (orientation[[2]] < 0L) {
        rotated_position <- rotate_position(position, "R", width)
        rotated_config <- rotate_cube_config(cube_config, "R")
        transition_result <- right_cube_transition(rotated_position, rotated_config, width)
        new_position <- rotate_position(transition_result$position, "L", width)
        new_orientation <- rotate_orientation(transition_result$orientation, "L")
        list(
          position = new_position,
          orientation = new_orientation
        )
      } else {
        cli::cli_abort(c(
          "The known orientations are (1,0), (0,1), (-1,0) and (0, -1).",
          "x" = "Orientation {orientation} not recognized."
        ))
      }
    }
    
    move_forward <- function(position, orientation, distance, borders, walls, transition_function) {
      move_result <- if (orientation[[1]] > 0L) {
        move_right(position, distance, borders, walls)
      } else if (orientation[[1]] < 0L) {
        move_left(position, distance, borders, walls)
      } else if (orientation[[2]] > 0L) {
        move_down(position, distance, borders, walls)
      } else if (orientation[[2]] < 0L) {
        move_up(position, distance, borders, walls)
      } else {
        cli::cli_abort(c(
          "The known orientations are (1,0), (0,1), (-1,0) and (0, -1).",
          "x" = "Orientation {orientation} not recognized."
        ))
      } 
      if (move_result$remaining_distance == 0L) {
        return(list(
          position = move_result$position,
          orientation = orientation
        ))
      }
      transition_result <- transition_function(move_result$position, orientation)
      remaining_distance <- move_result$remaining_distance - 1L
      
      if (transition_result$position[[2]] %in% walls$by_column[[transition_result$position[[1]]]]) {
        list(
          position = move_result$position,
          orientation = orientation
        )
      } else {
        move_forward(
          position = transition_result$position,
          orientation = transition_result$orientation,
          distance = remaining_distance,
          borders = borders,
          walls = walls,
          transition_function = transition_function
        )
      }
    }
    
    apply_moves <- function(start_point, start_orientation, borders, walls, transition_function, moves) {
      orientation <- start_orientation
      position <- start_point
      for (move_index in seq_along(moves$distances)) {
        move_result <- move_forward(position, orientation, moves$distances[[move_index]], borders, walls, transition_function)
        position <- move_result$position
        orientation <- move_result$orientation
        if (move_index <= length(moves$rotations)) {
          orientation <- rotate_orientation(orientation, moves$rotations[[move_index]])
        }
      }
      list(
        position = position,
        orientation = orientation
      )
    }
    
    cube_width <- function(map_width, map_height) {
      candidate1_1 <- max(c(map_width, map_height)) / 4L
      candidate1_2 <- min(c(map_width, map_height)) / 3L
      if (candidate1_1 == candidate1_2) {
        candidate1_1
      } else {
        max(c(map_width, map_height)) / 5L
      }
    }
    
    cube_configuration <- function(borders, width) {
      purrr::map_lgl(
        .x = 1:25L,
        .f = function(index) {
          x <- (floor((index - 1L) / 5L) + 1L) * width
          y <- (((index - 1L) %% 5L) + 1L) * width
          
          x <= length(borders$top) &&
            y <= length(borders$left) &&
            x >= borders$left[[y]] && 
            x <= borders$right[[y]] &&
            y >= borders$top[[x]] &&
            y <= borders$bottom[[x]]
        }
      ) %>% 
        matrix(ncol = 5L)
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(input) {
      start_point <- c(input$borders$left[[1]], 1L)
      start_orientation <- c(1L, 0L)
      width <- cube_width(
        map_width = length(input$borders$top),
        map_height = length(input$borders$left)
      )
      cube_config <- cube_configuration(input$borders, width)
      transition_function <- purrr::partial(
        perform_cube_transition,
        cube_config = cube_config,
        width = width
      ) 
      final_position <- apply_moves(start_point, start_orientation, input$borders, input$walls, transition_function, input$moves)
      result <- encode_position(final_position$position, final_position$orientation)
      as.character(result)
    }
  }
)


#' Parsed Input to Day22 of Advent Of Code 2022
#'
#' @param input_string Input string to day 22 of AdventOfCode
#'
#' @return A list containing the borders of the map, the wall positions, by row and by columns and move instructions 
#' @importFrom magrittr %>%
parse_day22_input <- function(input_string) {
  day22$parse_input(input_string)
}

#' Solution to Day22 Part1 of Advent of Code 2022
#'
#' @param input A list containing the borders of the map, the wall positions, by row and by columns and move instructions
#'
#' @return The final position and orientation after following the moves from the top-left encoded as an integer
#' @importFrom magrittr %>%
solve_day22_part1 <- function(input) {
  day22$solve_part1(input)
}

#' Solution to Day22 Part2 of Advent of Code 2022
#'
#' @param input A list containing the borders of the map, the wall positions, by row and by columns and move instructions
#'
#' @return The final position and orientation after following the moves from the top-left wrapping around the cube encoded as an integer
#' @importFrom magrittr %>%
solve_day22_part2 <- function(input) {
  day22$solve_part2(input)
}
