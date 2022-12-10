

parse_instruction <- function(instruction_text) {
  command_type = stringr::str_sub(
    string = instruction_text,
    start = 1L,
    end  = 4L
  )
  if (command_type == "noop") {
    list(
      type = "noop",
      argument = NULL,
      cycles = 1L
    )
  } else if (command_type == "addx") {
    list(
      type = "addx",
      argument = instruction_text %>%
            stringr::str_sub(start = 6L) %>%
            readr::parse_integer(),
      cycles = 2L
    )
  } else {
    cli::cli_abort(c(
        "Known command types are 'noop' and 'addx'.",
        "x" = "Cannot parse command '{command_text}'"
       )
    )
  }
}


#' Parsed Input to Day10 of Advent Of Code 2022
#'
#' @param input_string Input string to day 10 of AdventOfCode
#'
#' @return A list of commands, each a list with arguments `type`, the command type, `argument`, the argument to the command, and `cyles`, the time it takes to execute the command
#' @importFrom magrittr %>%
parse_day10_input <- function(input_string) {
  input_string %>%
    split_lines() %>%
    purrr::map(parse_instruction)
}


total_runtime <- function(code) {
  code %>%
    purrr::map(function(command) command$cycles) %>%
    unlist() %>%
    sum()
}

execute_instruction <- function(state, instruction) {
  new_state <- state
  if (instruction$type == "addx") {
    new_state$x <- state$x + instruction$argument
    new_state$instruction_pointer = state$instruction_pointer + 1L
  } else if (instruction$type == "noop") {
    new_state$instruction_pointer = state$instruction_pointer + 1L
  } else {
    cli::cli_abort(c(
      "Known command types are 'noop' and 'addx'.",
      "x" = "Unknown instruction '{instruction}'"
    ))
  }
  new_state$cycle <- state$cycle + instruction$cycles
  new_state
}

# State of the X register during each cycle
x_register_history <- function(code) {
  total_length <- total_runtime(code) + 1 # We also record the final state.
  register_history <- vector(mode = "integer", length = total_length)
  current_state <- list(
    x = 1L,
    cycle = 0L,
    instruction_pointer = 1L
  )
  start_cycle <- current_state$cycle
  register_history[[1]] <- current_state$x
  while (current_state$instruction_pointer <= length(code)) {
    new_state <- execute_instruction(current_state, code[[current_state$instruction_pointer]])
    
    if (new_state$cycle > current_state$cycle + 1L) {
      register_history[(current_state$cycle + 2L - start_cycle):(new_state$cycle - start_cycle)] <- current_state$x
    }
    
    register_history[[new_state$cycle + 1L]] <- new_state$x
    current_state <- new_state
  }
  register_history
}


#' Solution to Day10 Part1 of Advent of Code 2022
#'
#' @param code A list of commands, each a list with arguments `type`, the command type, `argument`, the argument to the command, and `cyles`, the time it takes to execute the command
#'
#' @return The sum of the signal strength at each 20th cycle
#' @importFrom magrittr %>%
solve_day10_part1 <- function(code) {
  register_history <- x_register_history(code)
  relevant_cycle_times <- 1:floor((length(register_history) + 20L) / 40L) * 40L - 20L
  relevant_register_values <- register_history[relevant_cycle_times]
  relevant_signal_strengths <- relevant_cycle_times * relevant_register_values
  relevant_signal_strengths %>%
  sum() %>%
    as.character()
}

pixel_lit <- function(x_history, width, start_position = 0L) {
  # We stop drawing when the code terminates.
  # The result of the last instruction is not drawn.
  pixel_cycle_time <- 1:(length(x_history) - 1) # We stop drawing whan the code terminates 
  relevant_history <- x_history[pixel_cycle_time]
  pixel_position <- (pixel_cycle_time - 1 + start_position) %% width
  relevant_history >= pixel_position - 1 &
    relevant_history <= pixel_position + 1
}

pixel_values <- function(lighting_status) {
  dplyr::if_else(
    condition = lighting_status,
    true = "#",
    false = "."
  )
}

screen_display <- function(pixels, width) {
  split(pixels, ceiling(seq_along(pixels) / width)) %>%
    purrr::map(function(row) paste0(row, collapse = "")) %>%
    paste0(collapse = "\n")
}


#' Solution to Day10 Part2 of Advent of Code 2022
#'
#' @param code A list of commands, each a list with arguments `type`, the command type, `argument`, the argument to the command, and `cyles`, the time it takes to execute the command
#'
#' @return A string representing the display on the CRT monitor
#' @importFrom magrittr %>%
solve_day10_part2 <- function(code) {
  code %>%
    x_register_history() %>%
    pixel_lit(width = 40L) %>%
    pixel_values() %>%
    screen_display(width = 40L)
}
