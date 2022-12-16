
#' Module containing the implementation for Day16 of Advent Of Code 2022
#' @noRd
day16 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    parse_valve <- function(text) {
      matches <- stringr::str_match(text, pattern = "^Valve (.+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)")
      name <- matches[[2]]
      rate <- readr::parse_integer(matches[[3]])
      connections <- stringr::str_split(matches[[4]], ", ") %>% unlist()
      list(
        name = name,
        rate = rate,
        connections = connections
      )
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      valves <- input_string %>%
        parsing$split_lines() %>%
        purrr::map(parse_valve)
      names(valves) <- purrr::map_chr(
        .x = valves,
        .f = function(valve) {
          valve$name
        }
      )
      valves
    }
    
    encode_state <- function(state) {
      encoded_valves <- paste0(state$open_valves, collapse = "_")
      paste(state$position, encoded_valves, sep = "__")
    }
    
    already_visited <- function(state, visited_states) {
      encode_state(state) %in% visited_states
    }
    
    available_next_actions <- function(state, valves, visited_states) {
      current_valve <- valves[[state$position]]
      move_states <- current_valve$connections %>%
        purrr::map(
          function(position) {
            list(
              position = position,
              open_valves = state$open_valves
            )
          }
        )
      
      state_already_visited <- purrr::map_lgl(
        .x = move_states,
        .f = purrr::partial(
          already_visited,
          visited_states = visited_states
        )
      )
      
      relevant_state_changes <- move_states[!state_already_visited] %>% 
        purrr::map(
          function(state) {
            list(
              state = state,
              rate_increase = 0
            )
          }
        )
      
      if (current_valve$rate > 0 && !(current_valve$name %in% state$open_valves)) {
        open_valve_state <- list(
          position = state$position,
          open_valves = c(state$open_valves, state$position) %>%
            unique() %>%
            sort()
        )
        open_valve <- list(
          state = open_valve_state,
          rate_increase = current_valve$rate
        )
        relevant_state_changes <- list(
          list(open_valve),
          relevant_state_changes
        ) %>% unlist(recursive = FALSE)
      }
      relevant_state_changes
    }
    
    max_pressure_release_from_state <- function(
      state, 
      valves, 
      remaining_time, 
      visited_states, 
      released_pressure, 
      max_known_released_pressure, 
      remaining_rate
    ) {
      if (remaining_time <= 0) {
        return(max_known_released_pressure)
      }
      available_pressure <- remaining_time * remaining_rate
      if (available_pressure <= max_known_released_pressure - released_pressure) {
        # There is no more chance release more pressure then the current best.
        return(max_known_released_pressure)
      }
      visited_states <- c(visited_states, encode_state(state)) 
      new_remaining_time = remaining_time - 1L
      
      available_next_moves <- available_next_actions(state, valves, visited_states)
      for (action in available_next_moves) {
        new_released_pressure <- released_pressure + action$rate_increase * new_remaining_time
        if (new_released_pressure > max_known_released_pressure) {
          max_known_released_pressure <- new_released_pressure
        }
        max_known_released_pressure <- max_pressure_release_from_state(
          state = action$state, 
          valves = valves, 
          remaining_time = new_remaining_time, 
          visited_states = visited_states, 
          released_pressure = new_released_pressure, 
          max_known_released_pressure = max_known_released_pressure,
          remaining_rate = remaining_rate - action$rate_increase 
        )
      }
      max_known_released_pressure
    }
    
    highest_releaseable_pressure <- function(start_point, valves, time_available) {
      start_state <- list(
        position = start_point,
        open_valves = c()
      )
      visited_states <- c()
      total_rate <- valves %>%
        purrr::map_int(function(valve) valve$rate) %>%
        sum()
      max_pressure_release_from_state(
        state = start_state,
        valves = valves, 
        remaining_time = time_available, 
        visited_states = visited_states, 
        released_pressure = 0, 
        max_known_released_pressure = 0,
        remaining_rate = total_rate
      )
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(valves) {
      start_point <- "AA"
      time_available <- 30L
      max_pressure_release <- highest_releaseable_pressure(
        start_point,
        valves,
        time_available
      )
      as.character(max_pressure_release)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(valves) {
      stop("To Be Implemented")
    }
  }
)


#' Parsed Input to Day16 of Advent Of Code 2022
#'
#' @param input_string Input string to day 16 of AdventOfCode
#'
#' @return TO DO
#' @importFrom magrittr %>%
parse_day16_input <- function(input_string) {
  day16$parse_input(input_string)
}

#' Solution to Day16 Part1 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day16_part1 <- function(valves) {
  day16$solve_part1(valves)
}

#' Solution to Day16 Part2 of Advent of Code 2022
#'
#' @param input TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day16_part2 <- function(valves) {
  day16$solve_part2(valves)
}
