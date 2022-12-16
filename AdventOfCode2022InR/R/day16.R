
#' Module containing the implementation for Day16 of Advent Of Code 2022
#' @noRd
day16 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    dijkstra <- modules::use("R/algorithms/dijkstra.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    point_encoder <- modules::use("R/utility/point_utils.R")
    
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
    
    available_next_actions <- function(state, valves, distances) {
      current_distances <- distances[[state$position]]
      relevant_valves <- names(current_distances) %>%
        setdiff(state$open_valves)
      relevant_valves %>%
        purrr::map(
          function(valve) {
            list(
              state = list(
                position = valve,
                open_valves = union(state$open_valves, valve)
              ),
              distance = current_distances[[valve]],
              rate_increase = valves[[valve]]$rate
            )
          }
        )
    }
    
    max_pressure_release_from_state <- function(
      state,
      valves,
      remaining_time,
      released_pressure,
      max_known_released_pressure,
      remaining_rate,
      distances
    ) {
      if (remaining_time <= 0) {
        return(max_known_released_pressure)
      }
      available_pressure <- remaining_time * remaining_rate
      if (available_pressure <= max_known_released_pressure - released_pressure) {
        # There is no more chance to release more pressure then the current best.
        return(max_known_released_pressure)
      }

      available_next_moves <- available_next_actions(state, valves, distances)
      for (action in available_next_moves) {
        new_remaining_time <- remaining_time - action$distance - 1L
        if (new_remaining_time > 0) {
          new_released_pressure <- released_pressure + action$rate_increase * new_remaining_time
          if (new_released_pressure > max_known_released_pressure) {
            max_known_released_pressure <- new_released_pressure
          }
          max_known_released_pressure <- max_pressure_release_from_state(
            state = action$state,
            valves = valves,
            remaining_time = new_remaining_time,
            released_pressure = new_released_pressure,
            max_known_released_pressure = max_known_released_pressure,
            remaining_rate = remaining_rate - action$rate_increase,
            distances = distances
          )
        }
      }
      max_known_released_pressure
    }
    
    highest_releaseable_pressure <- function(start_point, valves, time_available) {
      start_state <- list(
        position = start_point,
        open_valves = c()
      )
      total_rate <- valves %>%
        purrr::map_int(function(valve) valve$rate) %>%
        sum()
      distances <- distances_between_relevant_valves(valves, start_point)
      max_pressure_release_from_state(
        state = start_state,
        valves = valves,
        remaining_time = time_available,
        released_pressure = 0,
        max_known_released_pressure = 0,
        remaining_rate = total_rate,
        distances = distances
      )
    }
    
    distances_between_relevant_valves <- function(valves, start_valve) {
      is_relevant <- valves %>%
        purrr::map_lgl(
          function(valve) {
            valve$rate > 0 || valve$name == start_valve
          }
        )
      relevant_valves <- valves[is_relevant]
      
      connections_of <- function(valve_name) {
        valve <- valves[[valve_name]]
        purrr::map(
          .x = valve$connections,
          .f = function(connection) {
            list(
              destination = connection,
              distance = 1L
            )
          }
        )
      }
      
      find_distances <- purrr::partial(
        dijkstra$shortest_path_distances,
        connections_of = connections_of
      )
      
      all_distances <- purrr::map(
        .x = names(relevant_valves),
        .f = find_distances
      )
      
      relevant_distances <- all_distances %>%
        purrr::map(
          function(distances) {
            is_relevant_distance <- distances %>%
              names() %>%
              purrr::map_lgl(
                function(valve_name) {
                  valves[[valve_name]]$rate > 0
                }
              )
            distances[is_relevant_distance]
          }
        )
      names(relevant_distances) <- names(relevant_valves)
      
      relevant_distances
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
    
    
    
    highest_releaseable_pressure_duo <- function(start_point, valves, time_available) {
      start_state <- list(
        positions = rep(start_point, times = 2L),
        arrival_times = rep(time_available, times = 2L),
        open_valves = c()
      )
      total_rate <- valves %>%
        purrr::map_int(function(valve) valve$rate) %>%
        sum()
      distances <- distances_between_relevant_valves(valves, start_point)
      max_pressure_release_from_state_duo(
        state = start_state,
        valves = valves,  
        released_pressure = 0, 
        max_known_released_pressure = 0,
        remaining_rate = total_rate,
        distances = distances
      )
    }
    
    max_pressure_release_from_state_duo <- function(
      state,
      valves,
      released_pressure,
      max_known_released_pressure,
      remaining_rate,
      distances
    ) {
      next_action_time_remaining <- max(state$arrival_times)
      if (next_action_time_remaining <= 0) {
        return(max_known_released_pressure)
      }
      available_pressure <- next_action_time_remaining * remaining_rate
      if (available_pressure <= max_known_released_pressure - released_pressure) {
        # There is no more chance to release more pressure then the current best.
        return(max_known_released_pressure)
      }
      
      action_index <- match(next_action_time_remaining, state$arrival_times)
      action_valve_name <- state$positions[[action_index]]
      action_valve <- valves[[action_valve_name]]
      
      # If we arrived at a closed valve, we open it. Otherwise, there was no need to go here.
      if (!(action_valve_name %in% state$open_valves) && action_valve$rate > 0) {
        new_released_pressure <- released_pressure + action_valve$rate * (next_action_time_remaining - 1L)
        new_max_known_released_pressure <- max(max_known_released_pressure, new_released_pressure)
        new_arrival_times <- state$arrival_times
        new_arrival_times[[action_index]] <- next_action_time_remaining - 1L
        new_state <- list(
          positions = state$positions,
          arrival_times = new_arrival_times,
          open_valves = union(state$open_valves, action_valve_name)
        )
        new_max_known_released_pressure <- max_pressure_release_from_state_duo(
          state = new_state,
          valves = valves,
          released_pressure = new_released_pressure,
          max_known_released_pressure = new_max_known_released_pressure,
          remaining_rate = remaining_rate - action_valve$rate,
          distances = distances
        )
        return(new_max_known_released_pressure)
      }
      
      old_style_state <- list(
        position = action_valve_name,
        open_valves = state$open_valves
      )
      available_next_moves <- available_next_actions(old_style_state, valves, distances)
      for (action in available_next_moves) {
        new_arrival_times <- state$arrival_times
        new_arrival_times[[action_index]] <- next_action_time_remaining - action$distance
        new_positions <- state$positions
        new_positions[[action_index]] <- action$state$position
        new_state <- list(
          positions = new_positions,
          arrival_times = new_arrival_times,
          open_valves = state$open_valves
        )
        max_known_released_pressure <- max_pressure_release_from_state_duo(
          state = new_state,
          valves = valves,
          released_pressure = released_pressure,
          max_known_released_pressure = max_known_released_pressure,
          remaining_rate = remaining_rate,
          distances = distances
        )
      }
      max_known_released_pressure
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(valves) {
      start_point <- "AA"
      time_available <- 26L
      max_pressure_release <- highest_releaseable_pressure_duo(
        start_point,
        valves,
        time_available
      )
      as.character(max_pressure_release)
    }
  }
)


#' Parsed Input to Day16 of Advent Of Code 2022
#'
#' @param input_string Input string to day 16 of AdventOfCode
#'
#' @return A list of valves, each with its name, rate and connections to other valves
#' @importFrom magrittr %>%
parse_day16_input <- function(input_string) {
  day16$parse_input(input_string)
}

#' Solution to Day16 Part1 of Advent of Code 2022
#'
#' @param valves A list of valves, each with its name, rate and connections to other valves
#'
#' @return The must pressure you can release in 30 minutes
#' @importFrom magrittr %>%
solve_day16_part1 <- function(valves) {
  day16$solve_part1(valves)
}

#' Solution to Day16 Part2 of Advent of Code 2022
#'
#' @param valves A list of valves, each with its name, rate and connections to other valves
#'
#' @return The must pressure you can release in 26 minutes helped by an equally competent elephant
#' @importFrom magrittr %>%
solve_day16_part2 <- function(valves) {
  day16$solve_part2(valves)
}
