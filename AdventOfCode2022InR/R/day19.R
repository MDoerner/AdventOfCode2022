
#' Module containing the implementation for Day19 of Advent Of Code 2022
#' @noRd
day19 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    
    parse_blueprint <- function(text) {
      matches <- stringr::str_match(text, pattern = "^Blueprint (.+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.$")
      list(
        id = matches[[2]],
        ore = c(ore = readr::parse_integer(matches[[3]]), clay = 0L, obsidian = 0L),
        clay = c(ore = readr::parse_integer(matches[[4]]), clay = 0L, obsidian = 0L),
        obsidian = c(ore = readr::parse_integer(matches[[5]]), clay = readr::parse_integer(matches[[6]]), obsidian = 0L),
        geode = c(ore = readr::parse_integer(matches[[7]]), clay = 0L, obsidian = readr::parse_integer(matches[[8]]))
      )
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      blueprints <- input_string %>%
        parsing$split_lines() %>%
        purrr::map(parse_blueprint)
      names(blueprints) <- purrr::map_chr(blueprints, function(blueprint) blueprint$id)
      blueprints
    }
    
    
    limit_ore_reached <- function(ore_robots, max_ore_per_robot) {
      ore_robots >= max_ore_per_robot
    }
    
    possible_next_constructions <- function(current_robots, current_resources, remaining_time, blueprint, max_ore_cost, geods_harvested) {
      possible_constructions = list()
      
      time_to_ore_construction <- max(c(0L, ceiling((blueprint[["ore"]][["ore"]] - current_resources[["ore"]]) / current_robots[["ore"]])))
      if (remaining_time > time_to_ore_construction + 1L && !limit_ore_reached(current_robots[["ore"]], max_ore_cost)) {
        ore_construction <- list(
          remaining_time = remaining_time - time_to_ore_construction - 1L,
          geods_harvested = geods_harvested,
          robots = current_robots + c(ore = 1L, clay = 0L, obsidian = 0L),
          resources = (current_resources + (time_to_ore_construction + 1L) * current_robots - blueprint[["ore"]])
        )
        possible_constructions <- append(possible_constructions, list(ore_construction))
      }
      
      if (current_robots[["obsidian"]] > 0L) {
        time_to_geod_construction <- max(c(
          0L, 
          ceiling((blueprint[["geode"]][["obsidian"]] - current_resources[["obsidian"]]) / current_robots[["obsidian"]]), 
          ceiling((blueprint[["geode"]][["ore"]] - current_resources[["ore"]]) / current_robots[["ore"]])
        ))
        if (remaining_time > time_to_geod_construction + 1L) {
          geod_construction <- list(
            remaining_time = remaining_time - time_to_geod_construction - 1L,
            geods_harvested = geods_harvested + (remaining_time - time_to_geod_construction - 1L),
            robots = current_robots,
            resources = (current_resources + (time_to_geod_construction + 1L) * current_robots - blueprint[["geode"]])
          )
          possible_constructions <- append(possible_constructions, list(geod_construction))
        }
      }
      
      if (current_robots[["clay"]] > 0L) {
        time_to_obsidian_construction <- max(c(
          0L, 
          ceiling((blueprint[["obsidian"]][["clay"]] - current_resources[["clay"]]) / current_robots[["clay"]]), 
          ceiling((blueprint[["obsidian"]][["ore"]] - current_resources[["ore"]]) / current_robots[["ore"]])
        ))
        if (remaining_time > time_to_obsidian_construction + 1L) {
          obsidian_construction <- list(
            remaining_time = remaining_time - time_to_obsidian_construction - 1L,
            geods_harvested = geods_harvested,
            robots = current_robots + c(ore = 0L, clay = 0L, obsidian = 1L),
            resources = (current_resources + (time_to_obsidian_construction + 1L) * current_robots - blueprint[["obsidian"]])
          )
          possible_constructions <- append(possible_constructions, list(obsidian_construction))
        }
      }
      
      time_to_clay_construction <- max(c(0L, ceiling((blueprint[["clay"]][["ore"]] - current_resources[["ore"]]) / current_robots[["ore"]])))
      if (remaining_time > time_to_clay_construction + 1L) {
        clay_construction <- list(
          remaining_time = remaining_time - time_to_clay_construction - 1L,
          geods_harvested = geods_harvested,
          robots = current_robots + c(ore = 0L, clay = 1L, obsidian = 0L),
          resources = (current_resources + (time_to_clay_construction + 1L) * current_robots - blueprint[["clay"]])
        )
        possible_constructions <- append(possible_constructions, list(clay_construction))
      }
      
      possible_constructions
    }
    
    most_geods_possible <- function(current_robots, current_resources, remaining_time, blueprint, max_ore_cost, geods_harvested, best_known_geods_harvested) {
      if (remaining_time < 0L) {
        return(best_known_geods_harvested)
      }
      available_geods <- (remaining_time - 1L) * remaining_time / 2L
      if (best_known_geods_harvested >= available_geods + geods_harvested) {
        return(best_known_geods_harvested)
      }
      
      available_constructions <- possible_next_constructions(current_robots, current_resources, remaining_time, blueprint, max_ore_cost, geods_harvested)
      for (construction in available_constructions) {
        best_known_geods_harvested <- max(c(best_known_geods_harvested, construction$geods_harvested))
        geods_possible <- most_geods_possible(
          current_robots = construction$robots, 
          current_resources = construction$resources, 
          remaining_time = construction$remaining_time, 
          blueprint = blueprint, 
          max_ore_cost = max_ore_cost, 
          geods_harvested = construction$geods_harvested, 
          best_known_geods_harvested
        )
        best_known_geods_harvested <- max(c(best_known_geods_harvested, geods_possible))
      }
      
      best_known_geods_harvested
    }
    
    most_geods_harvestable <- function(blueprint, time_available) {
      start_robots <- c(ore = 1L, clay = 0L, obsidian = 0L)
      start_resources <- c(ore = 0L, clay = 0L, obsidian = 0L)
      start_geods_harvested <- 0L
      start_most_goeds <- 0L
      max_ore_cost <- max(c(blueprint[["clay"]][["ore"]], blueprint[["obsidian"]][["ore"]], blueprint[["geod"]][["ore"]]))
      most_geods_possible(
        current_robots = start_robots, 
        current_resources = start_resources, 
        remaining_time = time_available, 
        blueprint = blueprint, 
        max_ore_cost = max_ore_cost, 
        geods_harvested = start_geods_harvested, 
        best_known_geods_harvested = start_most_goeds
      )
    }
    
    
    modules::export("solve_part1")
    solve_part1 <- function(blueprints) {
      best_yields <- blueprints %>%
        purrr::map_dbl(
          purrr::partial(
            most_geods_harvestable,
            time_available = 24L
          )
        )
      indices <- seq_along(best_yields)
      result <- sum(indices * best_yields)
      as.character(result)
    }
    
    
    modules::export("solve_part2")
    solve_part2 <- function(blueprints) {
      available_blueprint_count <- min(length(blueprints), 3L)
      available_blueprints <- blueprints[1:available_blueprint_count]
      available_blueprints %>%
        purrr::map_dbl(
          purrr::partial(
            most_geods_harvestable,
            time_available = 32L
          )
        ) %>%
        prod() %>%
        as.character()
    }
  }
)


#' Parsed Input to Day19 of Advent Of Code 2022
#'
#' @param input_string Input string to day 19 of AdventOfCode
#'
#' @return A list of blueprints, each with an id and vectors of the required materials for each robot
#' @importFrom magrittr %>%
parse_day19_input <- function(input_string) {
  day19$parse_input(input_string)
}

#' Solution to Day19 Part1 of Advent of Code 2022
#'
#' @param blueprints A list of blueprints, each with an id and vectors of the required materials for each robot
#'
#' @return The sum product of the blueprint ids and the most geods harvestable within 24 minutes
#' @importFrom magrittr %>%
solve_day19_part1 <- function(blueprints) {
  day19$solve_part1(blueprints)
}

#' Solution to Day19 Part2 of Advent of Code 2022
#'
#' @param blueprints A list of blueprints, each with an id and vectors of the required materials for each robot
#'
#' @return The product of the most geods harvestable within 32 minutes for the first 3 blueprints
#' @importFrom magrittr %>%
solve_day19_part2 <- function(blueprints) {
  day19$solve_part2(blueprints)
}
