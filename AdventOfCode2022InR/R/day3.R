

parse_rucksack <- function(rucksack_text) {
  items <- stringr::str_split(rucksack_text, "") %>%
    unlist()
  if (length(items) > 1) {
    list(
      "left" = items[1:ceiling(length(items) / 2)],
      "right" = items[ceiling(length(items) / 2 + 1):length(items)]
    )
  } else if (length(items) == 0) {
    list("left" = c(), "right" = c())
  } else {
    list("left" = items, "right" = c())
  }
}



#' Parsed Input to Day3 of Advent Of Code 2022
#'
#' @param input_string Input string to day 3 of AdventOfCode
#'
#' @return A list with one item per rucksack. Each rucksack is represented by a list of two character vectors containing the stored items, one per compartment.
#' @importFrom magrittr %>%
parse_day3_input <- function(input_string) {
  input_string %>%
    split_lines() %>%
    purrr::map(parse_rucksack)
}



#' Priority of the Item Type
#'
#' @param item_type A vector of single characters
#'
#' @return The priority of the item types represented by the single characters
#' @noRd
item_type_priority <- function(item_type) {
  code_point <- purrr::map_int(item_type, utf8ToInt)
  dplyr::if_else(
    code_point >= 97, # 97 is the cod point of 'a'.
    true = code_point - 96,
    false = code_point - 64 + 26 # 65 is the code point of 'A'
  )
}


both_side_item_types <- function(rucksack) {
  intersect(
    rucksack[["left"]],
    rucksack[["right"]]
  )
}



#' Solution to Day3 Part1 of Advent of Code 2022
#'
#' @param rucksacks A list with one item per rucksack. Each rucksack is represented by a list of two character vectors containing the stored items, one per compartment.
#'
#' @return The total priority of the unique item types in each rucksack appearing in both compartments
#' @importFrom magrittr %>%
solve_day3_part1 <- function(rucksacks) {
  rucksacks %>%
    purrr::map(both_side_item_types) %>%
    unlist() %>%
    item_type_priority() %>%
    sum() %>%
    as.character()
}


split_in_groups <- function(rucksack_contents) {
  group_indicator <- floor((seq_along(rucksack_contents) - 1)/3)
  split(rucksack_contents, group_indicator)
}


contents <- function(rucksack) {
  c(rucksack[["left"]], rucksack[["right"]])
}


group_badge <- function(group_rucksack_contents) {
  purrr::reduce(group_rucksack_contents, intersect)
}


#' Solution to Day3 Part2 of Advent of Code 2022
#'
#' @param rucksacks TO DO
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day3_part2 <- function(rucksacks) {
  rucksacks %>%
    purrr::map(contents) %>%
    split_in_groups() %>%
    purrr::map(group_badge) %>%
    unlist() %>%
    item_type_priority() %>%
    sum() %>%
    as.character()
}
