



#' Parsed Input to Day1 of Advent Of Code 2022
#'
#' @param input_string Input string to day 1 of AdventOfCode
#'
#' @return A list of one integer vector per backpack containing the individual calories per item
parse_day1_input <- function(input_string) {
  backpack_texts <- split_line_separated_blocks(input_string)
  backpacks <- purrr::map(
    .x = backpack_texts,
    .f = purrr::compose(
      readr::parse_integer,
      split_lines
    )
  )
  backpacks
}



#' Solution to Day1 Part1 of Advent of Code 2022
#'
#' @param backpacks A list of one integer vector per backpack containing the individual calories per item
#'
#' @return The largest total calories in a backpack
#' @importFrom magrittr %>% 
solve_day1_part1 <- function(backpacks) {
  result <- backpacks %>%
    purrr::map(sum) %>%
    unlist() %>%
    max()
  as.character(result)
}



#' Solution to Day1 Part2 of Advent of Code 2022
#'
#' @param backpacks A list of one integer vector per backpack containing the individual calories per item
#'
#' @return The sum of the three largest totals of calories in a backpacks
#' @importFrom magrittr %>%
solve_day1_part2 <- function(backpacks) {
  sorted_backpack_calories <- backpacks %>%
    purrr::map(sum) %>%
    unlist() %>%
    sort(decreasing = TRUE)
  result <- sum(sorted_backpack_calories[1:3])
  as.character(result)
}
