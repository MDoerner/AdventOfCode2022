
## Stack implementation without safety checks

push <- function(stack, item) {
  c(stack, item)
}

peek <- function(stack, n = 1){
  stack[(length(stack) - n + 1):length(stack)]
}

pop <- function(stack, n = 1){
  if (length(stack) == n) {
    c()
  } else {
    stack[1:(length(stack) - n)]
  }
}


## Stack Parsing

add_stack_level <- function(stacks, stack_line) {
  purrr::map2(
    .x = stacks,
    .y = stack_line,
    .f = function(stack, item) {
      if (item == " ") {
        stack
      } else {
        push(stack, item)
      }
    }
  )
}

parse_stack_line <- function(line, first_item_offset, item_spacing) {
  characters <- stringr::str_split(line, "") %>% unlist()
  item_count <- floor((length(characters) - first_item_offset + 1) / item_spacing) + 1
  item_indices <- ((1:item_count) - 1) * item_spacing + first_item_offset
  characters[item_indices]
}

parsed_stacks <- function(stack_picture) {
  lines <- split_lines(stack_picture)
  name_line <- lines[[length(lines)]]
  stack_lines <- lines[1:(length(lines) - 1)]

  parse_line <- purrr::partial(
    parse_stack_line,
    first_item_offset = 2,
    item_spacing = 4
  )

  stack_names <- parse_line(name_line)
  stacks <- vector(mode = "list", length = length(stack_names))
  names(stacks) <- stack_names

  parsed_lines <- purrr::map(stack_lines, parse_line)
  
  purrr::reduce(
    .x = rev(parsed_lines),
    .f = add_stack_level,
    .init = stacks
  )
}


## Move Parsing

parse_move <- function(move_line) {
  matches <- stringr::str_match(move_line, pattern = "^move (\\d+) from (\\d+) to (\\d+)$")
  list(
    "count" = readr::parse_integer(matches[[2]]),
    "from" = matches[[3]],
    "to" = matches[[4]]
  )
}

parsed_move_list <- function(move_list_text){
  move_list_text %>%
    split_lines() %>%
    purrr::map(parse_move)
}

#' Parsed Input to Day5 of Advent Of Code 2022
#'
#' @param input_string Input string to day 5 of AdventOfCode
#'
#' @return A list containing an initial stack configuration and a list of moves.
#' @importFrom magrittr %>%
parse_day5_input <- function(input_string) {
  sections <- split_line_separated_blocks(input_string) 
  list(
    "initial_stacks" = parsed_stacks(sections[[1]]),
    "moves" = parsed_move_list(sections[[2]])
  )
}




move_one_item <- function(stacks, move){
  stacks[[move$to]] <- push(
      stacks[[move$to]], 
      peek(stacks[[move$from]])
    )
  stacks[[move$from]] <- pop(stacks[[move$from]])
  stacks
}

apply_move <- function(stacks, move) {
  move %>%
    list() %>% # This is necessary because of how rep works.
    rep(times = move$count) %>%
    purrr::reduce(
      .f = move_one_item,
      .init = stacks
    )
}

read_top <- function(stacks) {
  stacks %>%
    purrr::map(peek) %>%
    unlist() %>%
    paste0(collapse = "")
}

#' Solution to Day5 Part1 of Advent of Code 2022
#'
#' @param input A list containing an initial stack configuration and a list of moves.
#'
#' @return Top items after all moves are performed by the CrateMover 9000.
#' @importFrom magrittr %>%
solve_day5_part1 <- function(input) {
  inital_stacks <- input$initial_stacks
  processed_stacks <- purrr::reduce(
    .x = input$moves,
    .f = apply_move,
    .init = inital_stacks
  ) 
  # This is required because the list order changes whenever a stack gets empty.
  ordered_processed_stacks <- processed_stacks[names(inital_stacks)]  
  read_top(ordered_processed_stacks)
}


move_at_once_item <- function(stacks, move){
  stacks[[move$to]] <- push(
    stacks[[move$to]], 
    peek(stacks[[move$from]], move$count)
  )
  stacks[[move$from]] <- pop(stacks[[move$from]], move$count)
  stacks
}


#' Solution to Day5 Part2 of Advent of Code 2022
#'
#' @param input A list containing an initial stack configuration and a list of moves.
#'
#' @return Top items after all moves are performed by the CrateMover 9001.
#' @importFrom magrittr %>%
solve_day5_part2 <- function(input) {
  inital_stacks <- input$initial_stacks
  processed_stacks <- purrr::reduce(
    .x = input$moves,
    .f = move_at_once_item,
    .init = inital_stacks
  ) 
  # This is required because the list order changes whenever a stack gets empty.
  ordered_processed_stacks <- processed_stacks[names(inital_stacks)]  
  read_top(ordered_processed_stacks)
}
