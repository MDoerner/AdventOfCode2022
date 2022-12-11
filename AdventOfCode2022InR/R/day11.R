


#' Module containing the implementation for Day11 of Advent Of Code 2022
#' @noRd
day11 <- modules::module({
  parsing_utils <- modules::use("R/utility/parsing_utils.R")

  # Format is as follows.
  # Monkey <id>:
  #   Starting items: <start_items>(comma separated)
  #   Operation: new = old <operator> <operand>
  #   Test: divisible by <test_divisor>
  #     If true: throw to monkey <true_target>
  #     If false: throw to monkey <false_target>
  parse_monkey <- function(text) {
    lines <- parsing_utils$split_lines(text)
    id <- lines[[1]] %>%
      stringr::str_sub(start = 8L, end = -2L) %>%
      readr::parse_integer()
    start_items <- lines[[2]] %>%
      stringr::str_sub(start = 19L) %>%
      stringr::str_split(pattern = ", ") %>%
      unlist() %>%
      purrr::map(readr::parse_integer) %>%
      unlist()
    operator <- lines[[3]] %>%
      stringr::str_sub(start = 24L, end = 24L)
    operand <- lines[[3]] %>%
      stringr::str_sub(start = 26L)
    if (operand != "old") {
      operand <- readr::parse_integer(operand)
    }
    test_divisor <- lines[[4]] %>%
      stringr::str_sub(start = 22L) %>%
      readr::parse_integer()
    true_target <- lines[[5]] %>%
      stringr::str_sub(start = 30L) %>%
      readr::parse_integer()
    false_target <- lines[[6]] %>%
      stringr::str_sub(start = 31L) %>%
      readr::parse_integer()
    list(
      id = paste0("M", id),
      start_items = start_items,
      operator = operator,
      operand = operand,
      test_divisor = test_divisor,
      true_target = paste0("M", true_target),
      false_target = paste0("M", false_target)
    )
  }

  monkey_processor <- modules::module({
    queue <- modules::use("R/data_structures/queue.R")

    start_items <- function(monkeys) {
      items <- monkeys %>%
        purrr::map(function(monkey) {
          item_queue <- queue$empty_queue()
          queue$enqueue(item_queue, monkey$start_items)
        })
      names(items) <- names(monkeys)
      items
    }

    process_item <- function(monkey, item_worry_level, worry_level_divisor, test_divisor_product) {
      operand <- if (monkey$operand == "old") {
        item_worry_level
      } else {
        monkey$operand
      }
      intermediate_worry_level <- if (monkey$operator == "+") {
        as.double(operand) + item_worry_level
      } else if (monkey$operator == "*") {
        as.double(operand) * item_worry_level
      } else {
        cli::cli_abort(c(
          "Known operators are '+' and '*'.",
          "x" = "Unknown operator '{monkey$operator}'."
        ))
      }
      # This is technically the same as flooring the result of the division.
      # However, this representation makes it easier to understand why the next step is ok.
      new_worry_level_base <- if (worry_level_divisor == 1L) intermediate_worry_level else (intermediate_worry_level - intermediate_worry_level %% worry_level_divisor) / worry_level_divisor
      # This has no influence on the values module any of the test_divisors or the worry_level_divisor.
      new_worry_level <- new_worry_level_base %% (worry_level_divisor * test_divisor_product)
      target <- if (new_worry_level %% monkey$test_divisor == 0) {
        monkey$true_target
      } else {
        monkey$false_target
      }

      list(
        worry_level = new_worry_level,
        target = target
      )
    }

    modules::export("process_monkey")
    process_monkey <- function(monkey, items, worry_level_divisor, test_divisor_product) {
      current_monkey_items <- items[[monkey$id]]
      while (!queue$is_empty(current_monkey_items)) {
        item_to_inspect <- queue$peek(current_monkey_items)
        item_to_pass <- process_item(monkey, item_to_inspect, worry_level_divisor, test_divisor_product)
        items[[item_to_pass$target]] <- queue$enqueue(items[[item_to_pass$target]], item_to_pass$worry_level)
        current_monkey_items <- queue$dequeu(current_monkey_items)
      }
      items[[monkey$id]] <- current_monkey_items
      items
    }

    modules::export("process_round")
    process_round <- function(monkeys, items, items_thrown_so_far, worry_level_divisor) {
      test_divisor_product <- monkeys %>%
        purrr::map(function(monkey) monkey$test_divisor) %>%
        unlist() %>%
        prod()
      process_result <- purrr::reduce(
        .x = seq_along(monkeys),
        .f = function(state, monkey_index) {
          monkey <- monkeys[[monkey_index]]
          items_thrown <- state$items_thrown
          items_thrown[[monkey_index]] <- items_thrown[[monkey_index]] + queue$size(state$items[[monkey$id]])
          list(
            items_thrown = items_thrown,
            items = process_monkey(monkey, state$items, worry_level_divisor, test_divisor_product)
          )
        },
        .init = list(
          items_thrown = items_thrown_so_far,
          items = items
        )
      )
      list(
        items_thrown = process_result$items_thrown,
        items = purrr::map(
          .x = process_result$items,
          .f = queue$as_vector
        )
      )
    }

    modules::export("process_rounds")
    process_rounds <- function(monkeys, rounds, worry_level_divisor) {
      initial_items <- start_items(monkeys)
      items_thrown_so_far <- rep(0L, times = length(monkeys))
      purrr::reduce(
        .x = 1:rounds,
        .f = function(state, round) process_round(monkeys, state$items, state$items_thrown, worry_level_divisor),
        .init = list(
          items_thrown = items_thrown_so_far,
          items = initial_items
        )
      )
    }
  })

  modules::export("parse_input")
  parse_input <- function(input_string) {
    monkeys <- input_string %>%
      parsing_utils$split_line_separated_blocks() %>%
      purrr::map(parse_monkey)
    monkey_names <- purrr::map(
      .x = monkeys,
      .f = function(monkey) monkey$id
    )
    names(monkeys) <- monkey_names
    monkeys
  }


  modules::export("solve_part1")
  solve_part1 <- function(monkeys) {
    process_result <- monkey_processor$process_rounds(monkeys, rounds = 20L, worry_level_divisor = 3L)
    sorted_throw_counts <- process_result$items_thrown %>%
      sort(decreasing = TRUE)
    result <- sorted_throw_counts[[1]] * sorted_throw_counts[[2]]
    as.character(result)
  }


  modules::export("solve_part2")
  solve_part2 <- function(monkeys) {
    process_result <- monkey_processor$process_rounds(monkeys, rounds = 10000L, worry_level_divisor = 1L)
    sorted_throw_counts <- process_result$items_thrown %>%
      sort(decreasing = TRUE)
    # Need to coerce to double because R integers are too small.
    result <- as.double(sorted_throw_counts[[1]]) * sorted_throw_counts[[2]]
    as.character(result)
  }
})


#' Parsed Input to Day11 of Advent Of Code 2022
#'
#' @param input_string Input string to day 11 of AdventOfCode
#'
#' @return A list of monkeys
#' @importFrom magrittr %>%
parse_day11_input <- function(input_string) {
  day11$parse_input(input_string)
}

#' Solution to Day11 Part1 of Advent of Code 2022
#'
#' @param monkeys A list of monkeys
#'
#' @return The product of the number of items thrown by the two monkeys throwing the most in the first 20 rounds
#' @importFrom magrittr %>%
solve_day11_part1 <- function(monkeys) {
  day11$solve_part1(monkeys)
}

#' Solution to Day11 Part2 of Advent of Code 2022
#'
#' @param monkeys A list of monkeys
#'
#' @return TO DO
#' @importFrom magrittr %>%
solve_day11_part2 <- function(monkeys) {
  day11$solve_part2(monkeys)
}
