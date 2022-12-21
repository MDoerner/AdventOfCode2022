
#' Module containing the implementation for Day21 of Advent Of Code 2022
#' @noRd
day21 <- modules::module(
  {
    parsing <- modules::use("R/utility/parsing_utils.R")
    hashmap <- modules::use("R/data_structures/hashmap.R")
    stack <- modules::use("R/data_structures/stack.R")
    
    parse_monkey <- function(text) {
      matches <- stringr::str_match(text, pattern = "^(.+): ((-?\\d+)|((.+) ([+*/-]) (.+)))$")
      id <- matches[[2]]
      if (!is.na(matches[[4]])) {
        list(
          id = id,
          type = "number",
          value = readr::parse_integer(matches[[4]])
        )
      } else {
        list(
          id = id,
          type = "operation",
          operator = matches[[7]],
          left = matches[[6]],
          right = matches[[8]]
        )
      }
    }
    
    modules::export("parse_input")
    parse_input <- function(input_string) {
      monkeys <- input_string %>%
        parsing$split_lines() %>%
        purrr::map(parse_monkey)
      is_number_monkey <- purrr::map_lgl(
        .x = monkeys,
        .f = function(monkey) monkey$type == "number"
      )
      
      list(
        numbers = monkeys[is_number_monkey],
        operations = monkeys[!is_number_monkey]
      )
    }
    
    compute_shouted_value <- function(monkey, memory, operation_monkeys) {
      left <- get_shouted_value(monkey$left, memory, operation_monkeys) 
      memory <- left$memory
      right <- get_shouted_value(monkey$right, memory, operation_monkeys)
      memory <- right$memory
      value <- if (monkey$operator == "+") {
        left$value + right$value
      } else if (monkey$operator == "-") {
        left$value - right$value
      } else if (monkey$operator == "*") {
        left$value * right$value
      } else if (monkey$operator == "/") {
        left$value / right$value
      } else {
        cli::cli_abort(c(
          "Possible operators are +, -, * and /.",
          "x" = "unknown operator {monkey$operator} for monkey {monkey_name}"
        ))
      }
      
      list(
        value = value,
        memory = memory
      )
    }
    
    get_shouted_value <- function(monkey_name, memory, operation_monkeys) {
      if (!hashmap$has(memory, monkey_name)) {
        monkey <- operation_monkeys[[monkey_name]]
        result <- compute_shouted_value(monkey, memory, operation_monkeys)
        memory <- store_shout(result$memory, monkey_name, result$value)
      }
      
      list(
        value = hashmap$get(memory, monkey_name),
        memory = memory
      )
    }
    
    store_shout <- function(memory, monkey_name, value) {
      hashmap$set(memory, monkey_name, value)
    }
    
    number_shouted_by <- function(monkey_name, monkeys) {
      memory <- purrr::reduce(
        .x = monkeys$numbers,
        .f = function(memory, monkey) store_shout(memory, monkey$id, monkey$value),
        .init = hashmap$empty_hashmap()
      )
      operation_monkeys <- monkeys$operations
      names(operation_monkeys) <- purrr::map_chr(
        .x = operation_monkeys, 
        .f = function(monkey) monkey$id
      )
      result <- get_shouted_value(monkey_name, memory, operation_monkeys)
      result$value
    }
    
    modules::export("solve_part1")
    solve_part1 <- function(monkeys) {
      result <- number_shouted_by("root", monkeys)
      as.character(result)
    }
    
    
    try_compute_shouted_value <- function(monkey, memory, operation_monkeys, human_name) {
      left <- try_get_shouted_value(monkey$left, memory, operation_monkeys, human_name) 
      memory <- left$memory
      right <- try_get_shouted_value(monkey$right, memory, operation_monkeys, human_name)
      memory <- right$memory

      value <- if (!is.na(left$value) && !is.na(right$value)) {
        if (monkey$operator == "+") {
          left$value + right$value
        } else if (monkey$operator == "-") {
          left$value - right$value
        } else if (monkey$operator == "*") {
          left$value * right$value
        } else if (monkey$operator == "/") {
          left$value / right$value
        } else {
          cli::cli_abort(c(
            "Possible operators are +, -, * and /.",
            "x" = "unknown operator {monkey$operator} for monkey {monkey_name}"
          ))
        }
      } else {
        NA
      }
      
      if (!is.na(value)) {
        return(
          list(
            value = value,
            solve_stacks = NULL,
            memory = memory
          )
        )
      }
      
      if (is.na(left$value)) {
        if (is.na(right$value)) {
          cli::cli_abort(c( "The human may only be part of the left or right side of a formula." ))
        }
        
        solve_stacks <- list(
          operator = stack$push(left$solve_stacks$operator, monkey$operator),
          solve_right = stack$push(left$solve_stacks$solve_right, FALSE),
          other_value = stack$push(left$solve_stacks$other_value, right$value)
        )
        
        return(
          list(
            value = NA,
            solve_stacks = solve_stacks,
            memory = memory
          )
        )
      }
       
      if (is.na(right$value)) {
        if (is.na(left$value)) {
          cli::cli_abort(c( "The human may only be part of the left or right side of a formula." ))
        }
        
        solve_stacks <- list(
          operator = stack$push(right$solve_stacks$operator, monkey$operator),
          solve_right = stack$push(right$solve_stacks$solve_right, TRUE),
          other_value = stack$push(right$solve_stacks$other_value, left$value)
        )
        
        return(
          list(
            value = NA,
            solve_stacks = solve_stacks,
            memory = memory
          )
        )
      }
      
      cli::cli_abort("This should not be possible to reach!!!")
    }
    
    try_get_shouted_value <- function(monkey_name, memory, operation_monkeys, human_name) {
      if (monkey_name == human_name) {
        solve_stacks <- list(
          operator = stack$empty_stack(),
          solve_right = stack$empty_stack(),
          other_value = stack$empty_stack()
        )
        return(
          list(
            value = NA,
            solve_stacks = solve_stacks,
            memory = memory
          )
        )
      }
      
      if (hashmap$has(memory, monkey_name)) {
        return(
          list(
            value = hashmap$get(memory, monkey_name),
            solve_stacks = NULL,
            memory = memory
          )
        )
      }

      monkey <- operation_monkeys[[monkey_name]]
      result <- try_compute_shouted_value(monkey, memory, operation_monkeys, human_name)
      memory <- if (!is.na(result$value)) {
        memory <- store_shout(result$memory, monkey_name, result$value)
      } else {
        result$memory
      }
      
      list(
        value = result$value,
        solve_stacks = result$solve_stacks,
        memory = memory
      )
    }
    
    solve_equation <- function(operator, solve_right, value, other_value) {
      if (operator == "+") {
        value - other_value
      } else if (operator == "*") {
        value / other_value
      } else if (operator == "-") {
        if (solve_right) {
          other_value - value
        } else {
          other_value + value
        }
      } else if (operator == "/") {
        if (solve_right) {
          other_value / value
        } else {
          other_value * value
        }
      } else {
        cli::cli_abort(c(
          "Possible operators are +, -, * and /.",
          "x" = "unknown operator {monkey$operator} for monkey {monkey_name}"
        ))
      }
    }
    
    solve <- function(solve_stacks, value) {
      if (stack$is_empty(solve_stacks$operator)) {
        return(value)
      }
      
      operator <- stack$peek(solve_stacks$operator)
      solve_right <- stack$peek(solve_stacks$solve_right)
      other_value <- stack$peek(solve_stacks$other_value)
      
      new_value <- solve_equation(operator, solve_right, value, other_value)
      
      new_solve_stacks <- list(
        operator = stack$pop(solve_stacks$operator),
        solve_right = stack$pop(solve_stacks$solve_right),
        other_value = stack$pop(solve_stacks$other_value)
      ) 
      
      solve(new_solve_stacks, new_value)
    }
    
    number_to_shout <- function(equation_monkey_name, human_name, monkeys) {
      memory <- purrr::reduce(
        .x = monkeys$numbers,
        .f = function(memory, monkey) store_shout(memory, monkey$id, monkey$value),
        .init = hashmap$empty_hashmap()
      )
      operation_monkeys <- monkeys$operations
      names(operation_monkeys) <- purrr::map_chr(
        .x = operation_monkeys, 
        .f = function(monkey) monkey$id
      )
      equation_monkey <- operation_monkeys[[equation_monkey_name]]
      left <- try_get_shouted_value(equation_monkey$left, memory, operation_monkeys, human_name)
      memory <- left$memory
      right <- try_get_shouted_value(equation_monkey$right, memory, operation_monkeys, human_name)
      memory <- right$memory
      
      value <- if (!is.na(left$value)) {
        left$value
      } else {
        right$value
      }
      
      solve_stacks <- if (is.na(left$value)) {
        left$solve_stacks
      } else {
        right$solve_stacks
      }
      
      solve(solve_stacks, value)
    }
    
    modules::export("solve_part2")
    solve_part2 <- function(monkeys) {
      result <- number_to_shout("root", "humn", monkeys)
      as.character(result)
    }
  }
)


#' Parsed Input to Day21 of Advent Of Code 2022
#'
#' @param input_string Input string to day 21 of AdventOfCode
#'
#' @return Two lists, one for monkeys shouting numbers and one for operation monkeys
#' @importFrom magrittr %>%
parse_day21_input <- function(input_string) {
  day21$parse_input(input_string)
}

#' Solution to Day21 Part1 of Advent of Code 2022
#'
#' @param monkeys Two lists, one for monkeys shouting numbers and one for operation monkeys
#'
#' @return The number the root monkey will shout
#' @importFrom magrittr %>%
solve_day21_part1 <- function(monkeys) {
  day21$solve_part1(monkeys)
}

#' Solution to Day21 Part2 of Advent of Code 2022
#'
#' @param monkeys Two lists, one for monkeys shouting numbers and one for operation monkeys
#'
#' @return The number you have to shout in order for the root monkey equation to be true
#' @importFrom magrittr %>%
solve_day21_part2 <- function(monkeys) {
  day21$solve_part2(monkeys)
}
