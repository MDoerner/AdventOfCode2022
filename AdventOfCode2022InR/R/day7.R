



## Parsing

split_commands <- function(console_text) {
  commands <- stringr::str_split(console_text, "\r?\n\\$ ") %>% unlist()
  # The `$ ` on the first command needs to be stripped.
  commands[[1]] <- stringr::str_sub(commands[[1]], start = 3L)
  commands
}

parse_cd <- function(command_text) {
  list(
    command = "cd",
    argument = stringr::str_sub(command_text, start = 4L) # Format is 'cd <argument>'
  )
}

parse_ls_output_item <- function(ls_output_line) {
  if (stringr::str_sub(ls_output_line, start = 1L, end = 3L) == "dir") {
    # Format is 'dir <name>'
    list(
      type = "dir",
      name = stringr::str_sub(ls_output_line, start = 5L)
    )
  } else {
    # Format is '<size> <name>'
    parts <- stringr::str_split(ls_output_line, " ") %>% unlist()
    list(
      type = "file",
      name = parts[[2]],
      size = readr::parse_integer(parts[[1]])
    )
  }
}


parse_ls <- function(command_text) {
  command_lines <- split_lines(command_text)
  output_lines <- command_lines[2:length(command_lines)]
  list(
    command = "ls",
    output = purrr::map(
      .x = output_lines,
      .f = parse_ls_output_item
    )
  )
}


parse_command <- function(command_text) {
  command_name <- stringr::str_sub(command_text, start = 1L, end = 2L)
  if (command_name == "cd") {
    parse_cd(command_text)
  } else if (command_name == "ls") {
    parse_ls(command_text)
  } else {
    cli::cli_abort(c(
      "Command must be either 'cd' or 'ls'.",
      "x" = "Command {command_name} not recognized."
    ))
  }
}



#' Parsed Input to Day7 of Advent Of Code 2022
#'
#' @param input_string Input string to day 7 of AdventOfCode
#'
#' @return List of command line file system commands with output
#' @importFrom magrittr %>%
parse_day7_input <- function(input_string) {
  input_string %>%
    split_commands() %>%
    purrr::map(parse_command)
}



## File System

get_empty_file_system <- function() {
  rlang::new_environment()
}

add_subdirectory <- function(directory, name) {
  rlang::env_poke(
    env = directory,
    nm = name,
    value = rlang::child_env(directory)
  )
  directory
}

add_file <- function(directory, name, size) {
  rlang::env_poke(
    env = directory,
    nm = name,
    value = size
  )
  directory
}

add_item_to_directory <- function(directory, item) {
  if (item$type == "dir") {
    add_subdirectory(directory, item$name)
  } else if (item$type == "file") {
    add_file(directory, item$name, item$size)
  } else {
    cli::cli_abort(c(
      "Item type must be either 'dir' or 'file'.",
      "x" = "Type {item$type} not recognized."
    ))
  }
}

fill_directory <- function(directory, content_list) {
  purrr::reduce(
    .x = content_list,
    .f = add_item_to_directory,
    .init = directory
  )
}


change_directory <- function(current_directory, target, root) {
  if (target == "/") {
    root
  } else if (target == "..") {
    rlang::env_parent(current_directory)
  } else {
    current_directory[[target]]
  }
}

process_file_system_command <- function(current_directory, command, root) {
  if (command$command == "cd") {
    change_directory(current_directory, command$argument, root)
  } else if (command$command == "ls") {
    fill_directory(current_directory, command$output)
  } else {
    cli::cli_abort(c(
      "Command must be either 'cd' or 'ls'.",
      "x" = "Command {command$command} not recognized."
    ))
  }
}

file_system_from_command_line <- function(commands) {
  root <- get_empty_file_system()
  apply_command <- purrr::partial(
    .f = process_file_system_command,
    root = root
  )
  purrr::reduce(
    .x = commands,
    .f = apply_command,
    .init = root
  )
  root
}

## File System Utility

get_subdirectory_names <- function(directory) {
  item_names <- rlang::env_names(directory)
  values <- rlang::env_get_list(
    env = directory,
    nms = item_names,
    default = NA
  )
  is_directory <- values %>%
    purrr::map(rlang::is_environment) %>%
    unlist()
  item_names[is_directory]
}

get_subdirectories <- function(directory) {
  rlang::env_get_list(
    env = directory,
    nms = get_subdirectory_names(directory),
    default = NA
  )
}

get_file_names <- function(directory) {
  item_names <- rlang::env_names(directory)
  values <- rlang::env_get_list(
    env = directory,
    nms = item_names,
    default = NA
  )
  is_directory <- values %>%
    purrr::map(rlang::is_environment) %>%
    unlist()
  item_names[!is_directory]
}

directory_files_size <- function(directory) {
  file_names <- get_file_names(directory)
  rlang::env_get_list(
      env = directory,
      nms = file_names,
      default = NA
    ) %>% 
    unlist() %>%
    sum()
}  

# Memoization would make this a linear time algorithm.
# However, given the input size at hand, quadratic is ok.
directory_size <- function(directory) {
  own_size <- directory_files_size(directory)
  subdirectory_size <- get_subdirectories(directory) %>%
    purrr::map(directory_size) %>% 
    unlist() %>%
    sum()
  own_size + subdirectory_size
} 

post_fix_walk <- function(directory, valuation_function) {
  subdirectory_results <- get_subdirectories(directory) %>%
    purrr::map(
      purrr::partial(
        post_fix_walk,
        valuation_function = valuation_function
      )
    ) %>% 
    unlist()
  c(
    subdirectory_results,
    valuation_function(directory)
  )
} 


#' Solution to Day7 Part1 of Advent of Code 2022
#'
#' @param commands List of command line file system commands with output
#'
#' @return Sum of sizes of all directories with size <= 100000
#' @importFrom magrittr %>%
solve_day7_part1 <- function(commands) {
  file_system <- file_system_from_command_line(commands)
  file_sizes <- post_fix_walk(file_system, directory_size)
  file_sizes[file_sizes <= 100000] %>%
    sum %>%
    as.character()
}


#' Solution to Day7 Part2 of Advent of Code 2022
#'
#' @param commands List of command line file system commands with output
#'
#' @return Smallest directory size larger than the missing space to upgrade
#' @importFrom magrittr %>%
solve_day7_part2 <- function(commands) {
  file_system <- file_system_from_command_line(commands)
  
  storage_capacity <- 70000000L
  required_space <- 30000000L
  current_occupied_space <- directory_size(file_system)
  current_free_space <- storage_capacity - current_occupied_space
  space_to_free <- required_space - current_free_space
  
  file_sizes <- post_fix_walk(file_system, directory_size)
  file_sizes[file_sizes >= space_to_free] %>%
    min %>%
    as.character()
}
