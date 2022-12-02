



play_from_text <- function(play_text) {
  factor(
    play_text,
    levels = c("Rock", "Paper", "Scissors"),
    ordered = FALSE
  )
}



#' Opponent Play from Code
#'
#' Uses the Strategy Guide explanation to determine the enemy play
#'
#' @param code The character vector with items A, B or C
#'
#' @return A factor indicating the opponent play
opponent_play_from_code <- function(code) {
  play_text <- dplyr::case_when(
    code == "A" ~ "Rock",
    code == "B" ~ "Paper",
    code == "C" ~ "Scissors",
    TRUE ~ "ERROR"
  )

  error_records <- code[play_text == "ERROR"]
  if (length(error_records) > 0) {
    cli::cli_abort(c("x" = "{code} is not a valid oponent play code for Rock Paper Scissors."))
  }

  play_from_text(play_text)
}




#' Parsed Input to Day1 of Advent Of Code 2022
#'
#' @param input_string Input string to day 2 of AdventOfCode
#'
#' @return A tibble with columns `OpponentPlay` and `ResponseCode`
#' @importFrom magrittr %>%
parse_day2_input <- function(input_string) {
  column_spec <- readr::cols(
    X1 = readr::col_character(),
    X2 = readr::col_character()
  )
  readr::read_delim(
    I(input_string),
    delim = " ",
    col_names = FALSE,
    col_types = column_spec
  ) %>%
    dplyr::mutate(
      OpponentPlay = opponent_play_from_code(X1),
      ResponseCode = X2
    ) %>%
    dplyr::select(
      OpponentPlay,
      ResponseCode
    )
}



#' Result Table for Rock Paper Scissors Games
game_outcomes <- tibble::tribble(
  ~PlayerPlay, ~OpponentPlay, ~Outcome,
  "Rock", "Rock", "Draw",
  "Rock", "Paper", "Loss",
  "Rock", "Scissors", "Win",
  "Paper", "Rock", "Win",
  "Paper", "Paper", "Draw",
  "Paper", "Scissors", "Loss",
  "Scissors", "Rock", "Loss",
  "Scissors", "Paper", "Win",
  "Scissors", "Scissors", "Draw"
)


outcome_from_text <- function(outcome_text) {
  factor(
    outcome_text,
    levels = c("Loss", "Draw", "Win"),
    ordered = FALSE
  )
}


#' Results of Playing Rock Paper Scissors
#'
#' @param player_play A factor vector of Rock Paper Scissor plays
#' @param opponent_play A factor vector of Rock Paper Scissor plays the same length as `player_play`
#'
#' @return A factor with the result of the games with levels Loss, Draw and Win
#' @importFrom magrittr %>%
game_outcome <- function(player_play, opponent_play) {
  plays <- tibble::tibble(
    PlayerPlay = player_play,
    OpponentPlay = opponent_play
  )

  outcome_text <- plays %>%
    dplyr::inner_join(
      game_outcomes,
      by = c("PlayerPlay", "OpponentPlay")
    ) %>%
    dplyr::pull(Outcome)

  outcome_from_text(outcome_text)
}




play_score <- function(play) {
  dplyr::case_when(
    play == "Rock" ~ 1,
    play == "Paper" ~ 2,
    play == "Scissors" ~ 3,
    TRUE ~ -9999
  )
}

outcome_score <- function(outcome) {
  dplyr::case_when(
    outcome == "Loss" ~ 0,
    outcome == "Draw" ~ 3,
    outcome == "Win" ~ 6,
    TRUE ~ -9999
  )
}


round_score <- function(player_play, opponent_play) {
  play_score(player_play) + outcome_score(game_outcome(player_play, opponent_play))
}


game_score <- function(player_plays, opponent_plays) {
  sum(round_score(player_plays, opponent_plays))
}


part1_player_play_from_response_code <- function(response_code) {
  play_text <- dplyr::case_when(
    response_code == "X" ~ "Rock",
    response_code == "Y" ~ "Paper",
    response_code == "Z" ~ "Scissors",
    TRUE ~ "ERROR"
  )

  error_records <- response_code[play_text == "ERROR"]
  if (length(error_records) > 0) {
    cli::cli_abort(c("x" = "{code} is not a valid response code form the Rock Paper Scissors Strategy Guide."))
  }

  play_from_text(play_text)
}



#' Solution to Day2 Part1 of Advent of Code 2022
#'
#' @param strategy_guide A tibble with columns `OpponentPlay` and `ResponseCode`
#'
#' @return The final player score if the guide is followed based on the part 1 assumptions 
#' @importFrom magrittr %>%
solve_day2_part1 <- function(strategy_guide) {
  opponent_plays <- strategy_guide$OpponentPlay
  player_plays <- part1_player_play_from_response_code(strategy_guide$ResponseCode)
  total_player_score <- game_score(player_plays, opponent_plays)
  as.character(total_player_score)
}


desired_outcome_from_response_code <- function(response_code) {
  outcome_text <- dplyr::case_when(
    response_code == "X" ~ "Loss",
    response_code == "Y" ~ "Draw",
    response_code == "Z" ~ "Win",
    TRUE ~ "ERROR"
  )
  
  error_records <- response_code[outcome_text == "ERROR"]
  if (length(error_records) > 0) {
    cli::cli_abort(c("x" = "{code} is not a valid response code form the Rock Paper Scissors Strategy Guide."))
  }
  
  outcome_from_text(outcome_text)
}

player_play_with_outcome <- function(outcome, opponent_play){
  play_situation <- tibble::tibble(
    Outcome = outcome,
    OpponentPlay = opponent_play
  )
  
  play_text <- play_situation %>%
    dplyr::inner_join(
      game_outcomes,
      by = c("Outcome", "OpponentPlay")
    ) %>%
    dplyr::pull(PlayerPlay)
  
  play_from_text(play_text)
}


part2_player_play_from_response_code <- function(response_code, opponent_play) {
  response_code %>%
    desired_outcome_from_response_code() %>%
    player_play_with_outcome(opponent_play)
}


#' Solution to Day2 Part2 of Advent of Code 2022
#'
#' @param strategy_guide A tibble with columns `OpponentPlay` and `ResponseCode`
#'
#' @return 
#' @importFrom magrittr %>%
solve_day2_part2 <- function(strategy_guide) {
  opponent_plays <- strategy_guide$OpponentPlay
  player_plays <- part2_player_play_from_response_code(strategy_guide$ResponseCode, opponent_plays)
  total_player_score <- game_score(player_plays, opponent_plays)
  as.character(total_player_score)
}
