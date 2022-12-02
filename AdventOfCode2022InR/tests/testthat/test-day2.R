

test_that("parse_day2_input works: provided test input", {
  input <-
    "A Y
B X
C Z"
  actual <- parse_day2_input(input)
  expected_opponent_plays <- factor(c("Rock", "Paper", "Scissors"), levels = c("Rock", "Paper", "Scissors"))
  expected_responses <- c("Y", "X", "Z")
  expect_equal(actual$OpponentPlay, expected_opponent_plays)
  expect_equal(actual$ResponseCode, expected_responses)
})


test_that("solve_day2_part1 works: provided test input", {
  input <- tibble::tibble(
    OpponentPlay = factor(c("Rock", "Paper", "Scissors"), levels = c("Rock", "Paper", "Scissors")),
    ResponseCode = c("Y", "X", "Z")
  )
  actual <- solve_day2_part1(input)
  expected <- "15"
  expect_equal(actual, expected)
})


test_that("solve_day2_part2 works: provided test input", {
  input <- tibble::tibble(
    OpponentPlay = factor(c("Rock", "Paper", "Scissors"), levels = c("Rock", "Paper", "Scissors")),
    ResponseCode = c("Y", "X", "Z")
  )
  actual <- solve_day2_part2(input)
  expected <- "12"
  expect_equal(actual, expected)
})



