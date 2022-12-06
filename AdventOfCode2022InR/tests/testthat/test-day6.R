

test_that("parse_day6_input works: provided test input", {
  input <- "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  actual <- parse_day6_input(input)
  expected <- c("m", "j", "q", "j", "p", "q", "m", "g", "b", "l", "j", "s", "p", "h", "d", "z", "t", "n", "v", "j", "f", "q", "w", "r", "c", "g", "s", "m", "l", "b")
  expect_equal(actual, expected)
})


test_that("solve_day6_part1 works: provided test input", {
  input_texts <- list(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )
  inputs <- purrr::map(input_texts, parse_day6_input)
  actuals <- purrr::map(inputs, solve_day6_part1)
  expected <- list("7", "5", "6", "10", "11")
  for (index in seq_along(actuals)) {
    expect_equal(actuals[[index]], expected[[index]])
  }
})


test_that("solve_day6_part2 works: provided test input", {
  input_texts <- list(
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  )
  inputs <- purrr::map(input_texts, parse_day6_input)
  actuals <- purrr::map(inputs, solve_day6_part2)
  expected <- list("19", "23", "23", "29", "26")
  for (index in seq_along(actuals)) {
    expect_equal(actuals[[index]], expected[[index]])
  }
})




test_that("solve_day6_part1 works: real input", {
  input <- parse_day6_input(day6_input)
  actual <- solve_day6_part1(input)
  expected <- "1896"
  expect_equal(actual, expected)
})


test_that("solve_day6_part2 works: real input", {
  input <- parse_day6_input(day6_input)
  actual <- solve_day6_part2(input)
  expected <- "3452"
  expect_equal(actual, expected)
})



