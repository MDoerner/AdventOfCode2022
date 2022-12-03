

test_that("parse_day3_input works: provided test input", {
  input <- "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
  actual <- parse_day3_input(input)
  expected <- list(
    list(
      "left" = c("v", "J", "r", "w", "p", "W", "t", "w", "J", "g", "W", "r"),
      "right" = c("h", "c", "s", "F", "M", "M", "f", "F", "F", "h", "F", "p")
    ),
    list(
      "left" = c("j", "q", "H", "R", "N", "q", "R", "j", "q", "z", "j", "G", "D", "L", "G", "L"),
      "right" = c("r", "s", "F", "M", "f", "F", "Z", "S", "r", "L", "r", "F", "Z", "s", "S", "L")
    ),
    list(
      "left" = c("P", "m", "m", "d", "z", "q", "P", "r", "V"),
      "right" = c("v", "P", "w", "w", "T", "W", "B", "w", "g")
    ),
    list(
      "left" = c("w", "M", "q", "v", "L", "M", "Z", "H", "h", "H", "M", "v", "w", "L", "H"),
      "right" = c("j", "b", "v", "c", "j", "n", "n", "S", "B", "n", "v", "T", "Q", "F", "n")
    ),
    list(
      "left" = c("t", "t", "g", "J", "t", "R", "G", "J"),
      "right" = c("Q", "c", "t", "T", "Z", "t", "Z", "T")
    ),
    list(
      "left" = c("C", "r", "Z", "s", "J", "s", "P", "P", "Z", "s", "G", "z"),
      "right" = c("w", "w", "s", "L", "w", "L", "m", "p", "w", "M", "D", "w")
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day3_part1 works: provided test input", {
  input_texts <- list(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw",
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
  )
  inputs <- purrr::map(input_texts, parse_day3_input)
  actuals <- purrr::map(inputs, solve_day3_part1)
  expected <- list("16", "38", "42", "22", "20", "19", "157")
  for (index in seq_along(actuals)) {
    expect_equal(actuals[[index]], expected[[index]])
  }
})


test_that("solve_day3_part2 works: provided test input", {
  input_texts <- list(
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw",
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
  )
  inputs <- purrr::map(input_texts, parse_day3_input)
  actuals <- purrr::map(inputs, solve_day3_part2)
  expected <- list("18", "52", "70")
  for (index in seq_along(actuals)) {
    expect_equal(actuals[[index]], expected[[index]])
  }
})




test_that("solve_day3_part1 works: real input", {
  input <- parse_day3_input(day3_input)
  actual <- solve_day3_part1(input)
  expected <- "8394"
  expect_equal(actual, expected)
})


test_that("solve_day3_part2 works: real input", {
  input <- parse_day3_input(day3_input)
  actual <- solve_day3_part2(input)
  expected <- "2413"
  expect_equal(actual, expected)
})
