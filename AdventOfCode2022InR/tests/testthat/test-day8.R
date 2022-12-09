

test_that("parse_day8_input works: provided test input", {
  input <- "30373
25512
65332
33549
35390"
  actual <- parse_day8_input(input)
  expected <- tibble::tibble(
    X = rep(c(1, 2, 3, 4, 5), times = 1, each = 5),
    Y = rep(c(1, 2, 3, 4, 5), times = 5, each = 1),
    Height = c(
      3, 2, 6, 3, 3,
      0, 5, 5, 3, 5,
      3, 5, 3, 5, 3,
      7, 1, 3, 4, 9,
      3, 2, 2, 9, 0
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day8_part1 works: provided test input", {
  input <- tibble::tibble(
    X = rep(c(1, 2, 3, 4, 5), times = 1, each = 5),
    Y = rep(c(1, 2, 3, 4, 5), times = 5, each = 1),
    Height = c(
      3, 2, 6, 3, 3,
      0, 5, 5, 3, 5,
      3, 5, 3, 5, 3,
      7, 1, 3, 4, 9,
      3, 2, 2, 9, 0
    )
  )
  actual <- solve_day8_part1(input)
  expected <- "21"
  expect_equal(actual, expected)
})


test_that("solve_day8_part2 works: provided test input", {
  input <- tibble::tibble(
    X = rep(c(1, 2, 3, 4, 5), times = 1, each = 5),
    Y = rep(c(1, 2, 3, 4, 5), times = 5, each = 1),
    Height = c(
      3, 2, 6, 3, 3,
      0, 5, 5, 3, 5,
      3, 5, 3, 5, 3,
      7, 1, 3, 4, 9,
      3, 2, 2, 9, 0
    )
  )
  actual <- solve_day8_part2(input)
  expected <- "8"
  expect_equal(actual, expected)
})




test_that("solve_day8_part1 works: real input", {
  input <- parse_day8_input(day8_input)
  actual <- solve_day8_part1(input)
  expected <- "1672"
  expect_equal(actual, expected)
})


# TAKES TOO LONG
# test_that("solve_day8_part2 works: real input", {
#   input <- parse_day8_input(day8_input)
#   actual <- solve_day8_part2(input)
#   expected <- "327180"
#   expect_equal(actual, expected)
# })



