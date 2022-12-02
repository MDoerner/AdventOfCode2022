

test_that("parse_day1_input works: provided test input", {
  input <-
    "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"
  actual <- parse_day1_input(input)
  expected <- list(
    c(1000, 2000, 3000),
    c(4000),
    c(5000, 6000),
    c(7000, 8000, 9000),
    c(10000)
  )
  expect_equal(actual, expected)
})


test_that("solve_day1_part1 works: provided test input", {
  input <- list(
    c(1000, 2000, 3000),
    c(4000),
    c(5000, 6000),
    c(7000, 8000, 9000),
    c(10000)
  )
  actual <- solve_day1_part1(input)
  expected <- "24000"
  expect_equal(actual, expected)
})


test_that("solve_day1_part2 works: provided test input", {
  input <- list(
    c(1000, 2000, 3000),
    c(4000),
    c(5000, 6000),
    c(7000, 8000, 9000),
    c(10000)
  )
  actual <- solve_day1_part2(input)
  expected <- "45000"
  expect_equal(actual, expected)
})




test_that("solve_day1_part1 works: real input", {
  input <- parse_day1_input(day1_input)
  actual <- solve_day1_part1(input)
  expected <- "69836"
  expect_equal(actual, expected)
})


test_that("solve_day1_part2 works: real input", {
  input <- parse_day1_input(day1_input)
  actual <- solve_day1_part2(input)
  expected <- "207968"
  expect_equal(actual, expected)
})
