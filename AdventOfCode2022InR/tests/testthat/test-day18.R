

test_that("parse_day18_input works: provided test input", {
  input <- "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5"
  actual <- parse_day18_input(input)
  expected <- list(
    c(2L, 2L, 2L),
    c(1L, 2L, 2L),
    c(3L, 2L, 2L),
    c(2L, 1L, 2L),
    c(2L, 3L, 2L),
    c(2L, 2L, 1L),
    c(2L, 2L, 3L),
    c(2L, 2L, 4L),
    c(2L, 2L, 6L),
    c(1L, 2L, 5L),
    c(3L, 2L, 5L),
    c(2L, 1L, 5L),
    c(2L, 3L, 5L)
  )
  expect_equal(actual, expected)
})


test_that("solve_day18_part1 works: provided test input", {
  input <- list(
    c(2L, 2L, 2L),
    c(1L, 2L, 2L),
    c(3L, 2L, 2L),
    c(2L, 1L, 2L),
    c(2L, 3L, 2L),
    c(2L, 2L, 1L),
    c(2L, 2L, 3L),
    c(2L, 2L, 4L),
    c(2L, 2L, 6L),
    c(1L, 2L, 5L),
    c(3L, 2L, 5L),
    c(2L, 1L, 5L),
    c(2L, 3L, 5L)
  )
  actual <- solve_day18_part1(input)
  expected <- "64"
  expect_equal(actual, expected)
})


test_that("solve_day18_part2 works: provided test input", {
  input <- list(
    c(2L, 2L, 2L),
    c(1L, 2L, 2L),
    c(3L, 2L, 2L),
    c(2L, 1L, 2L),
    c(2L, 3L, 2L),
    c(2L, 2L, 1L),
    c(2L, 2L, 3L),
    c(2L, 2L, 4L),
    c(2L, 2L, 6L),
    c(1L, 2L, 5L),
    c(3L, 2L, 5L),
    c(2L, 1L, 5L),
    c(2L, 3L, 5L)
  )
  actual <- solve_day18_part2(input)
  expected <- "58"
  expect_equal(actual, expected)
})




test_that("solve_day18_part1 works: real input", {
  input <- parse_day18_input(day18_input)
  actual <- solve_day18_part1(input)
  expected <- "4332"
  expect_equal(actual, expected)
})


test_that("solve_day18_part2 works: real input", {
  input <- parse_day18_input(day18_input)
  actual <- solve_day18_part2(input)
  expected <- "2524"
  expect_equal(actual, expected)
})



