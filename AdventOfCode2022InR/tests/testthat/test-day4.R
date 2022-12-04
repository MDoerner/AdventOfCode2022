

test_that("parse_day4_input works: provided test input", {
  input <- "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"
  actual <- parse_day4_input(input)
  expected <- list(
    list(
      c(2L, 4L),
      c(6L, 8L)
    ),
    list(
      c(2L, 3L),
      c(4L, 5L)
    ),
    list(
      c(5L, 7L),
      c(7L, 9L)
    ),
    list(
      c(2L, 8L),
      c(3L, 7L)
    ),
    list(
      c(6L, 6L),
      c(4L, 6L)
    ),
    list(
      c(2L, 6L),
      c(4L, 8L)
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day4_part1 works: provided test input", {
  input <- list(
    list(
      c(2L, 4L),
      c(6L, 8L)
    ),
    list(
      c(2L, 3L),
      c(4L, 5L)
    ),
    list(
      c(5L, 7L),
      c(7L, 9L)
    ),
    list(
      c(2L, 8L),
      c(3L, 7L)
    ),
    list(
      c(6L, 6L),
      c(4L, 6L)
    ),
    list(
      c(2L, 6L),
      c(4L, 8L)
    )
  )
  actual <- solve_day4_part1(input)
  expected <- "2"
  expect_equal(actual, expected)
})


test_that("solve_day4_part2 works: provided test input", {
  input <- list(
    list(
      c(2L, 4L),
      c(6L, 8L)
    ),
    list(
      c(2L, 3L),
      c(4L, 5L)
    ),
    list(
      c(5L, 7L),
      c(7L, 9L)
    ),
    list(
      c(2L, 8L),
      c(3L, 7L)
    ),
    list(
      c(6L, 6L),
      c(4L, 6L)
    ),
    list(
      c(2L, 6L),
      c(4L, 8L)
    )
  )
  actual <- solve_day4_part2(input)
  expected <- "4"
  expect_equal(actual, expected)
})




test_that("solve_day4_part1 works: real input", {
  input <- parse_day4_input(day4_input)
  actual <- solve_day4_part1(input)
  expected <- "305"
  expect_equal(actual, expected)
})


test_that("solve_day4_part2 works: real input", {
  input <- parse_day4_input(day4_input)
  actual <- solve_day4_part2(input)
  expected <- "811"
  expect_equal(actual, expected)
})



