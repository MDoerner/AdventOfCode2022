

test_that("parse_day20_input works: provided test input", {
  input <- "1
2
-3
3
-2
0
4"
  actual <- parse_day20_input(input)
  expected <- c(1L, 2L, -3L, 3L, -2L, 0L, 4L)
  expect_equal(actual, expected)
})


test_that("solve_day20_part1 works: provided test input", {
  input <- c(1L, 2L, -3L, 3L, -2L, 0L, 4L)
  actual <- solve_day20_part1(input)
  expected <- "3"
  expect_equal(actual, expected)
})


test_that("solve_day20_part2 works: provided test input", {
  input <- c(1L, 2L, -3L, 3L, -2L, 0L, 4L)
  actual <- solve_day20_part2(input)
  expected <- "1623178306"
  expect_equal(actual, expected)
})




test_that("solve_day20_part1 works: real input", {
  input <- parse_day20_input(day20_input)
  actual <- solve_day20_part1(input)
  expected <- "6387"
  expect_equal(actual, expected)
})


test_that("solve_day20_part2 works: real input", {
  input <- parse_day20_input(day20_input)
  actual <- solve_day20_part2(input)
  expected <- "2455057187825"
  expect_equal(actual, expected)
})



