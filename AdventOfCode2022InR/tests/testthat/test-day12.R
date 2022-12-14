
hashmap <- modules::use("../../R/data_structures/hashmap.R")

test_that("parse_day12_input works: simplified test input", {
  input <- "Sab
abc
fEz"
  actual <- parse_day12_input(input)
  actual_map <- hashmap$as_list(actual$height_map)
  expected_map <- list(
    "1__1" = 0L,
    "1__2" = 0L,
    "1__3" = 5L,
    "2__1" = 0L,
    "2__2" = 1L,
    "2__3" = 25L,
    "3__1" = 1L,
    "3__2" = 2L,
    "3__3" = 25L
  )
  expected_start = c(1L, 1L)
  expected_end = c(2L, 3L)
  expect_equal(actual$start, expected_start)
  expect_equal(actual$end, expected_end)
  expect_equal(actual_map, expected_map)
})


test_that("solve_day12_part1 works: provided test input", {
  input_text <- "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
  input <- parse_day12_input(input_text)
  actual <- solve_day12_part1(input)
  expected <- "31"
  expect_equal(actual, expected)
})


test_that("solve_day12_part2 works: provided test input", {
  input_text <- "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"
  input <- parse_day12_input(input_text)
  actual <- solve_day12_part2(input)
  expected <- "29"
  expect_equal(actual, expected)
})




test_that("solve_day12_part1 works: real input", {
  input <- parse_day12_input(day12_input)
  actual <- solve_day12_part1(input)
  expected <- "339"
  expect_equal(actual, expected)
})


test_that("solve_day12_part2 works: real input", {
  input <- parse_day12_input(day12_input)
  actual <- solve_day12_part2(input)
  expected <- "332"
  expect_equal(actual, expected)
})



