

test_that("parse_day22_input works: provided test input", {
  input <- "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
  actual <- parse_day22_input(input)
  expected <- list(
    borders = list(
      top = c(5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 1L, 1L, 1L, 1L, 9L, 9L, 9L, 9L),
      bottom = c(8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L),
      left = c(9L, 9L, 9L, 9L, 1L, 1L, 1L, 1L, 9L, 9L, 9L, 9L),
      right = c(12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 16L, 16L, 16L, 16L)
    ),
    walls = list(
      by_column = list(
        NA_integer_,
        NA_integer_,
        7L,
        5L,
        NA_integer_,
        NA_integer_,
        NA_integer_,
        7L,
        c(3L, 6L),
        c(2L, 11L),
        8L,
        c(1L, 5L, 9L),
        NA_integer_,
        10L,
        12L,
        NA_integer_
      ),
      by_row = list(
        12L,
        10L,
        9L,
        NA_integer_,
        c(4L, 12L),
        9L,
        c(3L, 8L),
        11L,
        12L,
        14L,
        10L,
        15L
      )
    ),
    moves = list(
      distances = c(10L, 5L, 5L, 10L, 4L, 5L, 5L),
      rotations = c("R", "L", "R", "L", "R", "L")
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day22_part1 works: provided test input", {
  input_text <- "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
  input <- parse_day22_input(input_text)
  actual <- solve_day22_part1(input)
  expected <- "6032"
  expect_equal(actual, expected)
})


test_that("solve_day22_part2 works: provided test input", {
  input_text <- "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5"
  input <- parse_day22_input(input_text)
  actual <- solve_day22_part2(input)
  expected <- "5031"
  expect_equal(actual, expected)
})




test_that("solve_day22_part1 works: real input", {
  input <- parse_day22_input(day22_input)
  actual <- solve_day22_part1(input)
  expected <- "1484"
  expect_equal(actual, expected)
})


test_that("solve_day22_part2 works: real input", {
  input <- parse_day22_input(day22_input)
  actual <- solve_day22_part2(input)
  expected <- "142228"
  expect_equal(actual, expected)
})



