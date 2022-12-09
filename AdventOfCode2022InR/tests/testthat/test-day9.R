

test_that("parse_day9_input works: provided test input", {
  input <- "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2"
  actual <- parse_day9_input(input)
  expected <- list(
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "U",
      distance = 4
    ),
    list(
      direction = "L",
      distance = 3
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "L",
      distance = 5
    ),
    list(
      direction = "R",
      distance = 2
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day9_part1 works: provided test input", {
  input <- list(
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "U",
      distance = 4
    ),
    list(
      direction = "L",
      distance = 3
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "L",
      distance = 5
    ),
    list(
      direction = "R",
      distance = 2
    )
  )
  actual <- solve_day9_part1(input)
  expected <- "13"
  expect_equal(actual, expected)
})


test_that("solve_day9_part2 works: provided test input", {
  input <- list(
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "U",
      distance = 4
    ),
    list(
      direction = "L",
      distance = 3
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "R",
      distance = 4
    ),
    list(
      direction = "D",
      distance = 1
    ),
    list(
      direction = "L",
      distance = 5
    ),
    list(
      direction = "R",
      distance = 2
    )
  )
  actual <- solve_day9_part2(input)
  expected <- "1"
  expect_equal(actual, expected)
})




test_that("solve_day9_part1 works: real input", {
  input <- parse_day9_input(day9_input)
  actual <- solve_day9_part1(input)
  expected <- "6190"
  expect_equal(actual, expected)
})


test_that("solve_day9_part2 works: real input", {
  input <- parse_day9_input(day9_input)
  actual <- solve_day9_part2(input)
  expected <- "2516"
  expect_equal(actual, expected)
})



