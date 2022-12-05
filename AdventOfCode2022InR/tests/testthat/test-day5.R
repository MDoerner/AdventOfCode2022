

test_that("parse_day5_input works: provided test input", {
  input <- 
"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"
  actual <- parse_day5_input(input)
  expected <- list(
    "initial_stacks" = list(
      "1" = c("Z", "N"),
      "2" = c("M", "C", "D"),
      "3" = c("P")
    ),
    "moves" = list(
      list("count" = 1, "from" = "2", "to" = "1"),
      list("count" = 3, "from" = "1", "to" = "3"),
      list("count" = 2, "from" = "2", "to" = "1"),
      list("count" = 1, "from" = "1", "to" = "2")
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day5_part1 works: provided test input", {
  input <- list(
    "initial_stacks" = list(
      "1" = c("Z", "N"),
      "2" = c("M", "C", "D"),
      "3" = c("P")
    ),
    "moves" = list(
      list("count" = 1, "from" = "2", "to" = "1"),
      list("count" = 3, "from" = "1", "to" = "3"),
      list("count" = 2, "from" = "2", "to" = "1"),
      list("count" = 1, "from" = "1", "to" = "2")
    )
  )
  actual <- solve_day5_part1(input)
  expected <- "CMZ"
  expect_equal(actual, expected)
})


test_that("solve_day5_part2 works: provided test input", {
  input <- list(
    "initial_stacks" = list(
      "1" = c("Z", "N"),
      "2" = c("M", "C", "D"),
      "3" = c("P")
    ),
    "moves" = list(
      list("count" = 1, "from" = "2", "to" = "1"),
      list("count" = 3, "from" = "1", "to" = "3"),
      list("count" = 2, "from" = "2", "to" = "1"),
      list("count" = 1, "from" = "1", "to" = "2")
    )
  )
  actual <- solve_day5_part2(input)
  expected <- "MCD"
  expect_equal(actual, expected)
})




test_that("solve_day5_part1 works: real input", {
  input <- parse_day5_input(day5_input)
  actual <- solve_day5_part1(input)
  expected <- "CNSZFDVLJ"
  expect_equal(actual, expected)
})


test_that("solve_day5_part2 works: real input", {
  input <- parse_day5_input(day5_input)
  actual <- solve_day5_part2(input)
  expected <- "QNDWLMGNS"
  expect_equal(actual, expected)
})



