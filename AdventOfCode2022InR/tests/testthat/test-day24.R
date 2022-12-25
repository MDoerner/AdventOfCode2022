

test_that("parse_day24_input works: provided test input", {
  input <- "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"
  actual <- parse_day24_input(input)
  expected <- list(
    height = 4L,
    width = 6L,
    blizzards = list(
      c(0L, 0L), c(1L, 0L), c(3L, 0L), c(4L, 0L), c(5L, 0L),
      c(1L, 1L), c(4L, 1L), c(5L, 1L),
      c(0L, 2L), c(1L, 2L), c(3L, 2L), c(4L, 2L), c(5L, 2L),
      c(0L, 3L), c(1L, 3L), c(2L, 3L), c(3L, 3L), c(4L, 3L), c(5L, 3L)
    ),
    directions = list(
      c(1L, 0L), c(1L, 0L), c(-1L, 0L), c(0L, -1L), c(-1L, 0L),
      c(-1L, 0L), c(-1L, 0L), c(-1L, 0L),
      c(1L, 0L), c(0L, 1L), c(1L, 0L), c(-1L, 0L), c(1L, 0L),
      c(-1L, 0L), c(0L, -1L), c(0L, 1L), c(0L, -1L), c(0L, -1L), c(1L, 0L)
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day24_part1 works: provided test input", {
  input_text <- "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"
  input <- parse_day24_input(input_text)
  actual <- solve_day24_part1(input)
  expected <- "18"
  expect_equal(actual, expected)
})


test_that("solve_day24_part2 works: provided test input", {
  input_text <- "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"
  input <- parse_day24_input(input_text)
  actual <- solve_day24_part2(input)
  expected <- "54"
  expect_equal(actual, expected)
})



# TAKES WAY TOO LONG (36 minutes)
# test_that("solve_day24_part1 works: real input", {
#   input <- parse_day24_input(day24_input)
#   actual <- solve_day24_part1(input)
#   expected <- "308"
#   expect_equal(actual, expected)
# })


# TAKES WAY TOO LONG (1.7 hours)
# test_that("solve_day24_part2 works: real input", {
#   input <- parse_day24_input(day24_input)
#   actual <- solve_day24_part2(input)
#   expected <- "908"
#   expect_equal(actual, expected)
# })



