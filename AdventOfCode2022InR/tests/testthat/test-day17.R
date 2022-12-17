

test_that("parse_day17_input works: provided test input", {
  input <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  actual <- parse_day17_input(input)
  expected <- c(
    TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE,
    FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE,
    FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
    FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE 
  )
  expect_equal(actual, expected)
})


test_that("solve_day17_part1 works: provided test input", {
  input_text <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  input <- parse_day17_input(input_text)
  actual <- solve_day17_part1(input)
  expected <- "3068"
  expect_equal(actual, expected)
})


test_that("solve_day17_part2 works: provided test input", {
  input_text <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  input <- parse_day17_input(input_text)
  actual <- solve_day17_part2(input)
  expected <- "1514285714288"
  expect_equal(actual, expected)
})




test_that("solve_day17_part1 works: real input", {
  input <- parse_day17_input(day17_input)
  actual <- solve_day17_part1(input)
  expected <- "3209"
  expect_equal(actual, expected)
})


test_that("solve_day17_part2 works: real input", {
  input <- parse_day17_input(day17_input)
  actual <- solve_day17_part2(input)
  expected <- "1580758017509"
  expect_equal(actual, expected)
})



