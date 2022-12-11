

test_that("parse_day11_input works: provided test input", {
  input <- "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"
  actual <- parse_day11_input(input)
  expected <- list(
    M0 = list(
      id = "M0",
      start_items = c(79L, 98L),
      operator = "*",
      operand = 19L,
      test_divisor = 23L,
      true_target = "M2",
      false_target = "M3"
    ),
    M1 = list(
      id = "M1",
      start_items = c(54L, 65L, 75L, 74L),
      operator = "+",
      operand = 6L,
      test_divisor = 19L,
      true_target = "M2",
      false_target = "M0"
    ),
    M2 = list(
      id = "M2",
      start_items = c(79L, 60L, 97L),
      operator = "*",
      operand = "old",
      test_divisor = 13L,
      true_target = "M1",
      false_target = "M3"
    ),
    M3 = list(
      id = "M3",
      start_items = c(74L),
      operator = "+",
      operand = 3L,
      test_divisor = 17L,
      true_target = "M0",
      false_target = "M1"
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day11_part1 works: provided test input", {
  input <- list(
    M0 = list(
      id = "M0",
      start_items = c(79L, 98L),
      operator = "*",
      operand = 19L,
      test_divisor = 23L,
      true_target = "M2",
      false_target = "M3"
    ),
    M1 = list(
      id = "M1",
      start_items = c(54L, 65L, 75L, 74L),
      operator = "+",
      operand = 6L,
      test_divisor = 19L,
      true_target = "M2",
      false_target = "M0"
    ),
    M2 = list(
      id = "M2",
      start_items = c(79L, 60L, 97L),
      operator = "*",
      operand = "old",
      test_divisor = 13L,
      true_target = "M1",
      false_target = "M3"
    ),
    M3 = list(
      id = "M3",
      start_items = c(74L),
      operator = "+",
      operand = 3L,
      test_divisor = 17L,
      true_target = "M0",
      false_target = "M1"
    )
  )
  actual <- solve_day11_part1(input)
  expected <- "10605"
  expect_equal(actual, expected)
})


test_that("solve_day11_part2 works: provided test input", {
  input <- list(
    M0 = list(
      id = "M0",
      start_items = c(79L, 98L),
      operator = "*",
      operand = 19L,
      test_divisor = 23L,
      true_target = "M2",
      false_target = "M3"
    ),
    M1 = list(
      id = "M1",
      start_items = c(54L, 65L, 75L, 74L),
      operator = "+",
      operand = 6L,
      test_divisor = 19L,
      true_target = "M2",
      false_target = "M0"
    ),
    M2 = list(
      id = "M2",
      start_items = c(79L, 60L, 97L),
      operator = "*",
      operand = "old",
      test_divisor = 13L,
      true_target = "M1",
      false_target = "M3"
    ),
    M3 = list(
      id = "M3",
      start_items = c(74L),
      operator = "+",
      operand = 3L,
      test_divisor = 17L,
      true_target = "M0",
      false_target = "M1"
    )
  )
  actual <- solve_day11_part2(input)
  expected <- "2713310158"
  expect_equal(actual, expected)
})




test_that("solve_day11_part1 works: real input", {
  input <- parse_day11_input(day11_input)
  actual <- solve_day11_part1(input)
  expected <- "58794"
  expect_equal(actual, expected)
})

# TAKES TOO LONG
# test_that("solve_day11_part2 works: real input", {
#   input <- parse_day11_input(day11_input)
#   actual <- solve_day11_part2(input)
#   expected <- "20151213744"
#   expect_equal(actual, expected)
# })



