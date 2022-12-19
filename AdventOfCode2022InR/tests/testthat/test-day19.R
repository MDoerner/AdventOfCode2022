

test_that("parse_day19_input works: provided test input", {
  input <- "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"
  actual <- parse_day19_input(input)
  expected <- list(
    "1" = list(
      id = "1",
      ore = c(ore = 4L, clay = 0L, obsidian = 0L),
      clay = c(ore = 2L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 14L, obsidian = 0L),
      geode = c(ore = 2L, clay = 0L, obsidian = 7L)
    ),
    "2" = list(
      id = "2",
      ore = c(ore = 2L, clay = 0L, obsidian = 0L),
      clay = c(ore = 3L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 8L, obsidian = 0L),
      geode = c(ore = 3L, clay = 0L, obsidian = 12L)
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day19_part1 works: provided test input", {
  input <- list(
    "1" = list(
      id = "1",
      ore = c(ore = 4L, clay = 0L, obsidian = 0L),
      clay = c(ore = 2L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 14L, obsidian = 0L),
      geode = c(ore = 2L, clay = 0L, obsidian = 7L)
    ),
    "2" = list(
      id = "2",
      ore = c(ore = 2L, clay = 0L, obsidian = 0L),
      clay = c(ore = 3L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 8L, obsidian = 0L),
      geode = c(ore = 3L, clay = 0L, obsidian = 12L)
    )
  )
  actual <- solve_day19_part1(input)
  expected <- "33"
  expect_equal(actual, expected)
})


test_that("solve_day19_part2 works: provided test input", {
  input <- list(
    "1" = list(
      id = "1",
      ore = c(ore = 4L, clay = 0L, obsidian = 0L),
      clay = c(ore = 2L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 14L, obsidian = 0L),
      geode = c(ore = 2L, clay = 0L, obsidian = 7L)
    ),
    "2" = list(
      id = "2",
      ore = c(ore = 2L, clay = 0L, obsidian = 0L),
      clay = c(ore = 3L, clay = 0L, obsidian = 0L),
      obsidian = c(ore = 3L, clay = 8L, obsidian = 0L),
      geode = c(ore = 3L, clay = 0L, obsidian = 12L)
    )
  )
  actual <- solve_day19_part2(input)
  expected <- as.character(62 * 56)
  expect_equal(actual, expected)
})




test_that("solve_day19_part1 works: real input", {
  input <- parse_day19_input(day19_input)
  actual <- solve_day19_part1(input)
  expected <- "1550"
  expect_equal(actual, expected)
})


# test_that("solve_day19_part2 works: real input", {
#   input <- parse_day19_input(day19_input)
#   actual <- solve_day19_part2(input)
#   expected <- "18630"
#   expect_equal(actual, expected)
# })



