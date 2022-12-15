

test_that("parse_day15_input works: provided test input", {
  input <- "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  actual <- parse_day15_input(input)
  expected <- list(
    test_row = 2000000L,
    coord_limit = 4000000L,
    sensors = list(
      list(
        position = c(2L, 18L),
        closest_beacon = c(-2L, 15L)
      ),
      list(
        position = c(9L, 16L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(13L, 2L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(12L, 14L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(10L, 20L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(14L, 17L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(8L, 7L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(2L, 0L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(0L, 11L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(20L, 14L),
        closest_beacon = c(25L, 17L)
      ),
      list(
        position = c(17L, 20L),
        closest_beacon = c(21L, 22L)
      ),
      list(
        position = c(16L, 7L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(14L, 3L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(20L, 1L),
        closest_beacon = c(15L, 3L)
      )
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day15_part1 works: provided test input", {
  input <- list(
    test_row = 10L,
    coord_limit = 20L,
    sensors = list(
      list(
        position = c(2L, 18L),
        closest_beacon = c(-2L, 15L)
      ),
      list(
        position = c(9L, 16L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(13L, 2L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(12L, 14L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(10L, 20L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(14L, 17L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(8L, 7L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(2L, 0L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(0L, 11L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(20L, 14L),
        closest_beacon = c(25L, 17L)
      ),
      list(
        position = c(17L, 20L),
        closest_beacon = c(21L, 22L)
      ),
      list(
        position = c(16L, 7L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(14L, 3L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(20L, 1L),
        closest_beacon = c(15L, 3L)
      )
    )
  )
  actual <- solve_day15_part1(input)
  expected <- "26"
  expect_equal(actual, expected)
})


test_that("solve_day15_part2 works: provided test input", {
  input <- list(
    test_row = 10L,
    coord_limit = 20L,
    sensors = list(
      list(
        position = c(2L, 18L),
        closest_beacon = c(-2L, 15L)
      ),
      list(
        position = c(9L, 16L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(13L, 2L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(12L, 14L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(10L, 20L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(14L, 17L),
        closest_beacon = c(10L, 16L)
      ),
      list(
        position = c(8L, 7L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(2L, 0L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(0L, 11L),
        closest_beacon = c(2L, 10L)
      ),
      list(
        position = c(20L, 14L),
        closest_beacon = c(25L, 17L)
      ),
      list(
        position = c(17L, 20L),
        closest_beacon = c(21L, 22L)
      ),
      list(
        position = c(16L, 7L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(14L, 3L),
        closest_beacon = c(15L, 3L)
      ),
      list(
        position = c(20L, 1L),
        closest_beacon = c(15L, 3L)
      )
    )
  )
  actual <- solve_day15_part2(input)
  expected <- "56000011"
  expect_equal(actual, expected)
})




test_that("solve_day15_part1 works: real input", {
  input <- parse_day15_input(day15_input)
  actual <- solve_day15_part1(input)
  expected <- "5688618"
  expect_equal(actual, expected)
})


test_that("solve_day15_part2 works: real input", {
  input <- parse_day15_input(day15_input)
  actual <- solve_day15_part2(input)
  expected <- "12625383204261"
  expect_equal(actual, expected)
})



