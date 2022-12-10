

day10_example_input <- "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"


test_that("parse_day10_input works: provided test input", {
  input <- "addx 13
addx 4
noop
addx -1
"
  actual <- parse_day10_input(input)
  expected <- list(
    list(
      type = "addx",
      argument = 13L,
      cycles = 2L
    ),
    list(
      type = "addx",
      argument = 4L,
      cycles = 2L
    ),
    list(
      type = "noop",
      argument = NULL,
      cycles = 1L
    ),
    list(
      type = "addx",
      argument = -1L,
      cycles = 2L
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day10_part1 works: provided test input", {
  input <- parse_day10_input(day10_example_input)
  actual <- solve_day10_part1(input)
  expected <- "13140"
  expect_equal(actual, expected)
})


test_that("solve_day10_part2 works: provided test input", {
  input <- parse_day10_input(day10_example_input)
  actual <- solve_day10_part2(input)
  expected <- "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
  expect_equal(actual, expected)
})




test_that("solve_day10_part1 works: real input", {
  input <- parse_day10_input(day10_input)
  actual <- solve_day10_part1(input)
  expected <- "16880"
  expect_equal(actual, expected)
})


test_that("solve_day10_part2 works: real input", {
  input <- parse_day10_input(day10_input)
  actual <- solve_day10_part2(input)
  expected <- "###..#..#..##..####..##....##.###..###..
#..#.#.#..#..#....#.#..#....#.#..#.#..#.
#..#.##...#..#...#..#..#....#.###..#..#.
###..#.#..####..#...####....#.#..#.###..
#.#..#.#..#..#.#....#..#.#..#.#..#.#.#..
#..#.#..#.#..#.####.#..#..##..###..#..#."
  expect_equal(actual, expected)
})



