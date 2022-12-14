

test_that("parse_day13_input works: provided test input", {
  input <- "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]

[9]
[10]"
  actual <- parse_day13_input(input)
  expected <- list(
    list(
      left = list(1L, 1L, 3L, 1L, 1L),
      right = list(1L, 1L, 5L, 1L, 1L)
    ),
    list(
      left = list(list(1L), list(2L, 3L, 4L)),
      right = list(list(1L), 4L)
    ),
    list(
      left = list(9L),
      right = list(list(8L, 7L, 6L))
    ),
    list(
      left = list(list(4L, 4L), 4L, 4L),
      right = list(list(4L, 4L), 4L, 4L, 4L)
    ),
    list(
      left = list(7L, 7L, 7L, 7L),
      right = list(7L, 7L, 7L)
    ),
    list(
      left = list(),
      right = list(3L)
    ),
    list(
      left = list(list(list())),
      right = list(list())
    ),
    list(
      left = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 7L)))), 8L, 9L),
      right = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 0L)))), 8L, 9L)
    ),
    list(
      left = list(9L),
      right = list(10L)
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day13_part1 works: provided test input", {
  input <- list(
    list(
      left = list(1L, 1L, 3L, 1L, 1L),
      right = list(1L, 1L, 5L, 1L, 1L)
    ),
    list(
      left = list(list(1L), list(2L, 3L, 4L)),
      right = list(list(1L), 4L)
    ),
    list(
      left = list(9L),
      right = list(list(8L, 7L, 6L))
    ),
    list(
      left = list(list(4L, 4L), 4L, 4L),
      right = list(list(4L, 4L), 4L, 4L, 4L)
    ),
    list(
      left = list(7L, 7L, 7L, 7L),
      right = list(7L, 7L, 7L)
    ),
    list(
      left = list(),
      right = list(3L)
    ),
    list(
      left = list(list(list())),
      right = list(list())
    ),
    list(
      left = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 7L)))), 8L, 9L),
      right = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 0L)))), 8L, 9L)
    )
  )
  actual <- solve_day13_part1(input)
  expected <- "13"
  expect_equal(actual, expected)
})


test_that("solve_day13_part2 works: provided test input", {
  input <- list(
    list(
      left = list(1L, 1L, 3L, 1L, 1L),
      right = list(1L, 1L, 5L, 1L, 1L)
    ),
    list(
      left = list(list(1L), list(2L, 3L, 4L)),
      right = list(list(1L), 4L)
    ),
    list(
      left = list(9L),
      right = list(list(8L, 7L, 6L))
    ),
    list(
      left = list(list(4L, 4L), 4L, 4L),
      right = list(list(4L, 4L), 4L, 4L, 4L)
    ),
    list(
      left = list(7L, 7L, 7L, 7L),
      right = list(7L, 7L, 7L)
    ),
    list(
      left = list(),
      right = list(3L)
    ),
    list(
      left = list(list(list())),
      right = list(list())
    ),
    list(
      left = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 7L)))), 8L, 9L),
      right = list(1L, list(2L, list(3L, list(4L, list(5L, 6L, 0L)))), 8L, 9L)
    )
  )
  actual <- solve_day13_part2(input)
  expected <- "140"
  expect_equal(actual, expected)
})




test_that("solve_day13_part1 works: real input", {
  input <- parse_day13_input(day13_input)
  actual <- solve_day13_part1(input)
  expected <- "5852"
  expect_equal(actual, expected)
})


test_that("solve_day13_part2 works: real input", {
  input <- parse_day13_input(day13_input)
  actual <- solve_day13_part2(input)
  expected <- "24190"
  expect_equal(actual, expected)
})




