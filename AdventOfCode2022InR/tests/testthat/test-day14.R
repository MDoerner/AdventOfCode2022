

test_that("parse_segment_specifiers works: provided test input", {
  input <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
  actual <- day14$parse_segment_specifiers(input)
  expected <- list(
    list(
      c(498L, 4L),
      c(498L, 6L),
      c(496L, 6L)
    ),
    list(
      c(503L, 4L),
      c(502L, 4L),
      c(502L, 9L),
      c(494L, 9L)
    )
  )
  expect_equal(actual, expected)
})


test_that("parse_day14_input works: provided test input", {
  input <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
  actual <- for_tests$hashmap$as_list(parse_day14_input(input))
  expected <- list(
    "494" = list(c(9L, 9L)),
    "495" = list(c(9L, 9L)),
    "496" = list(c(6L, 6L), c(9L, 9L)),
    "497" = list(c(6L, 6L), c(9L, 9L)),
    "498" = list(c(4L, 6L), c(6L, 6L), c(9L, 9L)),
    "499" = list(c(9L, 9L)),
    "500" = list(c(9L, 9L)),
    "501" = list(c(9L, 9L)),
    "502" = list(c(4L, 4L), c(4L, 9L), c(9L, 9L)),
    "503" = list(c(4L, 4L))
  )
  expect_equal(actual, expected)
})


test_that("parse_day14_input works: reversed test input", {
  input <- "496,6 -> 498,6 -> 498,4
494,9 -> 502,9 -> 502,4 -> 503,4"
  actual <- for_tests$hashmap$as_list(parse_day14_input(input))
  expected <- list(
    "494" = list(c(9L, 9L)),
    "495" = list(c(9L, 9L)),
    "496" = list(c(6L, 6L), c(9L, 9L)),
    "497" = list(c(6L, 6L), c(9L, 9L)),
    "498" = list(c(4L, 6L), c(6L, 6L), c(9L, 9L)),
    "499" = list(c(9L, 9L)),
    "500" = list(c(9L, 9L)),
    "501" = list(c(9L, 9L)),
    "502" = list(c(4L, 4L), c(4L, 9L), c(9L, 9L)),
    "503" = list(c(4L, 4L))
  )
  expect_equal(actual, expected)
})


test_that("parse_segment_specifiers works: adjusted test input", {
  input <- "498,4 -> 498,6
503,4 -> 502,4 -> 502,9 -> 494,9
498,6 -> 496,6"
  actual <- day14$parse_segment_specifiers(input)
  expected <- list(
    list(
      c(498L, 4L),
      c(498L, 6L)
    ),
    list(
      c(503L, 4L),
      c(502L, 4L),
      c(502L, 9L),
      c(494L, 9L)
    ),
    list(
      c(498L, 6L),
      c(496L, 6L)
    )
  )
  expect_equal(actual, expected)
})


test_that("parse_day14_input works: adjusted test input", {
  input <- "498,4 -> 498,6
503,4 -> 502,4 -> 502,9 -> 494,9
498,6 -> 496,6"
  actual <- for_tests$hashmap$as_list(parse_day14_input(input))
  expected <- list(
    "494" = list(c(9L, 9L)),
    "495" = list(c(9L, 9L)),
    "496" = list(c(6L, 6L), c(9L, 9L)),
    "497" = list(c(6L, 6L), c(9L, 9L)),
    "498" = list(c(4L, 6L), c(6L, 6L), c(9L, 9L)),
    "499" = list(c(9L, 9L)),
    "500" = list(c(9L, 9L)),
    "501" = list(c(9L, 9L)),
    "502" = list(c(4L, 4L), c(4L, 9L), c(9L, 9L)),
    "503" = list(c(4L, 4L))
  )
  expect_equal(actual, expected)
})


test_that("solve_day14_part1 works: provided test input", {
  input_text <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
  input <- parse_day14_input(input_text)
  actual <- solve_day14_part1(input)
  expected <- "24"
  expect_equal(actual, expected)
})



test_that("solve_day14_part2 works: provided test input", {
  input_text <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"
  input <- parse_day14_input(input_text)
  actual <- solve_day14_part2(input)
  expected <- "93"
  expect_equal(actual, expected)
})



test_that("solve_day14_part1 works: real input", {
  input <- parse_day14_input(day14_input)
  actual <- solve_day14_part1(input)
  expected <- "763"
  expect_equal(actual, expected)
})


# TAKES WAY TOO LONG
# test_that("solve_day14_part2 works: real input", {
#   input <- parse_day14_input(day14_input)
#   actual <- solve_day14_part2(input)
#   expected <- "23921"
#   expect_equal(actual, expected)
# })



