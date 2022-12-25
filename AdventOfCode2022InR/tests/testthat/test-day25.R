

test_that("parse_day25_input works: provided test input", {
  input <- "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122"
  actual <- parse_day25_input(input)
  expected <- c(
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
  )
  expect_equal(actual, expected)
})

test_that("day25 conversion works: SNAFU to decimal", {
  inputs <- c(
    "1",
    "2",
    "1=",
    "1-",
    "10",
    "11",
    "12",
    "2=",
    "2-",
    "20",
    "1=0",
    "1-0",
    "1=11-2",
    "1-0---0",
    "1121-1110-1=0",
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
  )
  actuals <- purrr::map(
    .x = inputs,
    .f = day25$snafu_to_decimal
  )
  expecteds <- c(
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    15,
    20,
    2022,
    12345,
    314159265,
    1747,
    906,
    198,
    11,
    201,
    31,
    1257,
    32,
    353,
    107,
    7,
    3,
    37
  )
  for (index  in seq_along(actuals)) {
    expect_equal(actuals[[index]], expecteds[[index]])
  }
})

test_that("day25 conversion works: decimal to SNAFU", {
  inputs <- c(
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    15,
    20,
    2022,
    12345,
    314159265,
    1747,
    906,
    198,
    11,
    201,
    31,
    1257,
    32,
    353,
    107,
    7,
    3,
    37
  )
  actuals <- purrr::map(
    .x = inputs,
    .f = day25$decimal_to_snafu
  )
  expecteds <- c(
    "1",
    "2",
    "1=",
    "1-",
    "10",
    "11",
    "12",
    "2=",
    "2-",
    "20",
    "1=0",
    "1-0",
    "1=11-2",
    "1-0---0",
    "1121-1110-1=0",
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
  )
  for (index  in seq_along(actuals)) {
    expect_equal(actuals[[index]], expecteds[[index]])
  }
})


test_that("solve_day25_part1 works: provided test input", {
  input <- c(
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
  )
  actual <- solve_day25_part1(input)
  expected <- "2=-1=0"
  expect_equal(actual, expected)
})




test_that("solve_day25_part1 works: real input", {
  input <- parse_day25_input(day25_input)
  actual <- solve_day25_part1(input)
  expected <- "2=2-1-010==-0-1-=--2"
  expect_equal(actual, expected)
})



