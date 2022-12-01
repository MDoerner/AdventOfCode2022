
# SPLIT LINES

test_that("Split lines works: single line", {
  input <- "Hello"
  actual <- split_lines(input)
  expected <- "Hello"
  expect_equal(actual, expected)
})

test_that("Split lines works: multiple lines", {
  input <- 
"Hello
bla
blub blub"
  actual <- split_lines(input)
  expected <- c("Hello", "bla", "blub blub")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty vector", {
  input <- c()
  actual <- split_lines(input)
  expected <- NULL
  expect_equal(actual, expected)
})

test_that("Split lines works: empty character vector", {
  input <- character()
  actual <- split_lines(input)
  expected <- NULL
  expect_equal(actual, expected)
})

test_that("Split lines works: one empty line, remove_empty = TRUE", {
  input <- ""
  actual <- split_lines(input, remove_empty = TRUE)
  expected <- character()
  expect_equal(actual, expected)
})

test_that("Split lines works: one empty line, remove_empty = default", {
  input <- ""
  actual <- split_lines(input)
  expected <- character()
  expect_equal(actual, expected)
})

test_that("Split lines works: one empty line, remove_empty = FALSE", {
  input <- ""
  actual <- split_lines(input, remove_empty = FALSE)
  expected <- ""
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line in between, remove_empty = TRUE", {
  input <- 
"Hello


There "
  actual <- split_lines(input, remove_empty = TRUE)
  expected <- c("Hello", "There ")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line in between, remove_empty = default", {
  input <- 
"Hello


There "
  actual <- split_lines(input)
  expected <- c("Hello", "There ")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line in between, remove_empty = FALSE", {
  input <- 
"Hello


There "
  actual <- split_lines(input, remove_empty = FALSE)
  expected <- c("Hello", "", "", "There ")
  expect_equal(actual, expected)
})



test_that("Split lines works: empty line at end, remove_empty = TRUE", {
  input <- 
"Hello
There

"
  actual <- split_lines(input, remove_empty = TRUE)
  expected <- c("Hello", "There")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line at end, remove_empty = default", {
  input <- 
    "Hello
There

"
  actual <- split_lines(input)
  expected <- c("Hello", "There")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line at end, remove_empty = FALSE", {
  input <- 
"Hello
There

"
  actual <- split_lines(input, remove_empty = FALSE)
  expected <- c("Hello", "There", "", "")
  expect_equal(actual, expected)
})




test_that("Split lines works: empty line at start, remove_empty = TRUE", {
  input <- 
"

Hello
There "
  actual <- split_lines(input, remove_empty = TRUE)
  expected <- c("Hello", "There ")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line at start, remove_empty = default", {
  input <- 
"

Hello
There "
  actual <- split_lines(input)
  expected <- c("Hello", "There ")
  expect_equal(actual, expected)
})

test_that("Split lines works: empty line at start, remove_empty = FALSE", {
  input <- 
"

Hello
There "
  actual <- split_lines(input, remove_empty = FALSE)
  expected <- c("", "", "Hello", "There ")
  expect_equal(actual, expected)
})



# SPLIT BLOCKS

test_that("Split line separated blocks works: single line", {
  input <- "Hello"
  actual <- split_line_separated_blocks(input)
  expected <- "Hello"
  expect_equal(actual, expected)
})

test_that("Split line separated blocks works: one block", {
  input <- "Hello
There"
  actual <- split_line_separated_blocks(input)
  expected <- "Hello
There"
  expect_equal(actual, expected)
})

test_that("Split line separated blocks works: multiple blocks", {
  input <- "Hello
There

bla"
  actual <- split_line_separated_blocks(input)
  expected <- c(
"Hello
There",
"bla"
  )
  expect_equal(actual, expected)
})

test_that("Split line separated blocks leaves empty line at start", {
  input <- "
Hello
There

bla"
  actual <- split_line_separated_blocks(input)
  expected <- c(
    "
Hello
There",
"bla"
  )
  expect_equal(actual, expected)
})

test_that("Split line separated blocks leaves empty line at end", {
  input <- "Hello
There

bla
"
  actual <- split_line_separated_blocks(input)
  expected <- c(
    "Hello
There",
"bla
"
  )
  expect_equal(actual, expected)
})

test_that("Split line separated blocks swallows additional inner empty lines", {
  input <- "Hello
There


bla




bbb"
  actual <- split_line_separated_blocks(input)
  expected <- c(
    "Hello
There",
"bla",
"bbb"
  )
  expect_equal(actual, expected)
})







