

test_that("parse_day21_input works: provided test input", {
  input <- "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
  actual <- parse_day21_input(input)
  expected <- list(
    numbers = list(
      list(
        id = "dbpl",
        type = "number",
        value = 5L
      ),
      list(
        id = "zczc",
        type = "number",
        value = 2L
      ),
      list(
        id = "dvpt",
        type = "number",
        value = 3L
      ),
      list(
        id = "lfqf",
        type = "number",
        value = 4L
      ),
      list(
        id = "humn",
        type = "number",
        value = 5L
      ),
      list(
        id = "ljgn",
        type = "number",
        value = 2L
      ),
      list(
        id = "sllz",
        type = "number",
        value = 4L
      ),
      list(
        id = "hmdt",
        type = "number",
        value = 32L
      )
    ),
    operations = list(
      list(
        id = "root",
        type = "operation",
        operator = "+",
        left = "pppw",
        right = "sjmn"
      ),
      list(
        id = "cczh",
        type = "operation",
        operator = "+",
        left = "sllz",
        right = "lgvd"
      ),
      list(
        id = "ptdq",
        type = "operation",
        operator = "-",
        left = "humn",
        right = "dvpt"
      ),
      list(
        id = "sjmn",
        type = "operation",
        operator = "*",
        left = "drzm",
        right = "dbpl"
      ),
      list(
        id = "pppw",
        type = "operation",
        operator = "/",
        left = "cczh",
        right = "lfqf"
      ),
      list(
        id = "lgvd",
        type = "operation",
        operator = "*",
        left = "ljgn",
        right = "ptdq"
      ),
      list(
        id = "drzm",
        type = "operation",
        operator = "-",
        left = "hmdt",
        right = "zczc"
      )
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day21_part1 works: provided test input", {
  input_text <- "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
  input <- parse_day21_input(input_text)
  actual <- solve_day21_part1(input)
  expected <- "152"
  expect_equal(actual, expected)
})


test_that("solve_day21_part2 works: provided test input", {
  input_text <- "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"
  input <- parse_day21_input(input_text)
  actual <- solve_day21_part2(input)
  expected <- "301"
  expect_equal(actual, expected)
})




test_that("solve_day21_part1 works: real input", {
  input <- parse_day21_input(day21_input)
  actual <- solve_day21_part1(input)
  expected <- "331120084396440"
  expect_equal(actual, expected)
})


test_that("solve_day21_part2 works: real input", {
  input <- parse_day21_input(day21_input)
  actual <- solve_day21_part2(input)
  expected <- "3378273370680"
  expect_equal(actual, expected)
})



