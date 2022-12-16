

test_that("parse_day16_input works: provided test input", {
  input <- "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II"
  actual <- parse_day16_input(input)
  expected <- list(
    "AA" = list(
      name = "AA",
      rate = 0L,
      connections = c("DD", "II", "BB")
    ),
    "BB" = list(
      name = "BB",
      rate = 13L,
      connections = c("CC", "AA")
    ),
    "CC" = list(
      name = "CC",
      rate = 2L,
      connections = c("DD", "BB")
    ),
    "DD" = list(
      name = "DD",
      rate = 20L,
      connections = c("CC", "AA", "EE")
    ),
    "EE" = list(
      name = "EE",
      rate = 3L,
      connections = c("FF", "DD")
    ),
    "FF" = list(
      name = "FF",
      rate = 0L,
      connections = c("EE", "GG")
    ),
    "GG" = list(
      name = "GG",
      rate = 0L,
      connections = c("FF", "HH")
    ),
    "HH" = list(
      name = "HH",
      rate = 22L,
      connections = c("GG")
    ),
    "II" = list(
      name = "II",
      rate = 0L,
      connections = c("AA", "JJ")
    ),
    "JJ" = list(
      name = "JJ",
      rate = 21L,
      connections = c("II")
    )
  )
  expect_equal(actual, expected)
})


test_that("solve_day16_part1 works: provided test input", {
  input <- list(
    "AA" = list(
      name = "AA",
      rate = 0L,
      connections = c("DD", "II", "BB")
    ),
    "BB" = list(
      name = "BB",
      rate = 13L,
      connections = c("CC", "AA")
    ),
    "CC" = list(
      name = "CC",
      rate = 2L,
      connections = c("DD", "BB")
    ),
    "DD" = list(
      name = "DD",
      rate = 20L,
      connections = c("CC", "AA", "EE")
    ),
    "EE" = list(
      name = "EE",
      rate = 3L,
      connections = c("FF", "DD")
    ),
    "FF" = list(
      name = "FF",
      rate = 0L,
      connections = c("EE", "GG")
    ),
    "GG" = list(
      name = "GG",
      rate = 0L,
      connections = c("FF", "HH")
    ),
    "HH" = list(
      name = "HH",
      rate = 22L,
      connections = c("GG")
    ),
    "II" = list(
      name = "II",
      rate = 0L,
      connections = c("AA", "JJ")
    ),
    "JJ" = list(
      name = "JJ",
      rate = 21L,
      connections = c("II")
    )
  )
  actual <- solve_day16_part1(input)
  expected <- "1651"
  expect_equal(actual, expected)
})


test_that("solve_day16_part2 works: provided test input", {
  input <- list(
    "AA" = list(
      name = "AA",
      rate = 0L,
      connections = c("DD", "II", "BB")
    ),
    "BB" = list(
      name = "BB",
      rate = 13L,
      connections = c("CC", "AA")
    ),
    "CC" = list(
      name = "CC",
      rate = 2L,
      connections = c("DD", "BB")
    ),
    "DD" = list(
      name = "DD",
      rate = 20L,
      connections = c("CC", "AA", "EE")
    ),
    "EE" = list(
      name = "EE",
      rate = 3L,
      connections = c("FF", "DD")
    ),
    "FF" = list(
      name = "FF",
      rate = 0L,
      connections = c("EE", "GG")
    ),
    "GG" = list(
      name = "GG",
      rate = 0L,
      connections = c("FF", "HH")
    ),
    "HH" = list(
      name = "HH",
      rate = 22L,
      connections = c("GG")
    ),
    "II" = list(
      name = "II",
      rate = 0L,
      connections = c("AA", "JJ")
    ),
    "JJ" = list(
      name = "JJ",
      rate = 21L,
      connections = c("II")
    )
  )
  actual <- solve_day16_part2(input)
  expected <- "1707"
  expect_equal(actual, expected)
})




# test_that("solve_day16_part1 works: real input", {
#   input <- parse_day16_input(day16_input)
#   actual <- solve_day16_part1(input)
#   expected <- "1915"
#   expect_equal(actual, expected)
# })

#TAKES WAY TOO LONG
# 
# $time_execution
# Time difference of 1.433213 hours
# 
# test_that("solve_day16_part2 works: real input", {
#   input <- parse_day16_input(day16_input)
#   actual <- solve_day16_part2(input)
#   expected <- "2772"
#   expect_equal(actual, expected)
# })



