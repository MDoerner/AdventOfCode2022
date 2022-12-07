

test_that("parse_day7_input works: provided test input", {
  input <- "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"
  actual <- parse_day7_input(input)
  expected <- list(
    list(
      command = "cd",
      argument = "/"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "a"
        ),
        list(
          type = "file",
          name = "b.txt",
          size = 14848514
        ),
        list(
          type = "file",
          name = "c.dat",
          size = 8504156
        ),
        list(
          type = "dir",
          name = "d"
        )
      )
    ),
    list(
      command = "cd",
      argument = "a"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "e"
        ),
        list(
          type = "file",
          name = "f",
          size = 29116
        ),
        list(
          type = "file",
          name = "g",
          size = 2557
        ),
        list(
          type = "file",
          name = "h.lst",
          size = 62596
        )
      )
    ),
    list(
      command = "cd",
      argument = "e"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "i",
          size = 584
        )
      )
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = "d"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "j",
          size = 4060174
        ),
        list(
          type = "file",
          name = "d.log",
          size = 8033020
        ),
        list(
          type = "file",
          name = "d.ext",
          size = 5626152
        ),
        list(
          type = "file",
          name = "k",
          size = 7214296
        )
      )
    )
  )
  expect_equal(actual, expected)
})


test_that("day7 directory size works: provided test input", {
  input <- list(
    list(
      command = "cd",
      argument = "/"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "a"
        ),
        list(
          type = "file",
          name = "b.txt",
          size = 14848514
        ),
        list(
          type = "file",
          name = "c.dat",
          size = 8504156
        ),
        list(
          type = "dir",
          name = "d"
        )
      )
    ),
    list(
      command = "cd",
      argument = "a"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "e"
        ),
        list(
          type = "file",
          name = "f",
          size = 29116
        ),
        list(
          type = "file",
          name = "g",
          size = 2557
        ),
        list(
          type = "file",
          name = "h.lst",
          size = 62596
        )
      )
    ),
    list(
      command = "cd",
      argument = "e"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "i",
          size = 584
        )
      )
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = "d"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "j",
          size = 4060174
        ),
        list(
          type = "file",
          name = "d.log",
          size = 8033020
        ),
        list(
          type = "file",
          name = "d.ext",
          size = 5626152
        ),
        list(
          type = "file",
          name = "k",
          size = 7214296
        )
      )
    )
  )
  file_system <- file_system_from_command_line(input)
  
  expect_equal(directory_size(file_system), 48381165)
  expect_equal(directory_size(file_system$d), 24933642)
  expect_equal(directory_size(file_system$a), 94853)
  expect_equal(directory_size(file_system$a$e), 584)
})



test_that("solve_day7_part1 works: provided test input", {
  input <- list(
    list(
      command = "cd",
      argument = "/"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "a"
        ),
        list(
          type = "file",
          name = "b.txt",
          size = 14848514
        ),
        list(
          type = "file",
          name = "c.dat",
          size = 8504156
        ),
        list(
          type = "dir",
          name = "d"
        )
      )
    ),
    list(
      command = "cd",
      argument = "a"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "e"
        ),
        list(
          type = "file",
          name = "f",
          size = 29116
        ),
        list(
          type = "file",
          name = "g",
          size = 2557
        ),
        list(
          type = "file",
          name = "h.lst",
          size = 62596
        )
      )
    ),
    list(
      command = "cd",
      argument = "e"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "i",
          size = 584
        )
      )
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = "d"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "j",
          size = 4060174
        ),
        list(
          type = "file",
          name = "d.log",
          size = 8033020
        ),
        list(
          type = "file",
          name = "d.ext",
          size = 5626152
        ),
        list(
          type = "file",
          name = "k",
          size = 7214296
        )
      )
    )
  )
  actual <- solve_day7_part1(input)
  expected <- "95437"
  expect_equal(actual, expected)
})


test_that("solve_day7_part2 works: provided test input", {
  input <- list(
    list(
      command = "cd",
      argument = "/"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "a"
        ),
        list(
          type = "file",
          name = "b.txt",
          size = 14848514
        ),
        list(
          type = "file",
          name = "c.dat",
          size = 8504156
        ),
        list(
          type = "dir",
          name = "d"
        )
      )
    ),
    list(
      command = "cd",
      argument = "a"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "dir",
          name = "e"
        ),
        list(
          type = "file",
          name = "f",
          size = 29116
        ),
        list(
          type = "file",
          name = "g",
          size = 2557
        ),
        list(
          type = "file",
          name = "h.lst",
          size = 62596
        )
      )
    ),
    list(
      command = "cd",
      argument = "e"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "i",
          size = 584
        )
      )
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = ".."
    ),
    list(
      command = "cd",
      argument = "d"
    ),
    list(
      command = "ls",
      output = list(
        list(
          type = "file",
          name = "j",
          size = 4060174
        ),
        list(
          type = "file",
          name = "d.log",
          size = 8033020
        ),
        list(
          type = "file",
          name = "d.ext",
          size = 5626152
        ),
        list(
          type = "file",
          name = "k",
          size = 7214296
        )
      )
    )
  )
  actual <- solve_day7_part2(input)
  expected <- "24933642"
  expect_equal(actual, expected)
})




test_that("solve_day7_part1 works: real input", {
  input <- parse_day7_input(day7_input)
  actual <- solve_day7_part1(input)
  expected <- "1086293"
  expect_equal(actual, expected)
})


test_that("solve_day7_part2 works: real input", {
  input <- parse_day7_input(day7_input)
  actual <- solve_day7_part2(input)
  expected <- "366028"
  expect_equal(actual, expected)
})



