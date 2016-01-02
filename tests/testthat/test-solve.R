context("solve")

# Extract the lights configuration out of a solution in vector form, row-major
lights_vec <- function(solution) {
  as.numeric(t(solution))
}

test_that("is_solved works", {
  expect_true(empty_board(3) %>% is_solved())
  expect_true(empty_board(5) %>% is_solved())
  expect_true(empty_board(3, classic = FALSE) %>% is_solved())
  expect_false(empty_board(3) %>% play(1, 1) %>% is_solved())
  expect_true(empty_board(3) %>% play(1, 1) %>% play(1, 1) %>% is_solved())
})

test_that("is_solvable works", {
  expect_true(empty_board(3) %>% is_solvable())
  expect_true(empty_board(3, classic = FALSE) %>% is_solvable())
  expect_false(
    new_board(c(1, rep(0, 24))) %>% is_solvable()
  )
  expect_true(
    new_board(c(1, 1, 1, rep(0, 22))) %>% is_solvable()
  )
  expect_false(
    new_board(c(1, 1, 1, rep(0, 22)), classic = FALSE) %>% is_solvable()
  )
})

test_that("solve_board works", {
  expect_equal(
    new_board(c(1, rep(0, 8))) %>% solve_board() %>% lights_vec,
    c(1, 0, 1,
      0, 0, 1,
      1, 1, 0)
  )
  expect_error(
    new_board(c(1, rep(0, 8)), classic = FALSE) %>% solve_board(),
    "Board does not have a solution"
  )
  expect_equal(
    new_board(c(
      0, 1, 1,
      0, 1, 1,
      0, 0, 0
    )) %>% solve_board() %>% lights_vec,
    c(0, 1, 1, 1, 1, 1, 0, 1, 0)
  )
  expect_equal(
    new_board(c(
      0, 1, 1,
      0, 1, 1,
      0, 0, 0
    ), classic = FALSE) %>% solve_board() %>% lights_vec,
    c(0, 0, 0, 1, 1, 1, 1, 0, 0)
  )
})

# This isn't exactly a unit test because it tests a few functions together
test_that("solving a random board indeed returns a solved board", {
  board1 <- random_board(3)
  sol1 <-solve_board(board1)
  expect_true(
    board1 %>% play(matrix = sol1) %>% is_solved
  )

  board2 <- random_board(5)
  sol2 <-solve_board(board2)
  expect_true(
    board2 %>% play(matrix = sol2) %>% is_solved
  )

  board3 <- random_board(3, classic = FALSE)
  sol3 <-solve_board(board3)
  expect_true(
    board3 %>% play(matrix = sol3) %>% is_solved
  )

  board4 <- random_board(3, classic = FALSE)
  sol4 <-solve_board(board4)
  expect_true(
    board4 %>% play(matrix = sol4) %>% is_solved
  )
})
