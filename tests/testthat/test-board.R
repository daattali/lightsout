context("board")

test_that("new_board error messages", {
  expect_error(new_board(rep(0, 5)), "square matrix")
  expect_error(new_board(rep(0, 16)), "board dimensions")
  expect_error(new_board(matrix(0, nrow = 2, ncol = 3)), "square matrix")
  expect_error(new_board(matrix(0, nrow = 4, ncol = 4)), "board dimensions")
  expect_error(new_board(c(2, rep(0, 8))), "allowed in the board")
})

test_that("new_board creation works", {
  expect_equal(
    new_board(c(0, 1, 0, 0, 1, 1, 0, 0, 0)),
    new_board(matrix(c(0, 1, 0, 0, 1, 1, 0, 0, 0), nrow = 3, byrow = TRUE))
  )
})

test_that("empty_board creation works", {
  expect_equal(
    empty_board(3),
    new_board(c(rep(0, 9)))
  )
})
