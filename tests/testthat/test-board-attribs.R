context("board-attribs")

test_that("board attributes are correct", {
  lights <- c(1, 0, 0, 0, 0, 0, 0, 0, 0)
  board <- new_board(lights, classic = FALSE)
  expect_equal(as.vector(board_entries(board)), lights)
  expect_equal(board_size(board), 3)
  expect_false(board_classic(board))
})
