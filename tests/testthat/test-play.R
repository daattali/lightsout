context("play")

# Extract the lights configuration out of a board in vector form, row-major
lights_vec <- function(board) {
  as.numeric(t(board_entries(board)))
}

test_that("play throws the right errors", {
  expect_error(
    play(matrix(0, nrow = 3, ncol = 3)),
    "inherits"
  )

  expect_error(
    empty_board(3) %>% play(matrix = rep(0, 9)),
    "dimensions"
  )

  expect_error(
    empty_board(3) %>% play(matrix = matrix(0, ncol = 2, nrow = 2)),
    "dimensions"
  )

  expect_error(
    empty_board(3) %>% play(c(1, 1), c(1, 2, 3)),
    "same length"
  )

  expect_error(
    empty_board(3) %>% play(0, 3),
    "board dimensions"
  )

  expect_error(
    empty_board(3) %>% play(4, 3),
    "board dimensions"
  )

  expect_error(
    empty_board(3) %>% play(1, 0),
    "board dimensions"
  )

  expect_error(
    empty_board(3) %>% play(1, 4),
    "board dimensions"
  )

  expect_error(
    empty_board(3) %>% play(c(1, 0, 1), c(1, 2, 3)),
    "board dimensions"
  )

  expect_error(
    empty_board(3) %>% play(c(1, 4, 1), c(1, 2, 3)),
    "board dimensions"
  )
})

test_that("play works in classic mode", {
  expect_equal(
    empty_board(5) %>% play(1, 1) %>% lights_vec,
    c(1, 1, 0, 0, 0,
      1, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0)
  )

  expect_equal(
    empty_board(5) %>% play(2, 4) %>% lights_vec,
    c(0, 0, 0, 1, 0,
      0, 0, 1, 1, 1,
      0, 0, 0, 1, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0)
  )

  expect_equal(
    empty_board(5) %>% play(5, 5) %>% lights_vec,
    c(0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 1,
      0, 0, 0, 1, 1)
  )

  expect_equal(
    empty_board(5) %>% play(5, 5) %>% lights_vec,
    c(0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 0,
      0, 0, 0, 0, 1,
      0, 0, 0, 1, 1)
  )
})

test_that("play works in variant mode", {
  expect_equal(
    empty_board(5, classic = FALSE) %>% play(1, 1) %>% lights_vec,
    c(1, 1, 1, 1, 1,
      1, 0, 0, 0, 0,
      1, 0, 0, 0, 0,
      1, 0, 0, 0, 0,
      1, 0, 0, 0, 0)
  )

  expect_equal(
    empty_board(5, classic = FALSE) %>% play(2, 4) %>% lights_vec,
    c(0, 0, 0, 1, 0,
      1, 1, 1, 1, 1,
      0, 0, 0, 1, 0,
      0, 0, 0, 1, 0,
      0, 0, 0, 1, 0)
  )

  expect_equal(
    empty_board(5, classic = FALSE) %>% play(5, 5) %>% lights_vec,
    c(0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      1, 1, 1, 1, 1)
  )

  expect_equal(
    empty_board(5, classic = FALSE) %>% play(5, 5) %>% lights_vec,
    c(0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      0, 0, 0, 0, 1,
      1, 1, 1, 1, 1)
  )
})

test_that("play changes 0s to 1s and 1s to 0s", {
  expect_equal(
    new_board(c(0, 1, 0,
                0, 0, 1,
                0, 0, 0)) %>%
      play(2, 2) %>% lights_vec,
    c(0, 0, 0, 1, 1, 0, 0, 1, 0)
  )
})

test_that("play can be chained", {
  expect_equal(
    empty_board(5) %>% play(2, 4) %>% play(3, 3) %>% lights_vec,
    c(0, 0, 0, 1, 0,
      0, 0, 0, 1, 1,
      0, 1, 1, 0, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0)
  )
})

test_that("play works with vectors", {
  expect_equal(
    empty_board(5) %>% play(c(2, 3), c(4, 3)) %>% lights_vec,
    c(0, 0, 0, 1, 0,
      0, 0, 0, 1, 1,
      0, 1, 1, 0, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0)
  )
})

test_that("play works with a matrix", {
  expect_equal(
    empty_board(5) %>%
      play(matrix = matrix(c(
        0, 0, 0, 0, 0,
        0, 0, 0, 1, 0,
        0, 0, 1, 0, 0,
        0, 0, 0, 0, 0,
        0, 0, 0, 0, 0
      ), nrow = 5, byrow = TRUE)) %>% lights_vec,
    c(0, 0, 0, 1, 0,
      0, 0, 0, 1, 1,
      0, 1, 1, 0, 0,
      0, 0, 1, 0, 0,
      0, 0, 0, 0, 0)
  )
})
