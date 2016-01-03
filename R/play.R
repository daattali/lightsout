#' Play (press) a single light or multiple lights on a board
#'
#' In classic mode, pressing a light will toggle it and its four adjacent lights.
#' In variant mode, pressing a light will toggle it and all other lights in its
#' row and column.  Toggling a light means switching it from on to off or from
#' off to on.
#'
#' @param board A \code{lightsout} board
#' @param row The row of the light to press. To press multiple lights, use a list
#' of row numbers. If a list is provided, then the \code{col} argument must also
#' be a list of the same length.
#' @param col The column of the light to press. To press multiple lights, use a list
#' of column numbers. If a list is provided, then the \code{row} argument must also
#' be a list of the same length.
#' @param matrix Instead of using \code{row} and \code{col}, a matrix can be used
#' to specify which lights to press. The matrix must have the same dimensions as
#' the board. Any position in the given matrix with a value of \code{1} will
#' result in a press of a light in the same position in the board.
#' @return A new \code{lightsout} board object after the given lights are pressed.
#' @seealso \code{\link[lightsout]{solve_board}}
#' \code{\link[lightsout]{empty_board}}
#' \code{\link[lightsout]{new_board}}
#' \code{\link[lightsout]{random_board}}
#' @examples
#' # Create a 5x5 board with all lights switched off and then press some lights
#'
#' board <- empty_board(5)
#' board
#'
#' # Press the light at (2,1)
#' newboard <- play(board, 2, 1)
#' newboard
#'
#' # Press the light at (2,1) and then at (3,4)
#' newboard <- board %>% play(2, 1) %>% play(3, 4)
#' newboard
#'
#' # Press both lights with one call
#' newboard <- play(board, c(2, 3), c(1, 4))
#' newboard
#'
#' # Press both lights using a matrix instead of specifying rows and columns
#' newboard <- play(board, matrix = matrix(
#'                           c(0, 0, 0, 0, 0,
#'                             1, 0, 0, 0, 0,
#'                             0, 0, 0, 1, 0,
#'                             0, 0, 0, 0, 0,
#'                             0, 0, 0, 0, 0),
#'                           nrow = 5, byrow = TRUE))
#' newboard
#'
#' # Press the same lights, but this time when the game mode is not classic,
#' # and the whole row/column get toggled
#' empty_board(5, classic = FALSE) %>% play(2, 1)
#' empty_board(5, classic = FALSE) %>% play(c(2, 3), c(1, 4))
#' @export
play <- function(board, row, col, matrix) {
  stopifnot(inherits(board, "lightsout"))

  size <- board_size(board)

  # If a matrix is given, make sure the matrix is valid, and play each position
  if (missing(row) && missing(col)) {
    if (!is.matrix(matrix) || length(matrix) != length(board_entries(board))) {
      stop("The matrix of lights to press must have the same dimensions as the board",
           call. = FALSE)
    }

    toggle_pos <- which(matrix == 1)

    for(pos1 in toggle_pos) {
      pos2 <- position_1d_to_2d(pos1, size, byrow = FALSE)
      board <- play_helper(board, row = pos2[1], col = pos2[2])
    }
  } else {
    if (length(row) != length(col)) {
      stop("The row and column vectors are not the same length",
           call. = FALSE)
    }
    if (sum(row > size | row < 1 | col > size | col < 1) > 0) {
      stop(paste0("All rows and columns provided must be within the board dimensions ",
                  "(1 to ", size, ")"),
           call. = FALSE)
    }

    for (i in seq_along(row)) {
      board <- play_helper(board, row = row[i], col = col[i])
    }
  }

  if (is_solved(board)) {
    message("Good job, you won!")
    invisible(board)
  } else {
    board
  }
}

play_helper <- function(board, row, col) {
  size <- board_size(board)
  classic <- board_classic(board)
  entries <- board_entries(board)

  if (classic) {
    entries[row, col] <- 1 - entries[row, col]
    if (row > 1)    entries[row - 1, col] <- 1 - entries[row - 1, col]
    if (row < size) entries[row + 1, col] <- 1 - entries[row + 1, col]
    if (col > 1)    entries[row, col - 1] <- 1 - entries[row, col - 1]
    if (col < size) entries[row, col + 1] <- 1 - entries[row, col + 1]
  } else {
    entries[row, col] <- 1 - entries[row, col]
    entries[row, ]    <- 1 - entries[row, ]
    entries[ , col]   <- 1 - entries[ , col]
  }

  board_entries(board) <- entries

  board
}
