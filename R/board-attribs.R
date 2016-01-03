#' Get the board entries (configuration of the lights)
#'
#' @param board A \code{lightsout} board object
#' @return A matrix representing the current state of the lights (0 for off,
#' 1 for on) in the board
#' @examples
#' board <- random_board(5)
#' board
#' board_entries(board)
#' @export
board_entries <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['entries']]
}
#' Set the board entries
#'
#' @param board A \code{lightsout} board object
#' @param value The new value to set the entries to
#' @export
#' @keywords internal
`board_entries<-` <- function(board, value) {
  stopifnot(inherits(board, "lightsout"))
  board[['entries']] <- value
  board
}

#' Get the board size (number of rows/columns)
#'
#' @param board A \code{lightsout} board object
#' @export
#' @keywords internal
board_size <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['size']]
}

#' Is the board using classic game mode?
#'
#' In classic mode, pressing a light will toggle it and its adjacent neighbours only.
#' If non-classic mode, pressing a light will toggle the entire row and column of the pressed light.
#'
#' @param board A \code{lightsout} board object
#' @export
#' @keywords internal
board_classic <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['classic']]
}

#' Get the toggle matrix used to solve the board using linear algebra
#'
#' A lightsout game can be solved using Gaussian elimination. A special matrix
#' \code{A} needs to be used to solve \code{Ax=b}, where \code{b} is the current
#' board configuration and \code{x} is the solution. This function returns
#' the matrix \code{A}.
#' @param board A \code{lightsout} board object
#' @export
#' @keywords internal
board_toggle_matrix <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['toggle_matrix']]
}
