#' Solve a Lights Out board
#'
#' Given a Lights Out board, find the set of lights that need to be pressed in
#' order to solve the board. If no solution is possible, an error is thrown.
#'
#' There are a few algorithms for solving Lights Out puzzles. This function
#' implements the Gaussian Elimination technique, which does not guarantee
#' the minimum number of steps. Therefore, some steps in the given solution
#' may be redundant.
#'
#' If you are interested, there are many resources online outlining the exact
#' details of how this technique works, and what the other solving strategies are.
#'
#' @param board A \code{lightsout} board object.
#' @return A matrix with the same dimensions as the input board, with a \code{1}
#' in every position that requires a press to solve to the board. Note that the
#' order of the light presses does not matter.
#' @seealso \code{\link[lightsout]{new_board}}
#' \code{\link[lightsout]{random_board}}
#' \code{\link[lightsout]{play}}
#' \code{\link[lightsout]{is_solvable}}
#' \code{\link[lightsout]{is_solved}}
#' @examples
#' # Create an empty 5x5 board, press two lights, and then see that the solution
#' # tells us to press the same lights in order to solve the board.
#' board <- empty_board(5) %>% play(3, 2) %>% play(4, 1)
#' board
#' solution <- solve_board(board)
#' solution
#' board <- play(board, matrix = solution)
#' is_solved(board)
#' @export
solve_board <- function(board) {
  if (!is_solvable(board)) {
    stop("Board does not have a solution", call. = FALSE)
  }

  answer <- solve_helper(board)
  answer
}

#' Is a given Lights Out board solvable?
#'
#' Not every Lights Out configuration has a solution (this has been mathematically
#' proven). This function determines whether a given board has a solution or not.
#' @param board A \code{lightsout} board
#' @return \code{TRUE} if the given board has a solution; \code{FALSE} otherwise.
#' @examples
#' # The following board is solvable using the classic mode (only adjacent lights
#' # are toggled), but has no solution in the variant mode.
#' lights <- c(1, 1, 0,
#'             1, 0, 0,
#'             0, 0, 0 )
#' board_classic <- new_board(lights)
#' board_variant <- new_board(lights, classic = FALSE)
#' is_solvable(board_classic)
#' is_solvable(board_variant)
#' @seealso \code{\link[lightsout]{is_solved}}
#' \code{\link[lightsout]{solve_board}}
#' @export
is_solvable <- function(board) {
  # To determine if the board has a solution, run the algorithm to solve the board.
  # The alogrithm always returns an answer, so just check the answer and see
  # if it indeed solves the board or not.
  answer <- solve_helper(board)
  suppressMessages(
    board <- play(board, matrix = answer)
  )
  is_solved(board)
}

#' Is the given board is a solved state?
#'
#' A board is considered solved if all the lights are switched off (have a state of \code{0}).
#' @param board A \code{lightsout} board
#' @return \code{TRUE} if the given board is solved; \code{FALSE} otherwise.
#' @examples
#' # Create a board solved with one move and solve it.
#' lights <- c(1, 1, 0,
#'             1, 0, 0,
#'             0, 0, 0 )
#' board <- new_board(lights)
#' is_solved(board)
#' board <- board %>% play(1, 1)
#' is_solved(board)
#' @seealso \code{\link[lightsout]{is_solvable}}
#' \code{\link[lightsout]{solve_board}}
#' @export
is_solved <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  sum(as.numeric(board_entries(board))) == 0
}

# The actual solving algorithm
# Performs Gaussian elimination in modulus 2 using the equation
# Ax = b, where b is the current board configuration, A is the toggle matrix
# (the toggle matrix is dependent only on the board size and game mode), and x
# is the solution set of lights that need to be pressed.
solve_helper <- function(board) {
  stopifnot(inherits(board, "lightsout"))

  nrows_board <- board_size(board)
  nrows <- nrows_board * nrows_board
  lightsout_mat <- board_toggle_matrix(board)
  entries <- board_entries(board)

  # Change the board matrix into a column vector
  entries <- t(entries)
  dim(entries) <- c(nrows, 1)

  augmented <- cbind(lightsout_mat, entries)

  # Perform Gaussian elimination
  # This code is highly optimized and vectorized to work in modulus 2 only, so
  # it doesn't look much like what you'd expect row reduction code to look like
  for (row in seq(2, nrows)) {
    nonzero_idx <- which(augmented[row:nrows, row - 1] == 1) + (row - 1)
    num_nonzero <- length(nonzero_idx)

    if (num_nonzero > 0) {
      augmented[nonzero_idx, ] <- ((augmented[nonzero_idx, ] - augmented[rep(row - 1, num_nonzero), ]) %% 2)
    }

    # Move all the zero-rows to the bottom
    if (augmented[row, row] == 0) {
      zero_rows <- which(augmented[row:nrows,row] == 1)
      if (length(zero_rows) > 0) {
        zero_rows <- seq(row, row + zero_rows[1] - 2)
        augmented <- rbind(augmented[-zero_rows, ], augmented[zero_rows, ])
      }
    }
  }

  # The augmented matrix is now in row echelon form
  # We just need to perform back-substitution to get the reduced row echelon form
  for (col in seq(nrows, 2)) {
    nonzero_idx <- which(augmented[1:(col - 1), col] == 1)
    num_nonzero <- length(nonzero_idx)
    if (num_nonzero > 0) {
      augmented[nonzero_idx, ] <- ((augmented[nonzero_idx, ] - augmented[rep(col, num_nonzero), ]) %% 2)
    }
  }


  # The last column of the augmented matrix is our answer vector, so take it
  # and make it into a nxn matrix
  answer <- augmented[, ncol(augmented)]
  answer <- matrix(answer, nrow = nrows_board, byrow = TRUE)
  structure(answer, class = "lightsout_solution")
}

#' Print a lightsout board solution
#' @export
#' @keywords internal
print.lightsout_solution <- function(x, ...) {
  cat("\n\t")
  utils::write.table(x, row.names = FALSE, col.names = FALSE, eol = "\n\t")
}
