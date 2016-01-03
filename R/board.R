#' Initialize a Lights Out board with a given lights configuration
#'
#' Create a Lights Out board that can be played by the user or solved automatically.
#' Only square boards of size 3x3, 5x5, 7x7, or 9x9 are supported. The initial
#' lights configuration must be provided. To create a board with a random
#' configuration, use the \code{\link[lightsout]{random_board}} function.
#'
#' @param entries The initial configuration of lights on the board. \code{entries}
#' can either be a vector or a matrix. If a vector is used, the vector is assumed
#' to start at the top-left corner of the board and is read row-by-row. Only values
#' of 0 (light off) and 1 (light on) are allowed in the vector or matrix. See
#' the examples below.
#' @param classic If \code{TRUE}, then pressing a light will toggle it and its
#' adjacent neighbours only. If \code{FALSE}, then pressing a light will toggle
#' the entire row and column of the pressed light.
#' @return A \code{lightsout} board object.
#' @seealso \code{\link[lightsout]{random_board}}
#' \code{\link[lightsout]{play}}
#' \code{\link[lightsout]{solve_board}}
#' @examples
#' vector <- c(1, 1, 0,
#'             1, 0, 1,
#'             0, 1, 1)
#' new_board(entries = vector)
#'
#' matrix <- matrix(
#'             c(1, 1, 0,
#'               1, 0, 1,
#'               0, 1, 1),
#'             nrow = 3, byrow = TRUE)
#' new_board(entries = matrix)
#' @export
new_board <- function(entries, classic = TRUE) {
  allowed_sizes <- c(3, 5, 7, 9)

  # If a vector was provided, turn it into a matrix
  if (is.vector(entries)) {
    n <- sqrt(length(entries))
    if (n %% 1 != 0) {
      stop("`entries` cannot be transformed into a square matrix. Make sure the vector length matches a square.",
           call. = FALSE)
    }
    entries <- matrix(entries, ncol = n, nrow = n, byrow = TRUE)
  }

  # Make sure the matrix is square
  if (nrow(entries) != ncol(entries)) {
    stop("The board must be a square matrix",
         call. = FALSE)
  }

  # Make sure the matrix is a valid size
  n <- nrow(entries)
  if (!n %in% allowed_sizes) {
    stop(paste0("Only the following board dimensions are allowed: [",
                paste(allowed_sizes, collapse = ","), "]"),
         call. = FALSE)
  }

  # Make sure all entries are 0 or 1
  if (sum(entries > 1 | entries < 0) > 0) {
    stop("Only values of 0 (light off) and 1 (light on) are allowed in the board",
         call. = FALSE)
  }

  # Make sure the matrix we store is a plain matrix with just the numbers,
  # in case the user passed in a matrix with more junk attached to it
  entries <- matrix(entries, ncol = n, nrow = n)

  # Generate the toggle matrix (used for solving the board)
  toggle_matrix <- generate_lightsout_matrix(n, classic)

  board <- list(
    entries = entries,
    size = n,
    classic = classic,
    toggle_matrix = toggle_matrix
  )

  structure(board, class = "lightsout")
}

#' Print a lightsout board
#' @export
#' @keywords internal
print.lightsout <- function(x, ...) {
  cat("Lights Out ", board_size(x), "x", board_size(x), " board", "\n", sep = "")
  cat("Game mode:", ifelse(board_classic(x), "classic", "entire row/column"), "\n\n\t")
  utils::write.table(board_entries(x), row.names = FALSE, col.names = FALSE, eol = "\n\t")
}

#' Initialize a Lights Out board with all lights switched off
#'
#' @param size Number of rows and columns for the board
#' @inheritParams new_board
#' @return A \code{lightsout} board.
#' @seealso \code{\link[lightsout]{random_board}}
#' \code{\link[lightsout]{new_board}}
#' @examples
#' empty_board(5)
#' @export
empty_board <- function(size, classic = TRUE) {
  new_board(entries = rep(0, size*size), classic = classic)
}

#' Create a random (but solvable) Lights Out board
#'
#' Create a Lights Out board that can be played by the user or solved automatically.
#' Only square boards of size 3x3, 5x5, 7x7, or 9x9 are supported. The initial
#' lights configuration is randomly generated, but always solvable. To create a
#' board with a user-defined configuration, use the \code{\link[lightsout]{new_board}} function.
#' @inheritParams new_board
#' @param size Number of rows and columns for the board
#' @seealso \code{\link[lightsout]{new_board}}
#' \code{\link[lightsout]{play}}
#' \code{\link[lightsout]{solve_board}}
#' @return A \code{lightsout} board object.
#' @examples
#' set.seed(10)
#'
#' # Create a random 5x5 classic board
#' board <- random_board(5)
#' board
#'
#' # Get the solution for the board
#' solution <- solve_board(board)
#' solution
#'
#' # Press the lights according to the solution, the result should be a board
#' # with all lights switched off
#' play(board, matrix = solution)
#' @export
random_board <- function(size, classic = TRUE) {
  # generate a solvable board by starting with an all-off board and pressing
  # somee lights randomly. The number of lights pressed is between 20% to 80% of
  # the number of total lights on the board
  board <- empty_board(size, classic = classic)
  num_plays <- round(stats::runif(1, size*size*0.2, size*size*0.8))
  positions <- sort(sample(size*size, num_plays))
  play_matrix <- matrix(0, ncol = size, nrow = size)
  play_matrix[positions] <- 1
  play_matrix <- t(play_matrix)
  suppressMessages(
    board <- play(board, matrix = play_matrix)
  )
  board
}
