# http://people.math.sfu.ca/~jtmulhol/math302/notes/24-Lights-Out.pdf

#' @export
new_board <- function(entries, classic = TRUE) {
  allowed_sizes <- c(3, 5, 7, 9)

  # If a vector was provided, turn it into a matrix
  if (is.vector(entries)) {
    n <- sqrt(length(entries))
    if (n %% 1 != 0) {
      stop("`entries` cannot be transformed into a square matrix",
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

board_entries <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['entries']]
}
`board_entries<-` <- function(board, value) {
  stopifnot(inherits(board, "lightsout"))
  board[['entries']] <- value
  board
}

board_size <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['size']]
}

board_classic <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['classic']]
}

board_toggle_matrix <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['toggle_matrix']]
}

print.lightsout <- function(x, ...) {
  cat("Lights Out ", board_size(x), "x", board_size(x), " board", "\n", sep = "")
  cat("Game mode:", ifelse(board_classic(x), "classic", "entire row/column"), "\n\n\t")
  write.table(board_entries(x), row.names = FALSE, col.names = FALSE, eol = "\n\t")
}

empty_board <- function(size, classic = TRUE) {
  new_board(entries = rep(0, size*size), classic = classic)
}

board_solved <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  sum(as.numeric(board_entries(board))) == 0
}

# Given a board and a position, simulate a click in that location
play <- function(board, row, col, matrix) {
  stopifnot(inherits(board, "lightsout"))

  if (missing(row) && missing(col)) {
    size <- board_size(board)
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
    for (i in seq_along(row)) {
      board <- play_helper(board, row = row[i], col = col[i])
    }
  }

  board
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

#' @export
random_board <- function(size, classic = TRUE) {
  board <- empty_board(size, classic = classic)
  num_plays <- round(runif(1, size*size*0.2, size*size*0.8))
  positions <- sort(sample(size*size, num_plays))
  play_matrix <- matrix(0, ncol = size, nrow = size)
  play_matrix[positions] <- 1
  play_matrix <- t(play_matrix)
  board <- play(board, matrix = play_matrix)
  board
}

position_1d_to_2d <- function(pos, size, byrow = TRUE) {
  row <- floor((pos - 1) / size) + 1
  col <- pos - ((row - 1) * size)
  if (byrow) {
    c(row, col)
  } else {
    c(col, row)
  }
}


# Create the lightsout matrix used to solve the board
generate_lightsout_matrix <- function(size, classic = TRUE) {
  mat <- matrix(0, ncol = size*size, nrow = size*size)

  for (i in seq(size)) {
    for (j in seq(size)) {
      row <- size * (i - 1) + j
      mat[row, row] <- 1
      if (classic) {
        if (i > 1)    mat[row, row - size] <- 1
        if (i < size) mat[row, row + size] <- 1
        if (j > 1)    mat[row, row - 1] <- 1
        if (j < size) mat[row, row + 1] <- 1
      } else {
        mat[row, size * (i - 1) + seq(size)] <- 1
        mat[row, size * seq(size - 1) + (j - 1) + 1] <- 1
      }
    }
  }

  mat
}

is_solvable <- function(board) {
  size <- board_size(board)
  answer <- solve_helper(board)
  board <- play(board, matrix = answer)

  board_solved(board)
}

#' Solve a lightsout board
#'
#' Given a 5x5 lightsout game board, find the minimum number of clicks
#' to solve the board.
#'
#' The input must be a 5x5 matrix of 0s and 1s.
#'
#' The return value is a matrix with the same dimensions as the game board, with
#' a 1 in every position that requires a click. The order of clicks doesn't matter.
#'
#' Note that a solution will ALWAYS be given, even if the board is not actually
#' solveable. Therefore, you should check the solution by "clicking" all the 1s
#' in the answer and see if the board actually gets solved. If not, then that means
#' there is no solution to the given board.
#' @export
solve_board <- function(board) {
  if (!is_solvable(board)) {
    stop("Board does not have a solution", call. = FALSE)
  }

  answer <- solve_helper(board)
  answer
}

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

print.lightsout_solution <- function(x, ...) {
  cat("\n\t")
  write.table(x, row.names = FALSE, col.names = FALSE, eol = "\n\t")
}
