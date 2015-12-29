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

  board <- list(
    entries = entries,
    size = n,
    classic = classic
  )

  structure(board, class = "lightsout")
}

board_entries <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['entries']]
}

board_size <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['size']]
}

board_classic <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  board[['classic']]
}

print.lightsout <- function(x, ...) {
  cat("Lights Out ", board_size(x), "x", board_size(x), " board", "\n", sep = "")
  cat("Game mode:", ifelse(board_classic(x), "classic", "entire row/column"), "\n\n")
  write.table(board_entries(x), row.names = FALSE, col.names = FALSE)
}

empty_board <- function(size, classic = TRUE) {
  new_board(entries = rep(0, size*size), classic = classic)
}

board_solved <- function(board) {
  stopifnot(inherits(board, "lightsout"))
  sum(as.numeric(board_entries(board))) == 0
}

# Given a board and a position, simulate a click in that location
play <- function(board, row, column) {
  stopifnot(inherits(board, "lightsout"))

  size <- board_size(board)
  classic <- board_classic(board)
  entries <- board_entries(board)

  if (classic) {
    entries[row, column] <- 1 - entries[row, column]
    if (row > 1)            entries[row - 1, column] <- 1 - entries[row - 1, column]
    if (row < size)         entries[row + 1, column] <- 1 - entries[row + 1, column]
    if (column > 1)         entries[row, column - 1] <- 1 - entries[row, column - 1]
    if (column < size)      entries[row, column + 1] <- 1 - entries[row, column + 1]
  } else {
    entries[row, column] <- 1 - entries[row, column]
    entries[row, ] <- 1 - entries[row, ]
    entries[ , column] <- 1 - entries[ , column]
  }

  board[['entries']] <- entries

  board
}

random_board <- function(size, classic = TRUE) {
  board <- empty_board(size, classic = classic)
  num_plays <- round(runif(1, size*size*0.2, size*size*0.8))
  positions_1d <- sort(sample(size*size, num_plays))
  for (p1 in positions_1d) {
    p2 <- position_1d_to_2d(p1, size)
    board <- play(board, row = p2[1], col = p2[2])
  }

  board
}

position_1d_to_2d <- function(pos, size) {
  row <- floor((pos - 1) / size) + 1
  col <- pos - ((row - 1) * size)
  c(row, col)
}

# Create the lightsout matrix used to solve the board
generate_lightsout_matrix <- function(size, full = FALSE) {
  mat <- matrix(0, ncol = size*size, nrow = size*size)

  for (i in seq(size)) {
    for (j in seq(size)) {
      row <- size * (i - 1) + j
      mat[row, row] <- 1
      if (full) {
        mat[row, size * (i - 1) + seq(size)] <- 1
        mat[row, size * seq(size - 1) + (j - 1) + 1] <- 1
      } else {
        if (i > 1)    mat[row, row - size] <- 1
        if (i < size) mat[row, row + size] <- 1
        if (j > 1)    mat[row, row - 1] <- 1
        if (j < size) mat[row, row + 1] <- 1
      }
    }
  }

  mat
}
