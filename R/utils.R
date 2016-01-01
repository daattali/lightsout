# Create the toggle matrix used to solve the Ax=b equation in order to get
# the solution to a board
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

# Translates an integer into a (row, column) location
position_1d_to_2d <- function(pos, size, byrow = TRUE) {
  row <- floor((pos - 1) / size) + 1
  col <- pos - ((row - 1) * size)
  if (byrow) {
    c(row, col)
  } else {
    c(col, row)
  }
}
