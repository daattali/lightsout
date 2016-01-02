context("utils")

test_that("position_1d_to_2d works byrow", {
  expect_equal(
    position_1d_to_2d(1, 4, byrow = TRUE),
    c(1, 1)
  )
  expect_equal(
    position_1d_to_2d(4, 4, byrow = TRUE),
    c(1, 4)
  )
  expect_equal(
    position_1d_to_2d(5, 4, byrow = TRUE),
    c(2, 1)
  )
  expect_equal(
    position_1d_to_2d(15, 4, byrow = TRUE),
    c(4, 3)
  )
})

test_that("position_1d_to_2d works by column", {
  expect_equal(
    position_1d_to_2d(1, 4, byrow = FALSE),
    c(1, 1)
  )
  expect_equal(
    position_1d_to_2d(4, 4, byrow = FALSE),
    c(4, 1)
  )
  expect_equal(
    position_1d_to_2d(5, 4, byrow = FALSE),
    c(1, 2)
  )
  expect_equal(
    position_1d_to_2d(15, 4, byrow = FALSE),
    c(3, 4)
  )
})

test_that("generating 3x3 classic lightsout matrix is correct", {
  M <- matrix(c(1, 1, 0,
                1, 1, 1,
                0, 1, 1), ncol = 3)
  I <- diag(3)
  Z <- matrix(0, ncol = 3, nrow = 3)
  matrix <-
    c() %>%
    rbind(cbind(M, I, Z)) %>%
    rbind(cbind(I, M, I)) %>%
    rbind(cbind(Z, I, M))

  expect_equal(matrix,
               generate_lightsout_matrix(3, classic = TRUE))
})

test_that("generating 3x3 variant lightsout matrix is correct", {
  M <- matrix(1, ncol = 3, nrow =3)
  I <- diag(3)
  Z <- matrix(0, ncol = 3, nrow = 3)
  matrix <-
    c() %>%
    rbind(cbind(M, I, I)) %>%
    rbind(cbind(Z, M, I)) %>%
    rbind(cbind(Z, I, M))

  expect_equal(matrix,
               generate_lightsout_matrix(3, classic = FALSE))
})

test_that("generating 5x5 classic lightsout matrix is correct", {
  M <- matrix(c(1, 1, 0, 0, 0,
                1, 1, 1, 0, 0,
                0, 1, 1, 1, 0,
                0, 0, 1, 1, 1,
                0, 0, 0, 1, 1), ncol = 5)
  I <- diag(5)
  Z <- matrix(0, ncol = 5, nrow = 5)
  matrix <-
    c() %>%
    rbind(cbind(M, I, Z, Z, Z)) %>%
    rbind(cbind(I, M, I, Z, Z)) %>%
    rbind(cbind(Z, I, M, I, Z)) %>%
    rbind(cbind(Z, Z, I, M, I)) %>%
    rbind(cbind(Z, Z, Z, I, M))

  expect_equal(matrix,
               generate_lightsout_matrix(5, classic = TRUE))
})

test_that("generating 5x5 variant lightsout matrix is correct", {
  M <- matrix(1, nrow = 5, ncol = 5)
  I <- diag(5)
  Z <- matrix(0, ncol = 5, nrow = 5)
  matrix <-
    c() %>%
    rbind(cbind(M, I, I, I, I)) %>%
    rbind(cbind(Z, M, I, I, I)) %>%
    rbind(cbind(Z, I, M, I, I)) %>%
    rbind(cbind(Z, I, I, M, I)) %>%
    rbind(cbind(Z, I, I, I, M))

  expect_equal(matrix,
               generate_lightsout_matrix(5, classic = FALSE))
})

