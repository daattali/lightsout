% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/board.R
\name{empty_board}
\alias{empty_board}
\title{Initialize a Lights Out board with all lights switched off}
\usage{
empty_board(size, classic = TRUE)
}
\arguments{
\item{size}{Number of rows and columns for the board}

\item{classic}{If \code{TRUE}, then pressing a light will toggle it and its
adjacent neighbours only. If \code{FALSE}, then pressing a light will toggle
the entire row and column of the pressed light.}
}
\value{
A \code{lightsout} board.
}
\description{
Initialize a Lights Out board with all lights switched off
}
\examples{
empty_board(5)
}
\seealso{
\code{\link[lightsout]{random_board}}
\code{\link[lightsout]{new_board}}
}
