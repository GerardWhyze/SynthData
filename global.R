# global --------------------------------------------------------------------

library(shiny)
library(MASS)  # for mvrnorm
library(DT)
library(bslib)

# Helper function to ensure correlation matrix is positive semidefinite
make_positive_definite <- function(mat, tol = 1e-8) {
  if (!isSymmetric(mat)) {
    mat <- (mat + t(mat)) / 2
  }
  eig <- eigen(mat)
  vals <- eig$values
  vecs <- eig$vectors
  vals[vals < tol] <- tol
  diag_mat <- diag(vals)
  adj_mat <- vecs %*% diag_mat %*% t(vecs)
  adj_mat <- (adj_mat + t(adj_mat)) / 2
  adj_mat
}