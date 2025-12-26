
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

kernel_base <- function(X, q = 1 / ncol(X)) {

  n <- nrow(X)
  p <- ncol(X)

  normx <- drop((X^2) %*% rep(1, p))
  A <- X %*% t(X)
  D <- (-2 * A + normx) + outer(rep(1, n), normx)

  K <- exp(-q * D)
  K
}

#set.seed(924)
#M <- matrix(runif(12), nrow = 3, ncol = 4)
#M

#kernel_base(M)
