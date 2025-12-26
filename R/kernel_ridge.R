#rm(list=ls())


# alpha select
kernel_ridge_fit <- function(K, y, lambda = 0.1) {
  n <- length(y)
  alpha <- solve(K + lambda * diag(n), y)
  alpha
}


# predict용 kernel 함수 계산
kernel_cross <- function(X_train, X_new, q = 1 / ncol(X_train)) {

  n <- nrow(X_train)
  m <- nrow(X_new)
  p <- ncol(X_train)

  norm_train <- rowSums(X_train^2)
  norm_new <- rowSums(X_new^2)

  D <- outer(norm_new, norm_train) - 2 * X_new %*% t(X_train)
  exp(-q * D)
}



## 최종 kernel ridge 함수
kernel_ridge <- function(X, y, lambda = 0.1, q = 1 / ncol(X)) {

  # 1. kernel matrix
  K <- kernel_base(X, q)

  # 2. fit
  alpha <- kernel_ridge_fit(K, y, lambda)

  # 3. predict function
  predict <- function(X_new) {
    K_new <- kernel_cross(X, X_new, q)
    as.vector(K_new %*% alpha)
  }

  list(
    K = K,
    alpha = alpha,
    lambda = lambda,
    q = q,
    predict = predict
  )
}
