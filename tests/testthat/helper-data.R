make_test_data <- function(K = 3, J = 5, T_pre = 6, T_post = 4) {
  set.seed(42)
  # treated unit is a convex combination of controls
  true_w <- c(0.4, 0.3, 0.2, 0.1, 0)
  X0 <- matrix(rnorm(K * J), nrow = K)
  X1 <- X0 %*% true_w + rnorm(K, sd = 0.01)
  Z0 <- matrix(rnorm(T_pre * J), nrow = T_pre)
  Z1 <- Z0 %*% true_w + rnorm(T_pre, sd = 0.01)
  Y0 <- matrix(rnorm((T_pre + T_post) * J), nrow = T_pre + T_post)
  Y1 <- Y0 %*% true_w + rnorm(T_pre + T_post, sd = 0.01)
  list(
    X0 = X0, X1 = matrix(X1, ncol = 1),
    Z0 = Z0, Z1 = matrix(Z1, ncol = 1),
    Y0 = Y0, Y1 = matrix(Y1, ncol = 1)
  )
}
