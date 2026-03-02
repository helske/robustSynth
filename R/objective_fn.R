#' Objective function for SCM
#' @noRd
objective_fn <- function(v, X0, X1, Z0, Z1, scale, qp_control, type,
                         estimate = TRUE) {
  J <- ncol(X0)
  K <- nrow(X0)
  # avoid numerical issues with tiny values
  idx <- which(v > max(1, max(v)) * qp_control$v_zerotol)
  if (length(idx) == 0 || sum(v[idx]) < 0.001) return(1e6)
  v[-idx] <- 0
  if (qp_control$normalize_v) {
    v <- v / sum(v)
  }
  sqrt_v <- sqrt(v[idx])
  opt <- try(
    limSolve::lsei(
      A = sqrt_v * X0[idx, , drop = FALSE],
      B = sqrt_v * X1[idx, , drop = FALSE],
      E = matrix(1, nrow = 1, ncol = J), F = 1,
      G = diag(J), H = numeric(J),
      tol = qp_control$tol,
      tolrank = qp_control$tolrank,
      verbose = qp_control$verbose,
      type = type
    ), silent = TRUE
  )
  if (inherits(opt, "try-error") || opt$IsError) {
    if (estimate) {
      return(1e6)
    } else {
      return(
        list(
          w = rep(NA, J), v = rep(NA, K), value = NA
        )
      )
    }
  }
  if (estimate) {
    out <- crossprod(Z1 - Z0 %*% opt$X)[1] / scale
  } else {
    out <- list(
      w = opt$X, v = v / sum(v), value = opt$solutionNorm
    )
  }
  out
}
