#' Function to run nlopt possibly twice using different QP solvers
#' @noRd
run_nloptr <- function(v, X0, X1, Z0, Z1, scale, nloptr_control, qp_control) {
  K <- length(v)
  if (qp_control$qp_type == "both") {
    opt_lsei <- nloptr::nloptr(
      v, objective_fn, lb = rep(0, K), opts = nloptr_control,
      X0 = X0, X1 = X1, Z0 = Z0, Z1 = Z1, scale = scale,
      qp_control = qp_control, type = 1, estimate = TRUE
    )
    opt_solveQP <- nloptr::nloptr(
      v, objective_fn, lb = rep(0, K), opts = nloptr_control,
      X0 = X0, X1 = X1, Z0 = Z0, Z1 = Z1, scale = scale,
      qp_control = qp_control, type = 2, estimate = TRUE
    )
    solution <- pick_nloptr_solution(opt_lsei, opt_solveQP)
  } else {
    type <- if (qp_control$qp_type == "lsei") 1 else 2
    solution <- nloptr::nloptr(
      v, objective_fn, lb = rep(0, K), opts = nloptr_control,
      X0 = X0, X1 = X1, Z0 = Z0, Z1 = Z1, scale = scale,
      qp_control = qp_control, type = type, estimate = TRUE
    )
    solution$qp_type <- qp_control$qp_type
  }
  solution
}

#' Pick the better solution
#' @noRd
pick_nloptr_solution <- function(opt_lsei, opt_solveQP) {

  lsei_failed <- opt_lsei$status < 0
  solveQP_failed <- opt_solveQP$status < 0

  if (lsei_failed && !solveQP_failed) {
    opt_solveQP$qp_type <- "solve.QP"
    return(opt_solveQP)
  } else if (!lsei_failed && solveQP_failed) {
    opt_lsei$qp_type <- "lsei"
    return(opt_lsei)
  } else if (lsei_failed && solveQP_failed) {
    opt_lsei$qp_type <- "lsei"
    return(opt_lsei)
  } else {
    if (opt_lsei$objective < opt_solveQP$objective) {
      opt_lsei$qp_type <- "lsei"
      return(opt_lsei)
    } else {
      opt_solveQP$qp_type <- "solve.QP"
      return(opt_solveQP)
    }
  }
}
#' Pick the better solution
#' @noRd
pick_qp_solution <- function(opt_lsei, opt_solveQP) {

  lsei_failed <- is.na(opt_lsei$value)
  solveQP_failed <- is.na(opt_solveQP$value)

  if (lsei_failed && !solveQP_failed) {
    opt_solveQP$qp_type <- "solve.QP"
    return(opt_solveQP)
  } else if (!lsei_failed && solveQP_failed) {
    opt_lsei$qp_type <- "lsei"
    return(opt_lsei)
  } else if (lsei_failed && solveQP_failed) {
    opt_lsei$qp_type <- "lsei"
    return(opt_lsei)
  } else {
    if (opt_lsei$value < opt_solveQP$value) {
      opt_lsei$qp_type <- "lsei"
      return(opt_lsei)
    } else {
      opt_solveQP$qp_type <- "solve.QP"
      return(opt_solveQP)
    }
  }
}
