#' Robust Synthetic Control
#'
#' Estimates synthetic control using multiple initializations for predictor
#' weights to improve the convergence to global optimum. This can be
#' parallelized using [future::plan()], with automatic progress reporting by
#' [progressr::with_progress()] (see examples below).
#'
#' In contrast to the original Synth package, this implementation uses a
#' computationally more efficient quadratic programming of [limSolve::lsei()]
#' donor weight optimization and optimization algorithms of `nloptr` for
#' predictor weight optimization. By default, both solver types of
#' [limSolve::lsei()] are used the one with better solution is picked. For
#' [nloptr::nloptr()], the default optimization algorithm is the
#' derivative-free `NLOPT_LN_SBPLX`.
#'
#' Finally, whether or not to scale predictors using their pre-treatment
#' standard deviations can be controlled using argument `scaling`. This is
#' especially useful if you want to use all pre-treatment outcomes as
#' predictors with uniform weights (i.e., `fixed_v = "uniform"` and
#' `scaling = FALSE`).
#'
#' @param data Either an output from [Synth::dataprep()], or a list with
#' matrix `Y0` and vector `Y1` of post-treatment outcomes of control units and
#' treated unit, respectively; matrix `X0` and vector `X1` of pre-treatment
#' predictors of control units and treated unit, respectively;
#' and matrix `Z0` and vector `Z1` of pre-treatment outcomes used in
#' optimizing donor weights w of control units and treated unit, respectively.
#' @param fixed_v Either `NULL` (default), `"uniform"`, or custom values.
#' If `NULL`, initializes the predictor weights uniformly. If `"uniform"`, uses
#' fixed uniform weights. If custom values, should be a numeric simplex vector
#' of length equal to the number of predictors which are used as fixed values for
#' predictor weights v.
#' @param scaling Logical, whether to scale each predictor to with their
#' pretreatment standard deviation. Default is `TRUE` unless `fixed_v` is set to
#' `"uniform"`.
#' @param trials Positive integer, number of random initializations to use
#' when `fixed_v` is `NULL`. First optimization run uses uniform weights,
#' while for the rest (if `trials` > 1) the initial values are sampled from
#' symmetric Dirichlet(0.5) distribution. The default is 20.
#' @param nloptr_control A list of control parameters to be passed to
#' [nloptr::nloptr()] as argument `control`. See documentation of
#' [nloptr::nloptr()] for details.
#' @param qp_control A list of control parameters to be passed to
#' [limSolve::lsei()], i.e, arguments `type`, `tol`, `tolrank`, and `verbose`.
#' @param alpha Positive numeric scalar, concentration parameter for symmetric
#' Dirichlet distribution used for sampling initial predictor weights Default
#' is 0.1, which puts more mass on sparse predictor weights.
#' @param synthlike_output Logical. If `TRUE`, uses same output format as in
#' [Synth::synth], i.e. the output is a list with elements such as `solution.v`
#' and `loss.v` instead of `v` and `loss_v`. Default is `FALSE`. Setting this
#' `TRUE` allows the use of `robustSynth` as a drop-in replacement for `Synth`.
#' @export
#' @examples
#' if (requireNamespace("Synth", quietly = TRUE)) {
#'   # from ?Synth::synth
#'   data(synth.data, package = "Synth")
#'
#'   d <- Synth::dataprep(
#'     synth.data,
#'     predictors = c("X1", "X2", "X3"),
#'     predictors.op = "mean",
#'     dependent = "Y",
#'     unit.variable = "unit.num",
#'     time.variable = "year",
#'     special.predictors = list(
#'       list("Y", 1991, "mean"), list("Y", 1985, "mean"), list("Y", 1980, "mean")
#'     ),
#'     treatment.identifier = 7,
#'     controls.identifier = c(29, 2, 13, 17, 32, 38),
#'     time.predictors.prior = c(1984:1989),
#'     time.optimize.ssr = c(1984:1990),
#'     unit.names.variable = "name",
#'     time.plot = 1984:1996
#'    )
#'    synth_out <- Synth::synth(d)
#'    # for parallelization, use, e.g., future::plan(multisession, workers = 4)
#'    if (interactive()) {
#'      # show progress bar
#'      progressr::handlers(global = TRUE)
#'    }
#'    robust_out <- scm(d, trials = 20)
#'    synth_out$loss.v
#'    robust_out$loss_v
#'    data.frame(
#'      time = d$tag$time.plot,
#'      synth = c(d$Y1plot - d$Y0plot %*% synth_out$solution.w),
#'      robust = robust_out$effect
#'    )
#' }
scm <- function(data,
                fixed_v = NULL, scaling = !identical(fixed_v, "uniform"),
                trials = 20,
                nloptr_control = list(),
                qp_control = list(),
                alpha = 0.1,
                synthlike_output = FALSE) {

  stopifnot(
    "Argument 'trials' should be positive integer." = {
      !missing(trials)
      is.numeric(trials)
      length(trials) == 1L
      trials > 0
      trials == as.integer(trials)
    }
  )
  stopifnot(
    "Argument 'alpha' should be positive numeric scalar." = {
      !missing(alpha)
      is.numeric(alpha)
      length(alpha) == 1L
      alpha > 0
    }
  )
  # check if data looks like an output from Synth::dataprep
  # if not, check that data has the required elements Y1, Y0, X1, X0, Z1, and Z0
  synth_names <- c(
    "X0", "X1", "Z0", "Z1", "Y0plot", "Y1plot", "names.and.numbers", "tag"
  )
  required_names <- c("Y1", "Y0", "X1", "X0", "Z1", "Z0")
  if (
    is.list(data) && (
      identical(names(data), synth_names) ||
      all(required_names %in% names(data))
    )
  ) {
    # should check for dims as well
    Y1 <- data$Y1
    Y0 <- data$Y0
    X1 <- data$X1
    X0 <- data$X0
    Z1 <- data$Z1
    Z0 <- data$Z0
  } else {
    stop (
      paste0(
        "Argument 'data' should be a list with elements ",
        "Y1, Y0, X1, X0, Z1, and Z0, or an output from Synth::dataprep."
      )
    )
  }

  if (scaling) {
    sd_x <- apply(cbind(X0, X1), 1, stats::sd)
    X0 <- X0 / sd_x
    X1 <- X1 / sd_x
  }
  nloptr_control <- utils::modifyList(
    list(
      maxeval = 1e4,
      ftol_rel = 1e-12,
      ftol_abs = 0,
      xtol_rel = 0,
      xtol_abs = 1e-10,
      print_level = 0,
      algorithm = "NLOPT_LN_SBPLX"
    ), nloptr_control
  )
  qp_control$qp_type <- match.arg(
    qp_control$qp_type, c("both", "lsei", "solve.QP")
  )
  if (!is.null(qp_control$v_zerotol) && qp_control$v_zerotol < 0) {
    stop ("Argument 'v_zerotol' in 'qp_control' should be non-negative scalar.")
  }
  qp_control <- utils::modifyList(
    list(
      tol = sqrt(.Machine$double.eps),
      tolrank = NULL,
      verbose = FALSE,
      v_zerotol = sqrt(.Machine$double.eps),
      normalize_v = TRUE,
      qp_type = "both"),
    qp_control
  )
  K <- nrow(X0) # number of predictors
  J <- ncol(X0) # number of control units
  T_pre <- nrow(Z0) # number of pre-treatment periods

  if (is.null(fixed_v) || identical(fixed_v, "uniform")) {
    v <- rep(1 / K, K)
  } else {
    if (!is_simplex(fixed_v, K)) {
      stop ("Fixed weight vector `fixed_v` is not a valid simplex.")
    }
    v <- fixed_v
  }
  scale_factor_v <- drop(crossprod(Z1 - rowMeans(Z0)))

  if (is.null(fixed_v)) {

    solution <- run_nloptr(
      v, X0, X1, Z0, Z1, scale_factor_v, nloptr_control, qp_control
    )
    # test weights scaled by variance, this should be optimal if X = Z
    if (scaling) {
      solution0 <- run_nloptr(
        sd_x^2, X0, X1, Z0, Z1, scale_factor_v,
        nloptr_control, qp_control
      )
    if (solution$status >= 0 && solution0$status >= 0 &&
        solution0$objective < solution$objective) {
      solution <- solution0
    }
  }
  if (trials > 1) {
    p <- progressr::progressor(trials)
    more_solutions <- future.apply::future_lapply(
      seq_len(trials - 1), \(i) {
        p()
        # Dirichlet(alpha)
        v <- stats::rgamma(K, alpha, 1)
        v <- v / sum(v)
        solution <- run_nloptr(
          v, X0, X1, Z0, Z1, scale_factor_v, nloptr_control, qp_control
        )
      },
      future.seed = TRUE
    )
    solutions <- c(list(solution), more_solutions)
    return_codes <- unlist(lapply(solutions, \(opt) opt$status))
    values <- unlist(lapply(solutions, \(opt) opt$objective)) * scale_factor_v / T_pre
    opt <- solutions[[which.min(values + 1e100 * (return_codes < 0))]]
    qp_types <- unlist(lapply(solutions, \(opt) opt$qp_type))
    best_qp_type <- opt$qp_type
  } else {
    opt <- solution
    return_codes <- opt$status
    values <- opt$objective * scale_factor_v / T_pre
    qp_types <- opt$qp_type
    best_qp_type <- opt$qp_type
  }
  if (!is.finite(opt$objective) || opt$status < 0) {
    error_msg <- paste0(
      "Optimization did not converge, ",
      "converge code from first run of 'nloptr': ",
      solution$message, "."
    )
    stop (error_msg)
  }
  v <- opt$solution
  out <- objective_fn(
    v, X0, X1, Z0, Z1, scale_factor_v, qp_control,
    type = if (best_qp_type == "lsei") 1 else 2, estimate = FALSE
  )
} else {
  return_codes <- NULL
  values <- NULL
  opt <- NULL
  qp_types <- NULL
  if (qp_control$qp_type == "both") {
    opt_lsei <- objective_fn(
      v, X0, X1, Z0, Z1, scale_factor_v, qp_control,
      type = 1, estimate = FALSE
    )
    opt_solveQP <- objective_fn(
      v, X0, X1, Z0, Z1, scale_factor_v, qp_control,
      type = 2, estimate = FALSE
    )
    out <- pick_qp_solution(opt_lsei, opt_solveQP)
  } else {
    type <- if (qp_control$qp_type == "lsei") 1 else 2
    out <- objective_fn(
      v, X0, X1, Z0, Z1, scale_factor_v, qp_control,
      type = type, estimate = FALSE
    )
  }
  best_qp_type <- out$qp_type
}

w <- out$w
v <- out$v
synth_Y <- as.numeric(Y0 %*% w)
effect <- as.numeric(Y1 - synth_Y)
loss_v <- crossprod(Z1 - Z0 %*% w) / T_pre
loss_w <- t(X1 - X0 %*% w) %*% diag(v) %*% (X1 - X0 %*% w)
if (any(is.na(w)) || any(is.na(v))) {
  warning (
    "Donor weights w or predictor weights v contain NA values, ",
    "indicating optimization failure. Try adjusting control parameters in
      'nloptr_control' and/or 'qp_control'."
  )
}
if (synthlike_output) {
  out <- stats::setNames(
    vector("list", 7L),
    c(
      "solution.v", "solution.w", "loss.v", "loss.w",
      "synthetic_Y", "effect", "optimization_info"
    )
  )
  out$solution.v <- stats::setNames(data.frame(t(v)), rownames(X0))
  out$solution.w <- matrix(
    w, ncol = 1, dimnames = list(colnames(X0), "w.weight")
  )
  out$loss.v <- loss_v
  out$loss.w <- loss_w
  dimnames(out$loss.w) <- dimnames(loss_v)
} else {
  out <- stats::setNames(
    vector("list", 7L),
    c(
      "solution_v", "solution_w", "loss_v", "loss_w",
      "synthetic_Y", "effect", "optimization_info"
    )
  )
  out$solution_v <- stats::setNames(v, rownames(X0))
  out$solution_w <- w
  out$loss_v <- loss_v[1]
  out$loss_w <- loss_w[1]
}
out$synthetic_Y <- synth_Y
out$effect <- effect
out$optimization_info <- list(
  all_loss_v = values,
  return_codes = return_codes,
  qp_types = qp_types,
  best_nloptr_run = opt,
  best_qp_type = best_qp_type
)
out
}



