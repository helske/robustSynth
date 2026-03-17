test_that("scm errors on invalid data", {
  expect_snapshot(scm("not a list"), error = TRUE)
  expect_snapshot(scm(list(a = 1)), error = TRUE)
})

test_that("scm errors on invalid trials", {
  d <- make_test_data()
  expect_snapshot(scm(d, trials = 1.5), error = TRUE)
})

test_that("scm errors on invalid alpha", {
  d <- make_test_data()
  expect_snapshot(scm(d, trials = 1, alpha = -1), error = TRUE)
})

test_that("scm errors on invalid fixed_v", {
  d <- make_test_data()
  expect_snapshot(scm(d, fixed_v = c(0.5, 0.5)), error = TRUE)
})

test_that("scm errors on negative v_zerotol", {
  d <- make_test_data()
  expect_snapshot(
    scm(d, trials = 1, qp_control = list(v_zerotol = -1)),
    error = TRUE
  )
})

test_that("scm works with fixed_v = 'uniform'", {
  d <- make_test_data()
  out <- scm(d, fixed_v = "uniform")
  expect_named(
    out,
    c(
      "solution_v", "solution_w", "loss_v", "loss_w",
      "synthetic_Y", "effect", "optimization_info"
    )
  )
  expect_length(out$solution_v, nrow(d$X0))
  expect_length(out$solution_w, ncol(d$X0))
  expect_identical(out$effect, as.numeric(d$Y1 - d$Y0 %*% out$solution_w))
})

test_that("donor weights are a valid simplex with fixed_v = 'uniform'", {
  d <- make_test_data()
  out <- scm(d, fixed_v = "uniform")
  expect_identical(
    is_simplex(out$solution_w, ncol(d$X0)),
    TRUE
  )
})

test_that("scm works with custom fixed_v", {
  d <- make_test_data()
  v <- c(0.5, 0.3, 0.2)
  out <- scm(d, fixed_v = v)
  expect_length(out$solution_v, 3)
  expect_identical(
    is_simplex(out$solution_w, ncol(d$X0)),
    TRUE
  )
})

test_that("scm works with optimization (fixed_v = NULL, single trial)", {
  d <- make_test_data()
  out <- scm(d, trials = 1)
  expect_named(
    out,
    c(
      "solution_v", "solution_w", "loss_v", "loss_w",
      "synthetic_Y", "effect", "optimization_info"
    )
  )
  expect_identical(
    is_simplex(out$solution_w, ncol(d$X0)),
    TRUE
  )
  expect_length(out$synthetic_Y, nrow(d$Y0))
  expect_length(out$effect, nrow(d$Y0))
})

test_that("scm works with multiple trials", {
  d <- make_test_data()
  out <- suppressMessages(scm(d, trials = 3))
  expect_length(out$optimization_info$all_loss_v, 3)
  expect_length(out$optimization_info$return_codes, 3)
})

test_that("synthlike_output produces Synth-compatible format", {
  d <- make_test_data()
  out <- scm(d, fixed_v = "uniform", synthlike_output = TRUE)
  expect_named(
    out,
    c(
      "solution.v", "solution.w", "loss.v", "loss.w",
      "synthetic_Y", "effect", "optimization_info"
    )
  )
  expect_s3_class(out$solution.v, "data.frame")
  expect_identical(is.matrix(out$solution.w), TRUE)
  expect_identical(colnames(out$solution.w), "w.weight")
})
test_that("scm works with each qp_type", {
  d <- make_test_data()
  for (type in c("lsei", "solve.QP", "both")) {
    out <- scm(d, fixed_v = "uniform", qp_control = list(qp_type = type))
    expect_length(out$solution_w, ncol(d$X0))
  }
})
