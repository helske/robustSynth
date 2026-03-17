test_that("pick_nloptr_solution picks better when both succeed", {
  opt_lsei <- list(status = 1, objective = 0.5)
  opt_solveQP <- list(status = 1, objective = 0.8)
  res <- robustSynth:::pick_nloptr_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")
  expect_identical(res$objective, 0.5)

  opt_lsei$objective <- 0.9
  res <- robustSynth:::pick_nloptr_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "solve.QP")
  expect_identical(res$objective, 0.8)
})

test_that("pick_nloptr_solution handles one failure", {
  opt_lsei <- list(status = -1, objective = 0.5)
  opt_solveQP <- list(status = 1, objective = 0.8)
  res <- robustSynth:::pick_nloptr_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "solve.QP")

  opt_lsei$status <- 1
  opt_solveQP$status <- -1
  res <- robustSynth:::pick_nloptr_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")
})

test_that("pick_nloptr_solution returns lsei when both fail", {
  opt_lsei <- list(status = -1, objective = 0.5)
  opt_solveQP <- list(status = -1, objective = 0.8)
  res <- robustSynth:::pick_nloptr_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")
})

test_that("pick_qp_solution picks better when both succeed", {
  opt_lsei <- list(value = 0.3, w = 1)
  opt_solveQP <- list(value = 0.7, w = 2)
  res <- robustSynth:::pick_qp_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")

  opt_lsei$value <- 0.9
  res <- robustSynth:::pick_qp_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "solve.QP")
})

test_that("pick_qp_solution handles one failure (NA)", {
  opt_lsei <- list(value = NA)
  opt_solveQP <- list(value = 0.5)
  res <- robustSynth:::pick_qp_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "solve.QP")

  opt_lsei$value <- 0.5
  opt_solveQP$value <- NA
  res <- robustSynth:::pick_qp_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")
})

test_that("pick_qp_solution returns lsei when both fail", {
  opt_lsei <- list(value = NA)
  opt_solveQP <- list(value = NA)
  res <- robustSynth:::pick_qp_solution(opt_lsei, opt_solveQP)
  expect_identical(res$qp_type, "lsei")
})
