test_that("is_simplex accepts valid simplexes", {
  expect_identical(is_simplex(c(0.5, 0.5), 2), TRUE)
  expect_identical(is_simplex(c(1, 0, 0), 3), TRUE)
  expect_identical(is_simplex(rep(1 / 5, 5), 5), TRUE)
})

test_that("is_simplex rejects wrong length", {
  expect_identical(is_simplex(c(0.5, 0.5), 3), FALSE)
  expect_identical(is_simplex(c(1), 2), FALSE)
})

test_that("is_simplex rejects negative elements", {
  expect_identical(is_simplex(c(-0.1, 1.1), 2), FALSE)
})

test_that("is_simplex rejects vectors not summing to 1", {
  expect_identical(is_simplex(c(0.5, 0.3), 2), FALSE)
  expect_identical(is_simplex(c(0.5, 0.6), 2), FALSE)
})

test_that("is_simplex rejects non-numeric and non-atomic inputs", {
  expect_identical(is_simplex(c("a", "b"), 2), FALSE)
  expect_identical(is_simplex(list(0.5, 0.5), 2), FALSE)
})

test_that("is_simplex rejects non-finite values", {
  expect_identical(is_simplex(c(NA, 0.5), 2), FALSE)
  expect_identical(is_simplex(c(NaN, 0.5), 2), FALSE)
  expect_identical(is_simplex(c(Inf, -Inf), 2), FALSE)
})

test_that("is_simplex rejects matrix input", {
  expect_identical(is_simplex(matrix(c(0.5, 0.5), ncol = 1), 2), FALSE)
})

test_that("is_simplex rejects non-integer K", {
  expect_identical(is_simplex(c(0.5, 0.5), 2.5), FALSE)
})

test_that("is_simplex respects tolerance", {
  x <- c(0.5, 0.5 + 1e-15)
  expect_identical(is_simplex(x, 2), TRUE)
  x <- c(-1e-15, 1 + 1e-15)
  expect_identical(is_simplex(x, 2), TRUE)
  expect_identical(is_simplex(c(-0.01, 1.01), 2, tol = 0.001), FALSE)
})
