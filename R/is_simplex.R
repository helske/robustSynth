#' Is Vector a Simplex?
#'
#' Checks if a numeric vector x is a valid simplex of dimension K, i.e.,
#' all elements are non-negative, sum to 1, and length of x is K.
#' @param x Vector to check.
#' @param K Dimension of the simplex.
#' @param tol Numeric tolerance for checking non-negativity and sum to 1.
#' @export
is_simplex <- function(x, K, tol = sqrt(.Machine$double.eps)) {
  if (isFALSE(all.equal(K, as.integer(K)))) {
    stop("Argument 'K' must be an integer.")
  }
  is.atomic(x) &&
    is.numeric(x) &&
    is.null(dim(x)) &&
    length(x) == K &&
    all(is.finite(x)) &&
    all(x >= -tol) &&
    abs(sum(x) - 1) <= tol
}
