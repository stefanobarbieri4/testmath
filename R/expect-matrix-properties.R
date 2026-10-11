#' Do you expect a matrix to be symmetric?
#'
#' @param matrix matrix. The matrix you are checking for symmetry.
#' @param tolerance numeric. The tolerance of difference between compared
#'   entries. Defaults to `testthat`'s default tolerance
#'
#' @family matrix properties
#'
#' @export
#'
#' @details Checks whether \eqn{A_{ij} = A_{ji}} for all i, j. As soon as an
#'   infringement is found, it breaks early.
#'
#' @examples
#' mat1 <- matrix(c(7, 4, 4, 5), nrow = 2, ncol = 2) # symmetric
#' mat2 <- matrix(c(4, 1, 3, 9), nrow = 2, ncol = 2) # not symmetric
#'
#' expect_symmetric(mat1)
#' testthat::expect_failure(expect_symmetric(mat2))

expect_symmetric <- function(
    matrix,
    tolerance = testthat::testthat_tolerance()
) {
  matrix_quo <- rlang::enquo(matrix)

  res <- check_symmetric(matrix, tolerance, matrix_quo)

  if (!res$symmetric) {
    testthat::fail(res$message)
  } else {
    testthat::pass()
  }

  invisible(matrix)
}

#' Do you expect a matrix to be symmetric positive definite?
#'
#' @param matrix matrix. The matrix you are checking to be SPD.
#' @param tolerance numeric. The tolerance for symmetry checks. Defaults to
#'   `testthat`'s default tolerance.
#'
#' @family matrix properties
#'
#' @details In addition to checking whether the matrix is symmetric, it also
#'   checks if the eigenvalues of `matrix` are all strictly positive.
#'
#' @export
#'
#' @examples
#' mat_spd <- matrix(c(2, -1, -1, 2), nrow = 2) # spd
#' mat_not_spd <- matrix(c(1, 2, 2, 1), nrow = 2) # symmetric, but not positive definite
#'
#' expect_spd(mat_spd)
#' testthat::expect_failure(expect_spd(mat_not_spd))

expect_spd <- function(
    matrix,
    tolerance = testthat::testthat_tolerance()
) {
  matrix_quo <- rlang::enquo(matrix)

  res <- check_spd(matrix, tolerance, matrix_quo)

  if (!res$spd) {
    testthat::fail(res$message)
  } else {
    testthat::pass()
  }

  invisible(matrix)
}
