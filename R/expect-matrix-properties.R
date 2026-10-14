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
#' @param tolerance numeric. The tolerance for symmetry checks and eigenvalues.
#'  Defaults to `testthat`'s default tolerance.
#'
#' @family matrix properties
#'
#' @details In addition to checking whether the matrix is symmetric, it also
#'   checks if the eigenvalues of `matrix` are all strictly positive. Equivalent
#'   to checking if \eqn{x^TAx > 0}.
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

  res <- check_pd(matrix, tolerance, matrix_quo, "symmetric positive definite")

  if (!res$positive_definite) {
    testthat::fail(res$message)
  } else {
    testthat::pass()
  }

  invisible(matrix)
}

#' Do you expect a matrix to be positive semidefinite?
#'
#' @param matrix matrix. The matrix you are checking to be PSD.
#' @param tolerance numeric. The tolerance for symmetry checks and eigenvalues.
#'  Defaults to `testthat`'s default tolerance.
#'
#' @family matrix properties
#'
#' @details In addition to checking whether the matrix is symmetric, it also
#'   checks if the eigenvalues of `matrix` are all positive.  Equivalent
#'   to checking if \eqn{x^TAx \ge 0}.
#'
#' @export
#'
#' @examples
#' # Matrix with zero eigenvalue (PSD, but not strictly PD)
#' mat_psd <- matrix(c(1, 1, 1, 1), nrow = 2)
#' expect_psd(mat_psd)
#'
#' # Indefinite matrix fails PSD
#' mat_bad <- matrix(c(1, 2, 2, 1), nrow = 2)
#' testthat::expect_failure(expect_psd(mat_bad))

expect_psd <- function(
    matrix,
    tolerance = testthat::testthat_tolerance()
) {
  matrix_quo <- rlang::enquo(matrix)

  res <- check_pd(matrix, tolerance, matrix_quo, "positive semidefinite")

  if (!res$positive_semidefinite) {
    testthat::fail(res$message)
  } else {
    testthat::pass()
  }

  invisible(matrix)
}

#' Do you expect a matrix to be orthogonal?
#'
#' @param matrix matrix. The matrix you are checking for orthogonality.
#' @param tolerance numeric. The tolerance of difference between compared
#'   entries. Defaults to `testthat`'s default tolerance.
#' @param ... Additional arguments passed to `waldo::compare()`.
#'
#' @family matrix properties
#'
#' @export
#'
#' @examples
#' # Rotation matrix is orthogonal
#' theta <- pi / 4
#' R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
#' expect_orthogonal(R)
#'
#' # Non-orthogonal matrix
#' mat_bad <- matrix(1:4, nrow = 2)
#' testthat::expect_failure(expect_orthogonal(mat_bad))

expect_orthogonal <- function(
    matrix,
    tolerance = testthat::testthat_tolerance(),
    ...
) {

  matrix_act <- testthat::quasi_label(rlang::enquo(matrix))

  if (!inherits(matrix, "matrix")) {

    testthat::fail(
      message = sprintf("Expected %s to be a matrix.", matrix_act$lab)
    )

    return(invisible(FALSE))

  }

  rows <- nrow(matrix)
  cols <- ncol(matrix)

  if (rows != cols) {

    testthat::fail(
      message = sprintf("Expected %s to be square (it has %s rows and %s columns).", matrix_act$lab, rows, cols)
    )

    return(invisible(FALSE))

  }

  comparison <- waldo::compare(
    diag(rows),
    matrix %*% t(matrix),
    ...,
    tolerance = tolerance,
    x_arg = sprintf("diag(%i)", rows),
    y_arg = sprintf("%s * transpose%s", matrix_act$lab, matrix_act$lab)
  )

  if (length(comparison) == 0) {
    testthat::pass()
  } else {
    testthat::fail(
      c(
        sprintf(
          "Expected %s to be orthogonal",
          matrix_act$lab
        ),
        "",
        "Orthogonal matrices require:",
        "",
        sprintf(
          "    %s * %s^T == I",
          matrix_act$lab,
          matrix_act$lab
        ),
        "",
        "Differences:",
        "",
        comparison
      )
    )
  }

}

#' Do you expect a matrix to be diagonal?
#'
#' @param matrix matrix. The matrix you are checking to be diagonal.
#' @param tolerance numeric. The tolerance of difference between compared
#'   entries. Defaults to `testthat`'s default tolerance.
#' @param ... Additional arguments passed to `waldo::compare()`.
#'
#' @family matrix properties
#'
#' @export
#'
#' @examples
#' mat_diag <- diag(c(1, 5, 3))
#' mat_not_diag <- matrix(c(1, 2, 0, 3), nrow = 2)
#'
#' expect_diagonal(mat_diag)
#' testthat::expect_failure(expect_diagonal(mat_not_diag))

expect_diagonal <- function(
    matrix,
    tolerance = testthat::testthat_tolerance(),
    ...
) {
  matrix_quo <- rlang::enquo(matrix)
  matrix_act <- testthat::quasi_label(matrix_quo)

  if (!inherits(matrix, "matrix")) {
    testthat::fail(
      sprintf("Expected %s to be a matrix.", matrix_act$lab)
    )
    return(invisible(matrix))
  }

  rows <- nrow(matrix)
  cols <- ncol(matrix)

  if (rows != cols) {
    testthat::fail(
      sprintf("Expected %s to be square (it has %s rows and %s columns).", matrix_act$lab, rows, cols)
    )
    return(invisible(matrix))
  }

  diag_mat <- diag(diag(matrix))

  comparison <- waldo::compare(
    diag_mat,
    matrix,
    ...,
    tolerance = tolerance,
    x_arg = sprintf("diag(diag(%s))", matrix_act$lab),
    y_arg = matrix_act$lab
  )

  if (length(comparison) == 0) {
    testthat::pass()
  } else {
    testthat::fail(
      c(
        sprintf("Expected %s to be a diagonal matrix.", matrix_act$lab),
        "",
        "Off-diagonal elements must be zero.",
        "",
        "Differences:",
        "",
        comparison
      )
    )
  }

  invisible(matrix)
}



