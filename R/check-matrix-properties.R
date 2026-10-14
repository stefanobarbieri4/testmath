#' Helper function to check if a matrix is symmetric. Design needs to be a little
#'   different since it is called by `expect_spd()` and hence can't hand fails
#'   or passes directly
#' @noRd

check_symmetric <- function(
    matrix,
    tolerance = testthat::testthat_tolerance(),
    matrix_quo = NULL
) {

  if (is.null(matrix_quo)) matrix_quo <- rlang::enquo(matrix)
  matrix_act <- testthat::quasi_label(matrix_quo)

  if (!inherits(matrix, "matrix")) {

    return(
      list(
        symmetric = FALSE,
        message = sprintf("Expected %s to be a matrix.", matrix_act$lab)
      )
    )

  }

  rows <- nrow(matrix)
  cols <- ncol(matrix)

  if (rows != cols) {
    return(
      list(
      symmetric = FALSE,
      message = sprintf("Expected %s to be square (it has %s rows and %s columns).", matrix_act$lab, rows, cols)
      )
    )
  }

  asymmetric_spot <- NULL

  for (i in seq_len(rows)) {
    for (j in seq_len(cols)) {
      val1 <- matrix[i, j]
      val2 <- matrix[j, i]

      if (!isTRUE(all.equal(val1, val2, tolerance = tolerance))) {
        asymmetric_spot <- list(row = i, col = j, val_ij = val1, val_ji = val2)
        break
      }
    }
    if (!is.null(asymmetric_spot)) break
  }

  if (!is.null(asymmetric_spot)) {

    return(
      list(
        symmetric = FALSE,
        message = c(
          sprintf("Expected %s to be symmetric.", matrix_act$lab),
          "\n",
          sprintf(
            "Details: %s[%i, %i] (%s) != %s[%i, %i] (%s)",
            matrix_act$lab, asymmetric_spot$row, asymmetric_spot$col, asymmetric_spot$val_ij,
            matrix_act$lab, asymmetric_spot$col, asymmetric_spot$row, asymmetric_spot$val_ji
          )
        )
      )
    )

  }

  return(
    list(
      symmetric = TRUE,
      message = NULL
    )
  )
}

#' Helper function to check if a matrix is (symmetric) (semi)-positive definite.
#' Returns a list with `spd` (logical) and `message` (character or NULL).
#' Matrix must be symmetric with eigen values (=)>0
#' @noRd
check_pd <- function(
    matrix,
    tolerance = testthat::testthat_tolerance(),
    matrix_quo = NULL,
    desc
) {
  if (is.null(matrix_quo)) matrix_quo <- rlang::enquo(matrix)
  matrix_act <- testthat::quasi_label(matrix_quo)

  sym_res <- check_symmetric(matrix, tolerance, matrix_quo)
  if (!sym_res$symmetric) {
    return(
      list(
        positive_semidefinite = FALSE,
        positive_definite = FALSE,
        message = gsub("to be symmetric\\.", sprintf("to be symmetric (a requirement for %s).", desc), sym_res$message)
      )
    )
  }

  ev <- eigen(matrix, only.values = TRUE)$values
  negative_indices <- which(ev < -tolerance)

  if (length(negative_indices) > 0) {
    bad_idx <- negative_indices[1]
    bad_val <- ev[bad_idx]

    return(
      list(
        positive_semidefinite = FALSE,
        positive_definite = FALSE,
        message = c(
          sprintf("Expected %s to be %s.", matrix_act$lab, desc),
          "\n",
          sprintf("Details: Found negative eigenvalue at index %i (value: %s)", bad_idx, bad_val)
        )
      )
    )
  }

  positive_definite <- TRUE
  message <- NULL

  if (any(ev <= tolerance))  {
    positive_definite = FALSE
    message <- c(
      sprintf("Expected %s to be symmetric positive definite.", matrix_act$lab),
      "\n",
      sprintf("Details: Found an eigenvalue of 0.")
    )
  }

  return(
    list(
      positive_semidefinite = TRUE,
      positive_definite = positive_definite,
      message = message
    )
  )

}
