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

#' Helper function to check if a matrix is symmetric positive definite.
#' Returns a list with `spd` (logical) and `message` (character or NULL).
#' @noRd
check_spd <- function(
    matrix,
    tolerance = testthat::testthat_tolerance(),
    matrix_quo = NULL
) {
  if (is.null(matrix_quo)) matrix_quo <- rlang::enquo(matrix)
  matrix_act <- testthat::quasi_label(matrix_quo)

  # 1. Check symmetry first using check_symmetric
  sym_res <- check_symmetric(matrix, tolerance, matrix_quo)
  if (!sym_res$symmetric) {
    return(
      list(
        spd = FALSE,
        # Adjust message to specify it's a requirement for SPD if it was a symmetry failure
        message = gsub("to be symmetric\\.", "to be symmetric (a requirement for symmetric positive definiteness).", sym_res$message)
      )
    )
  }

  # 2. Check eigenvalues
  ev <- eigen(matrix, only.values = TRUE)$values
  non_pos_indices <- which(ev <= 0)

  if (length(non_pos_indices) > 0) {
    bad_idx <- non_pos_indices[1]
    bad_val <- ev[bad_idx]

    return(
      list(
        spd = FALSE,
        message = c(
          sprintf("Expected %s to be symmetric positive definite.", matrix_act$lab),
          "\n",
          sprintf("Details: Found non-positive eigenvalue at index %i (value: %s)", bad_idx, bad_val)
        )
      )
    )
  }

  list(spd = TRUE, message = NULL)
}
