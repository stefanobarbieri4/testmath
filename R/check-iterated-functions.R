#' Try apply x to f n times
#'
#' @param f function. Function to which you want to apply the value.
#' @param x value. Value to which you want to try apply to the function.
#' @param n Number of times to try apply.
#'
#' @returns list. Contains iteration and results from `try_apply_function()`
#' @noRd

apply_n_times <- function(f, x, n) {

  res <- list(
    iteration = 0:n,
    outcome = list()
  )

  res$outcome[[1]] <- x

  val <- x
  for (i in 1:n) {
    log <- try_apply_function(f, val)
    res$outcome[[i + 1]] <- log
    val <- log$value
  }

  res

}

#' Check function properties composing f to itself multiple times
#' @noRd
check_iterated_function_property <- function(
    f,
    x,
    property,
    applications = NULL,
    tolerance = testthat::testthat_tolerance(),
    f_quo = NULL,
    x_quo = NULL,
    ...
) {
  if (is.null(applications)) applications <- 2

  if (is.null(f_quo)) f_quo <- rlang::enquo(f)
  if (is.null(x_quo)) x_quo <- rlang::enquo(x)

  f_act <- testthat::quasi_label(f_quo)
  x_act <- testthat::quasi_label(x_quo)

  once_lab <- sprintf("%s(%s)", f_act$lab, x_act$lab)
  twice_lab <- sprintf("%s(%s)", f_act$lab, once_lab)

  n_times_lab <- sprintf(
    "%s%s(%s)",
    f_act$lab,
    superscript(applications),
    x_act$lab
  )

  if (!is.function(f)) {
    testthat::fail(sprintf("Expected %s to be a function.", f_act$lab))
    return(invisible(FALSE))
  }

  result <- apply_n_times(f, x, applications)

  found_failure <- FALSE
  i <- 2

  while (!found_failure && i <= (applications + 1)) {
    if (result$outcome[[i]]$success) {
      i <- i + 1
    } else {
      found_failure <- TRUE
    }
  }

  if (found_failure) {
    testthat::fail(
      c(
        sprintf("Could not apply %s to %s.", f_act$lab, x_act$lab),
        "",
        sprintf("First failure occurred on iteration: %i", i - 1),
        "",
        sprintf("Details: %s", conditionMessage(result$outcome[[i]]$error))
      )
    )
  } else {
    vals <- switch(
      property,
      "idempotent" = list(
        lhs = result$outcome[[applications + 1]]$value,
        rhs = result$outcome[[2]]$value,
        lhs_lab = twice_lab,
        rhs_lab = once_lab,
        description = property
      ),
      "n_idempotent" = list(
        lhs = result$outcome[[applications + 1]]$value,
        rhs = result$outcome[[2]]$value,
        lhs_lab = n_times_lab,
        rhs_lab = once_lab,
        description = sprintf("%i-idempotent", applications)
      ),
      "involutory" = list(
        lhs = result$outcome[[applications + 1]]$value,
        rhs = x,
        lhs_lab = twice_lab,
        rhs_lab = x_act$lab,
        description = property
      ),
      "n_involutory" = list(
        lhs = result$outcome[[applications + 1]]$value,
        rhs = x,
        lhs_lab = n_times_lab,
        rhs_lab = x_act$lab,
        description = sprintf("%i-involutory", applications)
      )
    )

    if (is.complex(vals$lhs) && !is.complex(vals$rhs)) {
      vals$rhs <- as.complex(vals$rhs)
    } else if (!is.complex(vals$lhs) && is.complex(vals$rhs)) {
      vals$lhs <- as.complex(vals$lhs)
    }

    comparison <- waldo::compare(
      vals$lhs,
      vals$rhs,
      ...,
      tolerance = tolerance,
      x_arg = vals$lhs_lab,
      y_arg = vals$rhs_lab
    )

    if (length(comparison) == 0) {
      testthat::pass()
    } else {
      testthat::fail(
        c(
          sprintf(
            "Expected %s to be %s on %s",
            f_act$lab,
            vals$description,
            x_act$lab
          ),
          "",
          sprintf(
            "%s functions require:",
            tools::toTitleCase(property)
          ),
          "",
          sprintf(
            "    %s == %s",
            vals$lhs_lab,
            vals$rhs_lab
          ),
          "",
          "Differences:",
          "",
          comparison
        )
      )
    }
  }
}
