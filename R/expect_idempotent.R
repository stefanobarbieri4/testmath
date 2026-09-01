#' Try apply x to f
#'
#' @param fun function. Function to which you want to apply the value.
#' @param val value. Value to which you want to try apply to the function.
#'
#' @returns list. Details success, value of fun(val) and the error message
#' @noRd

try_apply_function <- function(fun, val) {
  tryCatch(
    {
      list(
        success = TRUE,
        value = fun(val),
        error = NULL
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        value = NULL,
        error = e
      )
    }
  )
}

#' Do you expect a function to be idempotent?
#'
#' @description This function tests whether if you apply a function on a value
#'   twice, it is the same as applying it a single time.
#'
#' @param f function. The function you are testing to be idempotent
#' @param x parameter(s). The value under which you test the function.
#' @param ... Arguments passed into [waldo::compare()]
#'
#' @export
#'
#' @details Tests that \eqn{f(f(x)) = f(x)}
#'
#' @examples
#' x <- c(2, 1, 3)
#' expect_idempotent(mean, x)
#' expect_idempotent(sort, x)

expect_idempotent <- function(f, x, ...) {
  f_act <- testthat::quasi_label(rlang::enquo(f))
  x_act <- testthat::quasi_label(rlang::enquo(x))

  once_lab <- sprintf(
    "%s(%s)",
    f_act$lab,
    x_act$lab
  )

  twice_lab <- sprintf(
    "%s(%s)",
    f_act$lab,
    once_lab
  )

  if (!is.function(f)) {
    testthat::fail(
      sprintf(
        "Expected %s to be a function.",
        f_act$lab
      )
    )

    return(invisible(FALSE))
  }

  once <- try_apply_function(f, x)

  if (!once$success) {
    testthat::fail(
      c(
        sprintf(
          "Could not apply %s to %s.",
          f_act$lab,
          x_act$lab
        ),
        "",
        "Details:",
        "",
        conditionMessage(once$error)
      )
    )
  } else {
    twice <- try_apply_function(f, once$value)

    if (!twice$success) {
      testthat::fail(
        c(
          sprintf(
            "Could not apply %s to %s.",
            f_act$lab,
            once_lab
          ),
          "",
          sprintf(
            "The first application, %s, succeeded, but its result could not be applied to %s again.",
            once_lab,
            f_act$lab
          ),
          "",
          "Details:",
          "",
          conditionMessage(twice$error)
        )
      )
    } else {
      # Compare f(f(x)) with f(x)
      comparison <- waldo::compare(
        twice$value,
        once$value,
        ...,
        x_arg = twice_lab,
        y_arg = once_lab
      )

      if (length(comparison) > 0) {
        testthat::fail(
          c(
            sprintf(
              "Expected %s to be idempotent on %s.",
              f_act$lab,
              x_act$lab
            ),
            "",
            "Idempotence requires:",
            "",
            sprintf(
              "    %s == %s",
              twice_lab,
              once_lab
            ),
            "",
            "Differences:",
            "",
            comparison
          )
        )
      } else {
        testthat::pass()
      }
    }
  }
}
