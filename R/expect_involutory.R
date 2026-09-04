#' Do you expect a function to be involutory?
#'
#' @description This function tests whether a function, \eqn{f} is its own
#'  inverse for a given value of \eqn{x}.
#'
#'
#' @param f function. The function you are testing to be involutory
#' @param x parameter(s). The value under which you test the function.
#' @param ... Arguments passed into [waldo::compare()]
#'
#' @export
#'
#' @details Tests that \eqn{f(f(x)) = x}. A function is called involutory (or a
#'   self-inverse function) if this applies at each element in its domain.
#'
#' @examples
#' x <- c(2, 1, 3)
#' expect_involutory(rev, x) # passes
#' expect_involutory(sort, x) # errors

expect_involutory <- function(f, x, ...) {
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
      # Compare f(f(x)) with x
      comparison <- waldo::compare(
        twice$value,
        x,
        ...,
        x_arg = twice_lab,
        y_arg = once_lab
      )

      if (length(comparison) > 0) {
        testthat::fail(
          c(
            sprintf(
              "Expected %s to be involutory on %s.",
              f_act$lab,
              x_act$lab
            ),
            "",
            "Idempotence requires:",
            "",
            sprintf(
              "    %s == %s",
              twice_lab,
              x_act$lab
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
