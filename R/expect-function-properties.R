#' Do you expect a function to be idempotent?
#'
#' @description This function tests whether applying a function twice yields the
#'   same result as applying it once.
#'
#' @param f function. The function you are testing.
#' @param x parameter(s). The value under which you test the function.
#' @param tolerance Passed on to [waldo::compare()]. If non-null, ignores small
#' floating point differences
#' @param ... Additional arguments passed into [waldo::compare()]
#'
#' @family function properties
#'
#' @export
#'
#' @examples
#' x <- c(2, 1, 3)
#' expect_idempotent(mean, x)

expect_idempotent <- function(f, x, tolerance = testthat::testthat_tolerance(), ...) {
  f_quo <- rlang::enquo(f)
  x_quo <- rlang::enquo(x)
  check_iterated_function_property(
    f = f, x = x, property = "idempotent", applications = 2,
    tolerance = tolerance, f_quo = f_quo, x_quo = x_quo, ...
  )
}

#' Do you expect a function to be \eqn{n}-idempotent?
#'
#' @description This function tests whether a function, \eqn{f}, applied \eqn{n}
#'   times on \eqn{x} yields the same result as applying it a single time.
#'
#' @param f function. The function you are testing.
#' @param x parameter(s). The value under which you test the function `f`.
#' @param n integer. The expected order of idempotence.
#' @param tolerance Passed on to [waldo::compare()]. If non-null, ignores small
#' floating point differences
#' @param ... Additional arguments passed into [waldo::compare()]
#'
#' @family function properties
#'
#' @export
#'
#' @details Tests that \eqn{f^n(x) = f(x)}. For `n` = 2, this is equivalent to
#'   `expect_idempotent`.
#'
#' @examples
#' # A function that reaches its fixed/stable state after 3 steps
#' custom_func <- function(x) {
#'   pmin(pmax(x, 0), 1) # e.g., clipping logic
#' }
#'
#' expect_n_idempotent(custom_func, -5:5, n = 3)
#'
#'
expect_n_idempotent <- function(f, x, n, tolerance = testthat::testthat_tolerance(), ...) {
  f_quo <- rlang::enquo(f)
  x_quo <- rlang::enquo(x)
  check_iterated_function_property(
    f = f, x = x, property = "n_idempotent", applications = n,
    tolerance = tolerance, f_quo = f_quo, x_quo = x_quo, ...
  )
}
#' Do you expect a function to be involutory?
#'
#' @description This function tests whether a function, \eqn{f} is its own
#'  inverse for a given value of \eqn{x}.
#'
#'
#' @param f function. The function you are testing to be involutory
#' @param x parameter(s). The value under which you test the function.
#' @param tolerance Passed on to [waldo::compare()]. If non-null, ignores small
#' floating point differences
#' @param ... Additional arguments passed into [waldo::compare()]
#'
#' @family function properties
#'
#' @export
#'
#' @details Tests that \eqn{f(f(x)) = x}. A function is called involutory (or a
#'   self-inverse function) if this applies at each element in its domain.
#'
#' @examples
#' x <- c(2, 1, 3)
#' expect_involutory(rev, x) # passes
#' testthat::expect_failure(expect_involutory(sort, x)) # fails

expect_involutory <- function(f, x, tolerance = testthat::testthat_tolerance(), ...) {
  f_quo <- rlang::enquo(f)
  x_quo <- rlang::enquo(x)
  check_iterated_function_property(
    f = f, x = x, property = "involutory", applications = 2,
    tolerance = tolerance, f_quo = f_quo, x_quo = x_quo, ...
  )
}

#' Do you expect a function to be \eqn{n}-involutory?
#'
#' @description This function tests whether a function, \eqn{f}, applied \eqn{n}
#'   times on \eqn{x} returns \eqn{x}.
#'
#' @param f function. The function you are testing.
#' @param x parameter(s). The value under which you test the function `f`.
#' @param n integer. The expected order of involution.
#' @param tolerance Passed on to [waldo::compare()]. If non-null, ignores small
#' floating point differences
#' @param ... Additional arguments passed into [waldo::compare()]
#'
#' @family function properties
#'
#' @export
#'
#' @details Tests that \eqn{f^n(x) = x}. For `n` = 2, this is equivalent to
#'   `expect_involutory`.
#'
#' @examples
#' mult_by_im <- function(x) {
#'   x * 1i
#' }
#'
#' expect_n_involutory(mult_by_im, 1, n = 4)
#'
expect_n_involutory <- function(f, x, n, tolerance = testthat::testthat_tolerance(), ...) {
  f_quo <- rlang::enquo(f)
  x_quo <- rlang::enquo(x)
  check_iterated_function_property(
    f = f, x = x, property = "n_involutory", applications = n,
    tolerance = tolerance, f_quo = f_quo, x_quo = x_quo, ...
  )
}

#' Do you expect a function to be periodic?
#'
#' @description This function tests whether a function, \eqn{f}, is periodic on
#'  \eqn{x} with period \eqn{T}
#'
#' @param f function. The function you are testing to be periodic.
#' @param x parameter(s). The value under which you test the function `f`.
#' @param period numeric. The expected period of \eqn{f(x)}
#' @param tolerance Passed on to [waldo::compare()]. If non-null, ignores small
#' floating point differences
#' @param ... Additional arguments passed into [waldo::compare()]
#'
#' @family function properties
#'
#' @export
#'
#' @details Tests that \eqn{f(x+T) = f(x)}
#'
#' @examples
#' expect_periodic(cos, c(0, 1, 2), 2*pi)
#' expect_periodic(cos, c(0, 1, 2), 4*pi)
#' expect_periodic(cos, c(pi/2), pi)

expect_periodic <- function(f, x, period, tolerance = testthat::testthat_tolerance(), ...) {

  f_act <- testthat::quasi_label(rlang::enquo(f))
  x_act <- testthat::quasi_label(rlang::enquo(x))
  T_act <- testthat::quasi_label(rlang::enquo(period))

  once_lab <- sprintf("%s(%s)", f_act$lab, x_act$lab)
  period_lab <- sprintf("%s(%s + %s)", f_act$lab, x_act$lab, T_act$lab)

  if (!is.function(f)) {
    testthat::fail(sprintf("Expected %s to be a function.", f_act$lab))
    return(invisible(FALSE))
  }

  res1 <- try_apply_function(f, x)
  res2 <- try_apply_function(f, x + period)

  if (!is.null(res1$error)) {
    testthat::fail(
      c(
        sprintf("Could not apply %s to %s.", f_act$lab, x_act$lab),
        "",
        sprintf("Details: %s", conditionMessage(res1$error))
      )
    )

    return(invisible(FALSE))
  }

  if (!is.null(res2$error)) {
    testthat::fail(
      c(
        sprintf("Could not compute %s", period_lab),
        "",
        sprintf("Details: %s", conditionMessage(res1$error))
      )
    )

    return(invisible(FALSE))
  }

  comparison <- waldo::compare(
    res1$val,
    res2$val,
    ...,
    tolerance = tolerance,
    x_arg = once_lab,
    y_arg = period_lab
  )

  if (length(comparison) == 0) {
    testthat::pass()
  } else {
    testthat::fail(
      c(
        sprintf(
          "Expected %s to be periodic with period %s",
          f_act$lab,
          T_act$lab
        ),
        "",
        "Periodic functions require:",
        "",
        sprintf(
          "    %s == %s",
          once_lab,
          period_lab
        ),
        "",
        "Differences:",
        "",
        comparison
      )
    )
  }

}
