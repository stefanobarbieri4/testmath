try_apply_function <- function(fun, val, fun_lab, val_lab, context = "") {
  tryCatch(
    {
      fun(val)
    },
    error = function(e) {
      testthat::fail(
        c(
          sprintf("Could not apply %s to %s", fun_lab, val_lab),
          context,
          sprintf("\nDetails: %s", conditionMessage(e))
        )
      )
    }
  )
}

expect_idempotent <- function(f, x, ...) {

  # Capture labels before evaluation
  f_act <- testthat::quasi_label(rlang::enquo(f))
  x_act <- testthat::quasi_label(rlang::enquo(x))
  once_lab <- sprintf("%s(%s)", f_act$lab, x_act$lab)
  twice_lab <- sprintf("%s(%s)", f_act$lab, once_lab)

  if (!is.function(f)) {
    testthat::fail(
      sprintf(
        "Expected %s to be a function.",
        f_act$lab
      )
    )
  }

  once <- try_apply_function(f, x, f_act$lab, x_act$lab)
  twice <- try_apply_function(f, once, f_act$lab, once_lab, context = sprintf("\nThe first application, %s, succeeded but its result could not be applied to %s again.", once_lab, f_act$lab))

  comparison <- waldo::compare(
    twice,
    once,
    ...,
    x_arg = twice_lab,
    y_arg = once_lab
  )

  if (length(comparison) == 0) {
    testthat::pass()
  } else {
    msg <- paste(
      sprintf("Expected %s to be idempotent on %s.", f_act$lab, x_act$lab),
      sprintf("\nIdempotence requires:\n\n     %s == %s\n\n", twice_lab, once_lab),
      "Differences:\n",
      comparison
    )

    testthat::fail(msg)
  }

}

## TODO:
##  -- create twice_lab and once_lab variables to stop recomputing
##  -- refactor computing of values into helper function


expect_idempotent("hello!", 2)

expect_idempotent(sum, "hello")

f <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  return(paste("The number is", x))  # Returns a character string
}

expect_idempotent(f, 5)

g <- function(x) x + 1
expect_idempotent(g, 2)

expect_idempotent(mean, c(1,2,3))

