test_that("`expect_n_idempotent` succeeds when functions reach stability at order n", {
  # A custom function that takes multiple steps to reach a stable state
  # e.g., applying a dampening or boundary-clipping operation
  # Let's use a custom mapping where f^3(x) == f(x)
  step_func <- function(x) {
    pmin(pmax(x, -2), 2)
  }

  # Standard idempotence (n=2)
  expect_success(expect_n_idempotent(step_func, c(-5, 0, 5), n = 2))

  # Higher-order stabilization check
  expect_success(expect_n_idempotent(step_func, c(-5, 0, 5), n = 3))
})

test_that("`expect_n_idempotent` fails when stability is not reached at order n", {
  f_growing <- function(x) x * 2

  expect_failure(
    expect_n_idempotent(f_growing, 2, n = 3),
    "Expected `f_growing` to be 3-idempotent"
  )
})

test_that("`expect_n_idempotent` fails for non-functions and application errors", {
  expect_failure(
    expect_n_idempotent(123, 2, n = 2),
    "to be a function"
  )
})
