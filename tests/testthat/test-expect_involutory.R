test_that("`expect_involutory` succeeds for involutory (self-inverse) functions", {
  expect_success(expect_involutory(rev, c(1, 2, 3)))

  negate <- function(x) -x
  expect_success(expect_involutory(negate, c(-5, 0, 5)))

  reciprocal <- function(x) 1 / x
  expect_success(expect_involutory(reciprocal, c(2, 0.5, -4)))

  logical_not <- function(x) !x
  expect_success(expect_involutory(logical_not, c(TRUE, FALSE)))
})

test_that("`expect_involutory` fails for non-involutory functions", {
  expect_failure(
    expect_involutory(sort, c(3, 1, 2)),
    "Expected `sort` to be involutory"
  )

  expect_failure(
    expect_involutory(sqrt, c(4, 9, 16)),
    "Expected `sqrt` to be involutory"
  )
})

test_that("`expect_involutory` respects custom tolerance", {
  f_noisy_inv <- function(x) (1 / x) + 1e-3

  # Should fail with strict default tolerance
  expect_failure(expect_involutory(f_noisy_inv, 2))

  # Should pass with relaxed tolerance
  expect_success(expect_involutory(f_noisy_inv, 2, tolerance = 1e-2))
})

test_that("`expect_involutory` fails for non-functions and application errors", {
  expect_failure(
    expect_involutory("not a function", 2),
    "to be a function"
  )

  what_number <- function(x) {
    if (!is.numeric(x)) stop("Must be numeric.")
    x
  }

  expect_failure(
    expect_involutory(what_number, "hello"),
    "Could not apply"
  )
})
