test_that("`expect_periodic` succeeds for periodic functions", {
  expect_success(expect_periodic(cos, c(0, 1, 2), 2 * pi))
  expect_success(expect_periodic(sin, c(0, 1, 2), 2 * pi))
  expect_success(expect_periodic(tan, c(0.1, 0.5, 1.0), pi))

  # Custom modulo function (f(x) = x mod 2 is 2-periodic)
  f_mod <- function(x) x %% 2
  expect_success(expect_periodic(f_mod, c(1, 3, 5), period = 2))
})

test_that("`expect_periodic` fails for non-periodic functions or wrong periods", {
  # Cosine is NOT pi periodic
  expect_failure(
    expect_periodic(cos, c(0, 1, 2), period = pi),
    "Expected `cos` to be periodic"
  )

  # Identity function is not periodic
  expect_failure(
    expect_periodic(identity, c(1, 2, 3), period = 2),
    "Expected `identity` to be periodic"
  )

  # Linear function is not periodic
  f_linear <- function(x) x + 1
  expect_failure(
    expect_periodic(f_linear, c(1, 2, 3), period = 2),
    "Expected `f_linear` to be periodic"
  )
})

test_that("`expect_periodic` respects custom tolerance", {
  f_noisy_cos <- function(x) cos(x) + (x * 1e-03)

  # Should fail with strict default tolerance
  expect_failure(expect_periodic(f_noisy_cos, 1, 2 * pi))

  # Should pass with relaxed tolerance
  expect_success(expect_periodic(f_noisy_cos, 1, 2 * pi, tolerance = 0.02))
})

test_that("`expect_periodic` fails for non-functions", {
  expect_failure(
    expect_periodic("hello", c(1, 2, 3), period = 2),
    "to be a function"
  )

  expect_failure(
    expect_periodic(data.frame(1), c(1, 2, 3), period = 2),
    "to be a function"
  )
})

test_that("`expect_periodic` fails when f cannot be applied to x or x + period", {
  what_number <- function(x) {
    if (!is.numeric(x)) stop("Must be numeric.")
    paste("Number:", x)
  }

  # Applying to a string will cause f(x) or f(x + period) to throw an error
  expect_failure(
    expect_periodic(what_number, "hello", period = 1),
    "Could not apply"
  )
})
