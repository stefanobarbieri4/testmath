test_that("`expect_n_involutory` succeeds for order-n cyclic functions", {
  # Multiplying by imaginary unit 1i cycles every 4 steps: (1i)^4 = 1
  mult_by_im <- function(x) x * 1i
  expect_success(expect_n_involutory(mult_by_im, 1, n = 4))

  # Left cyclic shift on a vector of length 3 cycles every 3 steps
  shift_left <- function(x) c(x[-1], x[1])
  expect_success(expect_n_involutory(shift_left, c(1, 2, 3), n = 3))
})

test_that("`expect_n_involutory` fails for incorrect orders or non-cyclic functions", {
  mult_by_im <- function(x) x * 1i

  # Wrong order (should fail because (1i)^2 = -1 != 1)
  expect_failure(
    expect_n_involutory(mult_by_im, 1, n = 2),
    "Expected `mult_by_im` to be 2-involutory"
  )

  # Linear function is not cyclic
  f_linear <- function(x) x + 1
  expect_failure(
    expect_n_involutory(f_linear, 1, n = 4),
    "Expected `f_linear` to be 4-involutory"
  )
})

test_that("`expect_n_involutory` handles error states gracefully", {
  expect_failure(
    expect_n_involutory("not a function", 1, n = 3),
    "to be a function"
  )
})
