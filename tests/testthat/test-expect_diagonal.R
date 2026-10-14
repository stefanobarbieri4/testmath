test_that("`expect_diagonal` succeeds for diagonal matrices", {

  expect_success(expect_diagonal(diag(3)))

  mat_diag <- diag(c(2, -4, 3.5))
  expect_success(expect_diagonal(mat_diag))

  expect_success(expect_diagonal(matrix(7, nrow = 1)))
})

test_that("`expect_diagonal` fails for non-diagonal matrices", {

  mat_not_diag <- matrix(c(1, 2, 0, 3), nrow = 2)

  expect_failure(
    expect_diagonal(mat_not_diag),
    "Expected `mat_not_diag` to be a diagonal matrix"
  )
})

test_that("`expect_diagonal` fails early for non-matrix and non-square inputs", {

  expect_failure(
    expect_diagonal(c(1, 2, 3)),
    "to be a matrix"
  )

  mat_rect <- matrix(1:6, nrow = 2, ncol = 3)
  expect_failure(
    expect_diagonal(mat_rect),
    "to be square"
  )
})

test_that("`expect_diagonal` respects custom tolerance for floating-point noise", {

  mat_noisy_diag <- matrix(c(1, 1e-5, 0, 3), nrow = 2)

  expect_failure(expect_diagonal(mat_noisy_diag))

  expect_success(expect_diagonal(mat_noisy_diag, tolerance = 1e-4))
})
