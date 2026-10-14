test_that("`expect_spd` succeeds for symmetric positive definite matrices", {
  # Identity matrix is SPD
  expect_success(expect_spd(diag(3)))

  # Custom 2x2 SPD matrix
  mat_spd <- matrix(c(4, 1, 1, 3), nrow = 2)
  expect_success(expect_spd(mat_spd))
})

test_that("`expect_spd` fails for symmetric matrices with non-positive eigenvalues", {
  # Symmetric, but has a negative eigenvalue
  mat_indefinite <- matrix(c(1, 2, 2, 1), nrow = 2)

  expect_failure(
    expect_spd(mat_indefinite),
    "Expected `mat_indefinite` to be symmetric positive definite"
  )
})

test_that("`expect_spd` fails early for asymmetric or non-square matrices", {
  mat_asym <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_failure(
    expect_spd(mat_asym),
    "to be symmetric"
  )

  mat_rect <- matrix(1:6, nrow = 2, ncol = 3)
  expect_failure(
    expect_spd(mat_rect),
    "to be square"
  )
})
