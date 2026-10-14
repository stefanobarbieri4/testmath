test_that("`expect_psd` succeeds for positive semidefinite matrices", {

  expect_success(expect_psd(diag(3)))

  # Strict SPD matrices are also valid PSD matrices
  mat_spd <- matrix(c(4, 1, 1, 3), nrow = 2)
  expect_success(expect_psd(mat_spd))

  # Matrix with a zero eigenvalue (PSD, but not strictly PD) - eig = 2, 0
  mat_psd_zero <- matrix(c(1, 1, 1, 1), nrow = 2)
  expect_success(expect_psd(mat_psd_zero))
})

test_that("`expect_psd` fails for matrices with negative eigenvalues", {

  # Indefinite matrix with a negative eigenvalue (eigenvalues: 3 and -1)

  mat_indefinite <- matrix(c(1, 2, 2, 1), nrow = 2)

  expect_failure(
    expect_psd(mat_indefinite),
    "Expected `mat_indefinite` to be positive semidefinite"
  )
})

test_that("`expect_psd` fails early for asymmetric, non-matrix, or non-square inputs", {

  mat_asym <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_failure(
    expect_psd(mat_asym),
    "to be symmetric"
  )

  expect_failure(
    expect_psd(c(1, 1, 1, 1)),
    "to be a matrix"
  )

  mat_rect <- matrix(1:6, nrow = 2, ncol = 3)
  expect_failure(
    expect_psd(mat_rect),
    "to be square"
  )
})

test_that("`expect_psd` respects custom tolerance for floating-point noise", {

  mat_noisy_psd <- matrix(c(1, 1, 1, 1 - 1e-5), nrow = 2)

  expect_failure(expect_psd(mat_noisy_psd))

  expect_success(expect_psd(mat_noisy_psd, tolerance = 1e-4))
})
