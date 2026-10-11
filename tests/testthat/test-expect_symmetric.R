test_that("`expect_symmetric` succeeds for symmetric matrices", {
  mat1 <- matrix(c(1, 2, 2, 1), nrow = 2)
  expect_success(expect_symmetric(mat1))

  # 1x1 matrix is trivially symmetric
  expect_success(expect_symmetric(matrix(5, nrow = 1)))
})

test_that("`expect_symmetric` fails for asymmetric matrices", {
  mat_asym <- matrix(c(1, 2, 3, 4), nrow = 2)
  expect_failure(
    expect_symmetric(mat_asym),
    "Expected `mat_asym` to be symmetric"
  )
})

test_that("`expect_symmetric` fails for non-matrix and non-square inputs", {
  expect_failure(
    expect_symmetric(c(1, 2, 3)),
    "to be a matrix"
  )

  mat_rect <- matrix(1:6, nrow = 2, ncol = 3)
  expect_failure(
    expect_symmetric(mat_rect),
    "to be square"
  )
})

test_that("`expect_symmetric` respects custom tolerance for floating-point noise", {
  # Minor perturbation
  mat_noisy <- matrix(c(1, 2 + 1e-5, 2, 1), nrow = 2)

  # Should fail under default strict tolerance
  expect_failure(expect_symmetric(mat_noisy))

  # Should pass under a relaxed tolerance
  expect_success(expect_symmetric(mat_noisy, tolerance = 1e-4))
} )
