test_that("`expect_orthogonal` succeeds for orthogonal matrices", {

  # Diagonal, rotation and reflection matrices are orthogonal

  expect_success(expect_orthogonal(diag(3)))

  theta <- pi / 4
  rot <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow = 2)
  expect_success(expect_orthogonal(rot))

  ref <- matrix(c(1, 0, 0, -1), nrow = 2)
  expect_success(expect_orthogonal(ref))
})

test_that("`expect_orthogonal` fails for non-orthogonal matrices", {
  mat_bad <- matrix(1:4, nrow = 2)

  expect_failure(
    expect_orthogonal(mat_bad),
    "Expected `mat_bad` to be orthogonal"
  )
})

test_that("`expect_orthogonal` fails early for non-matrix and non-square inputs", {

  expect_failure(
    expect_orthogonal(c(1, 0, 0, 1)),
    "to be a matrix"
  )

  mat_rect <- matrix(1:6, nrow = 2, ncol = 3)
  expect_failure(
    expect_orthogonal(mat_rect),
    "to be square"
  )
})

test_that("`expect_orthogonal` respects custom tolerance for floating-point noise", {

  theta <- pi / 4

  R_noisy <- matrix(
    c(cos(theta) + 1e-5, -sin(theta), sin(theta), cos(theta)),
    nrow = 2
  )

  expect_failure(expect_orthogonal(R_noisy))

  expect_success(expect_orthogonal(R_noisy, tolerance = 1e-4))
})
