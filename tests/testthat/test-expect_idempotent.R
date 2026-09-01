f <- function(x) 5 # constant functions are idempotent
g <- function(x) x^2 # only idempotent for certain values of x

what_number <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  return(paste("The number is", x))  # returns a character string
}


test_that("`expect_idempotent` succeeds for idempotent functions", {

  expect_success(expect_idempotent(mean, c(1, 2, 3)))
  expect_success(expect_idempotent(sort, c(5, 1, 2, 4)))
  expect_success(expect_idempotent(abs, -100))
  expect_success(expect_idempotent(f, 100))

})

test_that("`expect_idempotent fails for non-idempotent functions", {

  expect_failure(
    expect_idempotent(rev, c(1, 2, 3)),
    "Expected `rev` to be idempotent"
  )

  expect_failure(
    expect_idempotent(runif, 100),
    "Expected `runif` to be idempotent"
  )

})

test_that("`expect_idempotent` can succeed and fail for specific values of non-idempotent functions", {

  expect_success(expect_idempotent(g, 0))
  expect_success(expect_idempotent(g, 1))

  expect_failure(
    expect_idempotent(g, 2),
    "Expected `g` to be idempotent on 2"
  )

})


test_that("`expect_idempotent` fails for non-functions", {

  expect_failure(
    expect_idempotent("hello", 2),
    "to be a function"
  )

  expect_failure(
    expect_idempotent(data.frame(1), 2),
    "to be a function"
  )

})

test_that("`expect_idempotent` fails when f can not be applied to x", {

  expect_failure(
    expect_idempotent(sum, "hello"),
    "Could not apply `sum` to \"hello\""
  )

  expect_failure(
    expect_idempotent(what_number, TRUE),
    "Could not apply `what_number` to TRUE"
  )

})

test_that("expect_idempotent` fails when f cannot be applied to x twice", {

  expect_failure(
    expect_idempotent(what_number, 5),
    "result could not be applied to `what_number` again"
  )

})

