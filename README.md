<div align="right">

[![Check Status](https://github.com/stefanobarbieri4/testmath/actions/workflows/check.yaml/badge.svg)](https://github.com/stefanobarbieri4/testmath/actions/workflows/check.yaml)

</div>

# `testmath`

The `testmath` package aims to extend functions from `testthat` (and `waldo`!) 
to create clear and concise unit tests for mathematical properties. 

- Provides feedback when unit tests fail, specifying the condition leading to 
failure.

- Integrates into the `testthat` framework.

- Helps make unit tests clearer.

## Installation

At the moment, this package is only available on Github. If you would like to 
install it, use:

```
pak::pak("stefanobarbieri4/testmath")
```

## Usage

If you would like to access `testmath`'s suite of functions within your unit
tests:

- Add `testmath` under Suggests in your `DESCRIPTION` file
- In `tests/testthat.R` and add a `library(testmath)` call somewhere. If you 
prefer to leave this file as default, you can create a helper file in 
`tests/testthat/` and load the package in there.

## Functions
Below is a list of current unit tests. If you're unsure of how to use them, 
they each have a help page that can be accessed via:

```
?expect_<property>
```

### Example usage

```r
library(testthat)
library(testmath)

test_that("My matrix is Symmetric Positive Definite", {
  mat <- matrix(c(2, 1, 1, 2), nrow = 2)
  expect_spd(mat)
})

```

### Matrix Properties

- `expect_diagonal()`
- `expect_orthogonal()`
- `expect_symmetric()`
- `expect_psd()`
- `expect_spd()`

### Function Properties

- `expect_idempotent()`
- `expect_involutory()`
- `expect_n_idempotent()`
- `expect_n_involutory()`
- `expect_periodic()`


