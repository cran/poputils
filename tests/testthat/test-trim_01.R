
## 'trim_01' ------------------------------------------------------------------

test_that("'trim_01' works with valid inputs - numeric", {
  x <- c(0.5,  0, -0.1, NA, 1, Inf, 2, NaN, 0.4)
  ans_obtained <- trim_01(x)
  ans_expected <- c(0.5, 0.4, 0.4, NA, 0.5, 0.5, 0.5, NaN, 0.4)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' works with valid inputs - rvec", {
  x <- rvec::rvec(matrix(c(0.5,  0, -0.1, NA,
                           1, Inf, NaN, 0.4),
                         nr = 4))
  ans_obtained <- trim_01(x)
  ans_expected <- rvec::rvec(matrix(c(0.5, 0.4, 0.4, NA,
                                      0.5, 0.5, NaN, 0.4),
                                    nr = 4))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' works when x all NA", {
  x <- rep(NA_real_, 5)
  ans_expected <- trim_01(x)
  ans_obtained <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' works when x has length 0", {
  x <- numeric()
  ans_expected <- trim_01(x)
  ans_obtained <- x
  expect_identical(ans_obtained, ans_expected)
})

test_that("'trim_01' throws correct error when non-numeric", {
  x <- c(0, -0.1, NA, 1, Inf, 2, "a")
  expect_error(trim_01(x),
               "`x` is non-numeric.")
  x <- rvec::rvec(matrix("a"))
  expect_error(trim_01(x),
               "`x` is non-numeric.")
})

test_that("'trim_01' throws correct error when need to truncate but cannot", {
  x <- c(0, -0.1, NA, 1, Inf, 2, NaN)
  expect_error(trim_01(x),
               "Unable to calculate truncated values.")
  x <- c(0, NA)
  expect_error(trim_01(x),
               "Unable to calculate truncated values.")
})

