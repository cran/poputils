
## 'invlogit_inner' -----------------------------------------------------------

test_that("'invlogit_inner' works with valid inputs", {
    x <- c(0, -10, -Inf, Inf, NA)
    ans_obtained <- invlogit_inner(x)
    ans_expected <- c(0.5, exp(-10) / (1 + exp(-10)), 0, 1, NA)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'invlogit_inner' and 'logit_inner' are inverses", {
    set.seed(0)
    p <- runif(n = 10)
    expect_equal(invlogit_inner(logit_inner(p)), p)
})


## 'logit_inner' --------------------------------------------------------------

test_that("'logit_inner' works with valid inputs", {
    p <- c(0.5, 0.1, 0, 1, NA)
    ans_obtained <- logit_inner(p)
    ans_expected <- c(0, log(0.1/0.9), -Inf, Inf, NA)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'logit_inner' gives expected errors with values outside [0, 1]", {
    expect_error(logit_inner(-1),
                 "'p' has value less than 0.")
    expect_error(logit_inner(1.00000000001),
                 "'p' has value greater than 1.")
})

