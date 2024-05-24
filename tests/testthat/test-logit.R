
## 'logit' --------------------------------------------------------------------

test_that("'logit' works with valid inputs - numeric", {
    expect_equal(logit(c(0, 0.5, NA, 1)),
                 c(-Inf, 0, NA, Inf))
    expect_equal(logit(numeric()),
                 numeric())
})

test_that("'logit' works with valid inputs - numeric", {
    expect_equal(logit(matrix(c(0, 0.5, NA, 1),
                              nr = 2)),
                 matrix(c(-Inf, 0, NA, Inf),
                        nr = 2))
    expect_equal(logit(matrix(numeric(), nrow = 0)),
                 matrix(numeric(), nrow = 0))
})

test_that("'logit' works with valid inputs - rvec", {
    expect_equal(logit(rvec::rvec(matrix(c(0, 0.5, NA, 1),
                                         nr = 2))),
                 rvec::rvec(matrix(c(-Inf, 0, NA, Inf),
                                   nr = 2)))
    expect_equal(logit(rvec::rvec_dbl()),
                 rvec::rvec_dbl())
})

test_that("'invlogit' gives correct message when is not numeric", {
    expect_error(logit("a"),
                 "`p` is not numeric.")
})


## 'invlogit' --------------------------------------------------------------------

test_that("'invlogit' works with valid inputs - numeric", {
    expect_equal(invlogit(logit(c(0, 0.5, NA, 1))),
                 c(0, 0.5, NA, 1))
    expect_equal(invlogit(numeric()),
                 numeric())
})

test_that("'invlogit' works with valid inputs - numeric", {
    expect_equal(invlogit(logit(matrix(c(0, 0.5, NA, 1),
                                       nr = 2))),
                 matrix(c(0, 0.5, NA, 1),
                        nr = 2))
    expect_equal(invlogit(matrix(numeric(), nrow = 0)),
                 matrix(numeric(), nrow = 0))
})

test_that("'invlogit' works with valid inputs - rvec", {
    expect_equal(invlogit(logit(rvec::rvec(matrix(c(0, 0.5, NA, 1),
                                                  nr = 2)))),
                 rvec::rvec(matrix(c(0, 0.5, NA, 1),
                                   nr = 2)))
    expect_equal(invlogit(rvec::rvec_dbl()),
                 rvec::rvec_dbl())
})

test_that("'invlogit' gives correct message when is not numeric", {
    expect_error(invlogit("a"),
                 "`x` is not numeric.")
})

test_that("'invlogit' gives correct message when is not atomic, matrix, or rvec", {
    expect_error(invlogit(list(1)),
                 "`x` has class")
})



                       
    
                          
