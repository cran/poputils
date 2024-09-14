
## 'is_ax_le_nx' --------------------------------------------------------------

test_that("'is_ax_le_nx' works with valid inputs", {
    expect_true(is_ax_le_nx(0, "0"))
    expect_true(is_ax_le_nx(1, "single"))
    expect_true(is_ax_le_nx(0.5, "five"))
    expect_false(is_ax_le_nx(33, "five"))
    expect_true(is_ax_le_nx(33, "open"))
    expect_true(is_ax_le_nx(NA_real_, "single"))
    expect_true(is_ax_le_nx(-1, "1-4"))
    expect_identical(is_ax_le_nx(c(-1, 1), c("1-4", "1-4")), c(TRUE, TRUE))
})


## 'Lx_to_ex' -----------------------------------------------------------------

test_that("'Lx_to_ex' works with no NAs", {
    set.seed(0)
    Lx <- matrix(runif(n = 20, max = 5), nrow = 5)
    lx <- matrix(runif(n = 20, max = 5), nrow = 5)
    ans_obtained <- Lx_to_ex(Lx = Lx, lx = lx)
    ans_expected <- apply(Lx[5:1,], 2, cumsum)[5:1,] / lx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'Lx_to_ex' works with NAs", {
    set.seed(0)
    Lx <- matrix(runif(n = 20, max = 5), nrow = 5)
    Lx[2,3] <- NA
    lx <- matrix(runif(n = 20, max = 5), nrow = 5)
    ans_obtained <- Lx_to_ex(Lx = Lx, lx = lx)
    ans_expected <- apply(Lx[5:1,], 2, cumsum)[5:1,] / lx
    expect_equal(ans_obtained, ans_expected)
    expect_identical(sum(is.na(ans_obtained)), 2L)
})


## 'lx_to_dx' -----------------------------------------------------------------

test_that("'lx_to_dx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- rbind(lx[-5,] - lx[-1,], lx[5,])
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_dx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    lx[2, 1] <- NA
    lx[4, 2] <- NA
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- rbind(lx[-5,] - lx[-1,], lx[5,])
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_qx' works with single row", {
    lx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- lx_to_dx(lx)
    ans_expected <- lx
    expect_equal(ans_obtained, ans_expected)
})


## 'lx_to_qx' -----------------------------------------------------------------

test_that("'lx_to_qx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    lx <- qx_to_lx(qx)
    expect_identical(lx_to_qx(lx), qx)
})

test_that("'lx_to_qx' works with NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    qx[1, 3] <- NA_real_
    qx[4, 2] <- NA_real_
    lx <- qx_to_lx(qx)
    ans_obtained <- lx_to_qx(lx)
    ans_expected <- rbind(1 - (lx[-1,] / lx[-5,]), 1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'lx_to_qx' works with single row", {
    lx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- lx_to_qx(lx)
    ans_expected <- lx
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_ex' -----------------------------------------------------------------

test_that("'mx_to_ex' agrees with example from MEASURE Evaluation", {
    ## Mortality rates and life expectancy taken from https://www.measureevaluation.org/resources/training/online-courses-and-resources/non-certificate-courses-and-mini-tutorials/multiple-decrement-life-tables/lesson-3.html
    mx <- c(0.07505,
            0.00701,
            0.00171,
            0.00128,
            0.00129,
            0.00181,
            0.00163,
            0.00198,
            0.00302,
            0.00442,
            0.00645,
            0.00923,
            0.01344,
            0.02364,
            0.03633,
            0.05182,
            0.07644,
            0.13520,
            0.33698)
    mx <- matrix(mx, ncol = 1)
    age_group_categ <- c("0", "1-4", rep("five", length(mx) - 3), "open")
    ax <- rep(NA_real_, times = length(age_group_categ))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_categ = age_group_categ,
                             sex = rep(NA_character_, times = nrow(mx)),
                             ax = ax,
                             methods = c(infant = "constant",
                                         child = "constant",
                                         closed = "constant",
                                         open = "constant"))
    ans_expected <- matrix(62.97331, nr = 1)
    expect_equal(ans_obtained, ans_expected, tolerance = 0.0001) ## MEASURE calculations involve rounding
})

test_that("'mx_to_ex' handles NA as expected", {
    mx <- c(0.07505,
            0.00701,
            0.00171,
            0.00128,
            0.00129,
            0.00181,
            0.00163,
            0.00198,
            0.00302,
            NA_real_,
            0.00645,
            0.00923,
            0.01344,
            0.02364,
            0.03633,
            0.05182,
            0.07644,
            0.13520,
            0.33698)
    mx <- matrix(mx, ncol = 1)
    age_group_categ <- c("0", "1-4", rep("five", length(mx) - 3), "open")
    ax <- rep(NA_real_, times = length(age_group_categ))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_categ = age_group_categ,
                             sex = NA_character_,
                             ax = ax,
                             methods = c(infant = "constant",
                                         child = "constant",
                                         closed = "constant",
                                         open = "constant"))
    ans_expected <- matrix(NA_real_, nr = 1)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, with ax supplied, all constant", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = rep(NA_character_, 4),
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "constant",
                                         child = "constant",
                                         closed = "constant",
                                         open = "constant"))
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), 1 - (5 * mx[3,])/(1 + 2 * mx[3,]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex_const' gives correct answer, with all constant, mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = rep(NA_character_, 4),
                             ax = c(NA_real_, NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "constant",
                                         child = "constant",
                                         closed = "constant",
                                         open = "constant"))
    px <- rbind(exp(-mx[1, ]), exp(-4 * mx[2, ]), exp(-5 * mx[3, ]))
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    Lx <- rbind((lx[-4,] / mx[-4,]) - (lx[-1,] / mx[-4,]), (lx[4, ] / mx[4, ]))
    Lx[3,2] <- lx[3,2] * 5
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, with mx with single row", {
    mx <- matrix(c(0.2, 0.3), nr = 1)
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group_categ = "open",
                             sex = NA_character_,
                             ax = NA_real_,
                             methods = c(infant = "constant",
                                         child = "constant",
                                         closed = "constant",
                                         open = "constant"))
    ans_expected <- 1 / mx
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = CD, child = CD, closed = linear, Female, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.023, 0.25),
                c(0.2, 0.04, 0.43))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = rep("Female", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "CD",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = CD, child = CD, closed = linear, Female, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = rep("Female", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.053 + 2.8 * mx[1,]
    a1 <- 1.522 - 1.518 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = CD, child = CD, closed = linear, Male, m0 >= 0.107", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})


test_that("'mx_to_ex' gives correct answer - infant = CD, child = CD, closed = linear, Male, m0 < 0.107", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.045 + 2.684 * mx[1, ]
    a1 <- 1.651 - 2.816 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                4 * lx[3,] + a1 * (lx[2,] - lx[3,]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, m0 >= 0.06891", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.31411
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, 0.01724 <= m0 < 0.06891", {
    mx <- cbind(c(0.01724, 0.01, 0.25),
                c(0.02, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.04667 + 3.88089 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, m0 < 0.01724", {
    mx <- cbind(c(0.01723, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Female", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.14903 - 2.05527 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - 3+ age groups, infant = AK, child = linear, closed = linear, Male, m0 >= 0.08307", {
    mx <- cbind(c(0.08307, 0.01, 0.25),
                c(0.25, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.29915
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Male, 0.023 <= m0 < 0.08307", {
    mx <- cbind(c(0.023, 0.01, 0.25),
                c(0.05, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.02832 + 3.26021 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear,  Male, m0 < 0.0230", {
    mx <- cbind(c(0.02299, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- mx_to_ex(mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.14929 - 1.99545 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]),
                mx[3, ] / (1 + 0.5 * mx[3, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]),
                lx[3, ] / mx[3, ])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, all linear", {
    mx <- cbind(c(0.011, 0.01, 0.05, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = NA_character_,
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "linear",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, with mx of 0", {
    mx <- cbind(c(0.011, 0.01, 0, 0.25),
                c(0.012, 0.015, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = NA_character_,
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "linear",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, all linear, with mx of NA", {
    mx <- cbind(c(0.011, 0.01, 0.2, 0.25),
                c(0.012, NA, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("0", "1-4", "five", "open"),
                             sex = rep(NA_character_, 4),
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "linear",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    ax <- matrix(rep(c(0.5, 2, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(mx[1,] / (1 + 0.5 * mx[1,]),
                4 * mx[2,] / (1 + 2 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(1, 4, 5), lx[4,]/mx[4,])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, starting age is 1", {
    mx <- cbind(c(0.011, 0.01, 0.2, 0.25),
                c(0.012, NA, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("1-4", "five", "five", "open"),
                             sex = rep(NA_character_, 4),
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "linear",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    ax <- matrix(rep(c(2, 2.5, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(4 * mx[1,] / (1 + 2 * mx[1,]),
                5 * mx[2,] / (1 + 2.5 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(4, 5, 5), lx[4,]/mx[4,])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_ex' gives correct answer, starting age is 5", {
    mx <- cbind(c(0.011, 0.01, 0.2, 0.25),
                c(0.012, NA, 0.06, 0.4))
    ans_obtained <- mx_to_ex(mx = mx,
                             age_group = c("five", "five", "five", "open"),
                             sex = rep(NA_character_, 4),
                             ax = c(NA_real_, NA_real_, 3, NA_real_),
                             methods = c(infant = "linear",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    ax <- matrix(rep(c(2.5, 2.5, 3, Inf), each = 2), nr = 4, byrow = TRUE)
    qx <- rbind(5 * mx[1,] / (1 + 2.5 * mx[1,]),
                5 * mx[2,] / (1 + 2.5 * mx[2,]),
                5 * mx[3,] / (1 + 2 * mx[3,]),
                1)
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ], px[1, ] * px[2, ] * px[3, ])
    dx <- rbind(lx[-4,] - lx[-1,], lx[4,])
    Lx <- rbind(ax[-4,] * dx[-4,] + lx[-1,] * c(5, 5, 5), lx[4,]/mx[4,])
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_lx' -----------------------------------------------------------------

test_that("'mx_to_lx' gives correct answer - infant CD, closed linear, Female, m0 >= 0.107, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer with very high value for mx", {
    mx <- cbind(c(0.107, 1e6, 0.3),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    qx[qx > 1] <- 1
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})


test_that("'mx_to_lx' gives correct answer - infant CD, closed linear, Female, m0 < 0.107, no ax supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer - infant CD, closed linear, Female, m0 < 0.107, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.5
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer - infant CD, child CD, Female, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    ans_expected[2:3,1] <- NA_real_
    ans_expected[3,2] <- NA_real_
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    ans_expected[3,2] <- 0
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer - infant CD, Male, m0 >= 0.107, no ax, single year", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_lx' gives correct answer - infacnt CD, child CD, Male, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    ans_expected <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    expect_equal(ans_obtained, ans_expected)
})


## 'mx_to_Lx' -----------------------------------------------------------------

test_that("'mx_to_Lx' gives correct answer - infant CD, Female, m0 >= 0.107, no ax supplied", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx * c(a0, 0.5) + lx[-1, ],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, m0 < 0.107, no ax supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.053 + 2.8 * mx[1, ]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, Female, m0 < 0.107, a0 supplied", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.5
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                1 * mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, child CD, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + 4 * lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer with NA", {
    mx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    lx[2:3,1] <- NA_real_
    lx[3,2] <- NA_real_
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer with Inf", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, Inf, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    qx[2,2] <- 1
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer with 0", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, Male, m0 >= 0.107, no ax, single year", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                mx[2, ] / (1 + 0.5 * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,],
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, child CD, Male, m0 >= 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    a1 <- 1.352
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - infant CD, child CD, Female, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.053 + 2.8 * mx[1,]
    a1 <- 1.522 - 1.518 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'mx_to_Lx' gives correct answer - Male, m0 < 0.107, no ax, life table age groups", {
    mx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- mx_to_Lx(mx = mx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 = 0.045 + 2.684 * mx[1,]
    a1 <- 1.651 - 2.816 * mx[1,]
    qx <- rbind(mx[1, ] / (1 + (1 - a0) * mx[1, ]),
                4 * mx[2, ] / (1 + (4 - a1) * mx[2, ]))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4,
                          lx[3, ] / mx[3,])
    expect_equal(ans_obtained, ans_expected)
})


## 'qx_to_ex' -----------------------------------------------------------------

test_that("'qx_to_ex' gives correct answer - infant CD, Female, q0 >= 0.1, no ax supplied", {
    qx <- cbind(c(0.10, 0.01, 1),
                c(0.11, 0.015, 1))
    a0 <- 0.35
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx * c(a0, 0.5) + lx[-1, ])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, q0 < 0.1, no ax supplied", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.05 + 3 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * 0.5 + lx[3,])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, Female, q0 < 0.1, a0 supplied", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.5
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * 0.5 + lx[3,])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, child CD, q0 >= 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.1, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + 4 * lx[3,])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer with NA", {
    qx <- cbind(c(NA_real_, 0.01, 0.25),
                c(0.11, NA_real_, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    lx[2:3,1] <- NA_real_
    lx[3,2] <- NA_real_
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * 0.5 + lx[3,])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer with 1", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 1, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + lx[3,] * 4)
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer with 0", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + lx[3,] * 4)
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, Male, q0 >= 0.1, no ax, single year", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * 0.5 + lx[3,])
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, child CD, Male, q0 >= 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    a1 <- 1.352
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + lx[3,] * 4)
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant CD, child CD, Female, q0 < 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.05 + 3 * qx[1,]
    a1 <- 1.524 - 1.625 * qx[1,]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + lx[3,] * 4)
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - Male, q0 < 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 = 0.0425 + 2.875 * qx[1,]
    a1 <- 1.653 - 3.013 * qx[1,]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    Lx <- rbind(dx[1,] * a0 + lx[2, ],
                dx[2,] * a1 + lx[3,] * 4)
    Lx <- rbind(Lx,
                lx[3, ] / (dx[2,] / Lx[2,]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})


test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Male, 0.0785 <= q0", {
  qx <- cbind(c(0.09, 0.01, 0.25),
              c(0.1, 0.015, 0.4))
  ans_obtained <- qx_to_ex(qx,
                           age_group_categ = c("0", "single", "open"),
                           sex = rep("Male", 3),
                           ax = rep(NA_real_, 3),
                           methods = c(infant = "AK",
                                       child = "linear",
                                       closed = "linear",
                                       open = "constant"))
  a0 <- 0.2991
  px <- 1 - qx
  lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
  Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
              0.5 * (lx[2, ] + lx[3, ]))
  Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
  ans_expected <- matrix(colSums(Lx), nr = 1)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, 0.0658 <= q0", {
  qx <- cbind(c(0.09, 0.01, 0.25),
              c(0.1, 0.015, 0.4))
  ans_obtained <- qx_to_ex(qx,
                           age_group_categ = c("0", "single", "open"),
                           sex = rep("Female", 3),
                           ax = rep(NA_real_, 3),
                           methods = c(infant = "AK",
                                       child = "linear",
                                       closed = "linear",
                                       open = "constant"))
  a0 <- 0.3141
  px <- 1 - qx
  lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
  Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
              0.5 * (lx[2, ] + lx[3, ]))
  Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
  ans_expected <- matrix(colSums(Lx), nr = 1)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, 0.017 <= q0 < 0.0658", {
  qx <- cbind(c(0.017, 0.01, 0.25),
              c(0.018, 0.015, 0.4))
  ans_obtained <- qx_to_ex(qx,
                           age_group_categ = c("0", "single", "open"),
                           sex = rep("Female", 3),
                           ax = rep(NA_real_, 3),
                           methods = c(infant = "AK",
                                       child = "linear",
                                       closed = "linear",
                                       open = "constant"))
  a0 <- 0.0438 + 4.1075 * qx[1,]
  px <- 1 - qx
  lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
  Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
              0.5 * (lx[2, ] + lx[3, ]))
  Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
  ans_expected <- matrix(colSums(Lx), nr = 1)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Female, q0 < 0.017", {
  qx <- cbind(c(0.001, 0.01, 0.25),
              c(0.002, 0.015, 0.4))
  ans_obtained <- qx_to_ex(qx,
                           age_group_categ = c("0", "single", "open"),
                           sex = rep("Female", 3),
                           ax = rep(NA_real_, 3),
                           methods = c(infant = "AK",
                                       child = "linear",
                                       closed = "linear",
                                       open = "constant"))
  a0 <- 0.149 - 2.0867 * qx[1,]
  px <- 1 - qx
  lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
  Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
              0.5 * (lx[2, ] + lx[3, ]))
  Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
  ans_expected <- matrix(colSums(Lx), nr = 1)
  expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear, Male, 0.0226 <= m0 < 0.0785", {
    qx <- cbind(c(0.029, 0.01, 0.25),
                c(0.05, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.0244 + 3.4994 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]))
    Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear,  Male, m0 < 0.0226", {
    qx <- cbind(c(0.022, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.1493 - 2.0367 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]))
    Lx <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - infant = AK, child = linear, closed = linear,  ax supplied for open age group", {
    qx <- cbind(c(0.022, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, 1.8),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.1493 - 2.0367 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]))
    Lx <- rbind(Lx, lx[3,] * 1.8)
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - starting age is 1", {
    qx <- cbind(c(0.022, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx,
                             age_group_categ = c("1-4", "five", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, 1.8),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(2 * (lx[1, ] + lx[2, ]),
                2.5 * (lx[2, ] + lx[3, ]))
    Lx <- rbind(Lx, lx[3,] * 1.8)
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_ex' gives correct answer - starting age is 5", {
    qx <- cbind(c(0.022, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- qx_to_ex(qx,
                             age_group_categ = c("five", "five", "open"),
                             sex = rep("Male", 3),
                             ax = c(NA_real_, NA_real_, 1.8),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(2.5 * (lx[1, ] + lx[2, ]),
                2.5 * (lx[2, ] + lx[3, ]))
    Lx <- rbind(Lx, lx[3,] * 1.8)
    ans_expected <- matrix(colSums(Lx), nr = 1)
    expect_equal(ans_obtained, ans_expected)
})


## 'qx_to_Lx' -----------------------------------------------------------------

test_that("'qx_to_Lx' gives correct answer - infant CD, Female, q0 >= 0.1, no ax supplied", {
    qx <- cbind(c(0.10, 0.01, 1),
                c(0.11, 0.015, 1))
    a0 <- 0.35
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx * c(a0, 0.5) + lx[-1, ])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, q0 < 0.1, no ax supplied", {
    qx <- cbind(c(0.02, 0.01, 1),
                c(0.01, 0.015, 1))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.05 + 3 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, Female, q0 < 0.1, a0 supplied", {
    qx <- cbind(c(0.02, 0.01, 1),
                c(0.01, 0.015, 1))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(0.5, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.5
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, child CD, q0 >= 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.1, 0.01, 1),
                c(0.11, 0.015, 1))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + 4 * lx[3,])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer with NA", {
    qx <- cbind(c(NA_real_, 0.01, 1),
                c(0.11, NA_real_, 1))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer with 1", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 1, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4)
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer with 0", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.35
    a1 <- 1.361
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4)
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, Male, q0 >= 0.1, no ax, single year", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "constant",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * 0.5 + lx[3,])
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, child CD, Male, q0 >= 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.107, 0.01, 0.25),
                c(0.11, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.33
    a1 <- 1.352
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4)
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant CD, child CD, Female, q0 < 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Female", "Female", "Female"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.05 + 3 * qx[1,]
    a1 <- 1.524 - 1.625 * qx[1,]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4)
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - Male, q0 < 0.1, no ax, life table age groups", {
    qx <- cbind(c(0.02, 0.01, 0.25),
                c(0.01, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx = qx,
                             age_group_categ = c("0", "1-4", "open"),
                             sex = c("Male", "Male", "Male"),
                             ax = c(NA_real_, NA_real_, NA_real_),
                             methods = c(infant = "CD",
                                         child = "CD",
                                         closed = "linear",
                                         open = "constant"))
    a0 = 0.0425 + 2.875 * qx[1,]
    a1 <- 1.653 - 3.013 * qx[1,]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    dx <- lx[-3,] - lx[-1,]
    ans_expected <- rbind(dx[1,] * a0 + lx[2, ],
                          dx[2,] * a1 + lx[3,] * 4)
    ans_expected <- rbind(ans_expected,
                          lx[3, ] / (dx[2,] / ans_expected[2,]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant = AK, child = linear, closed = linear, Male, 0.0226 <= m0 < 0.0785", {
    qx <- cbind(c(0.029, 0.01, 0.25),
                c(0.05, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.0244 + 3.4994 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]))
    ans_expected <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_Lx' gives correct answer - infant = AK, child = linear, closed = linear,  Male, m0 < 0.0226", {
    qx <- cbind(c(0.022, 0.01, 0.25),
                c(0.001, 0.015, 0.4))
    ans_obtained <- qx_to_Lx(qx,
                             age_group_categ = c("0", "single", "open"),
                             sex = rep("Male", 3),
                             ax = rep(NA_real_, 3),
                             methods = c(infant = "AK",
                                         child = "linear",
                                         closed = "linear",
                                         open = "constant"))
    a0 <- 0.1493 - 2.0367 * qx[1, ]
    px <- 1 - qx
    lx <- rbind(c(1, 1), px[1, ], px[1, ] * px[2, ])
    Lx <- rbind(lx[2, ] + a0 * (lx[1, ] - lx[2, ]),
                0.5 * (lx[2, ] + lx[3, ]))
    ans_expected <- rbind(Lx, lx[3,] * Lx[2,] / (lx[2, ] - lx[3, ]))
    expect_equal(ans_obtained, ans_expected)
})



## 'qx_to_lx' -----------------------------------------------------------------

test_that("'qx_to_lx' works with no NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    ans_obtained <- qx_to_lx(qx)
    px <- 1 - qx
    ans_expected <- rbind(1,
                          px[1,],
                          px[1,] * px[2,],
                          px[1,] * px[2,] * px[3,],
                          px[1,] * px[2,] * px[3,] * px[4,])
    expect_equal(ans_obtained, ans_expected)
})

test_that("'qx_to_lx' works with NAs", {
    set.seed(0)
    qx <- rbind(matrix(runif(12), nr = 4), 1)
    qx[1, 3] <- NA_real_
    qx[4, 2] <- NA_real_
    ans_obtained <- qx_to_lx(qx)
    px <- 1 - qx
    ans_expected <- rbind(1,
                          px[1,],
                          px[1,] * px[2,],
                          px[1,] * px[2,] * px[3,],
                          px[1,] * px[2,] * px[3,] * px[4,])
    expect_equal(ans_obtained, ans_expected)
    expect_identical(which(is.na(ans_obtained)),
                     c(10L, 12:15L))
})

test_that("'qx_to_lx' works with single row", {
    qx <- matrix(1, nr = 1, nc = 3)
    ans_obtained <- qx_to_lx(qx)
    ans_expected <- qx
    expect_equal(ans_obtained, ans_expected)
})


## 'q0_to_m0_inner' -----------------------------------------------------------

test_that("'q0_to_m0_inner' gives correct answer - infant CD, ax supplied", {
  q0 <- cbind(c(0.10, 0.01, 0.3),
              c(0.11, 0.015, 0.2))
  a0 <- c(0.35, 0.2, 0.1)
  ans_obtained <- q0_to_m0_inner(q0 = q0,
                                 sex = c("Female", "Female", "Male"),
                                 a0 = a0,
                                 infant = "CD")
  ans_expected <- q0 / ((q0 * a0) + (1 - q0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0_inner' gives correct answer - infant CD, q0 < 0.1, no ax supplied", {
  q0 <- cbind(c(0.02, 0.001, 0.025),
              c(0.01, 0.005, 0.004))
  ans_obtained <- q0_to_m0_inner(q0 = q0,
                                 sex = c("Female", "Female", "Female"),
                                 a0 = c(NA_real_, NA_real_, NA_real_),
                                 infant = "CD")
  a0 <- 0.05 + 3 * q0
  ans_expected <- q0 / ((q0 * a0) + (1 - q0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0_inner' gives correct answer with NA", {
  q0 <- cbind(c(NA_real_, 0.2, 0.25),
              c(0.11, NA_real_, 0.4))
  ans_obtained <- q0_to_m0_inner(q0 = q0,
                                 sex = c("Female", "Female", "Female"),
                                 a0 = c(NA_real_, NA_real_, NA_real_),
                                 infant = "CD")
  a0 <- 0.35
  a1 <- 1.361
  ans_expected <- q0 / ((q0 * a0) + (1 - q0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0_inner' gives correct answer with 1", {
  q0 <- cbind(c(0.107, 0.01, 0.25),
              c(0.11, 1, 0.4))
  ans_obtained <- q0_to_m0_inner(q0 = q0,
                                 sex = c("Female", "Female", "Female"),
                                 a0 = c(NA_real_, NA_real_, NA_real_),
                                 infant = "linear")
  ans_expected <- q0 / (q0 * 0.5 + (1 - q0))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0_inner' gives correct answer with 1", {
  q0 <- cbind(c(0.107, 0.01, 0.25),
              c(0.11, 0, 0.4))
  ans_obtained <- q0_to_m0_inner(q0 = q0,
                                 sex = c("Female", "Female", "Female"),
                                 a0 = c(NA_real_, NA_real_, NA_real_),
                                 infant = "linear")
  ans_expected <- q0 / (q0 * 0.5 + (1 - q0))
  expect_equal(ans_obtained, ans_expected)
})
