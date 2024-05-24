
## 'ex_to_lifetab_brass' ------------------------------------------------------

test_that("'ex_to_lifetab_brass' works with valid inputs - no sex", {
    target <- tibble::tibble(ex = c(70, 80),
                             beta = c(1, 1.1),
                             region = c("a", "b"))
    standard <- tibble::tibble(age = age_labels(type = "lt"),
                               lx = 100000 * exp(-0.5 * seq_along(age)))
    ans <- ex_to_lifetab_brass(target = target,
                               standard = standard,
                               infant = "linear",
                               child = "linear")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "region", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_equal(ans$ex[c(1, nrow(ans)/2 + 1)], c(70, 80), tolerance = 0.001)
})

test_that("'ex_to_lifetab_brass' works with valid inputs - no key", {
    target <- tibble::tibble(ex = 79,
                             beta = 1)
    standard <- tibble::tibble(age = age_labels(type = "lt"),
                               lx = 100000 * exp(-0.5 * seq_along(age)))
    ans <- ex_to_lifetab_brass(target = target,
                               standard = standard,
                               infant = "linear",
                               child = "linear")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_equal(ans$ex[1], 79, tolerance = 0.001)
})

test_that("'ex_to_lifetab_brass' works with valid inputs - ex is rvec, beta is rvec", {
    target <- tibble::tibble(ex = rvec::rvec(list(c(70, 75), c(71, 76))),
                             beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))))
    age <- age_labels(type = "lt")
    standard <- tibble::tibble(lx = rep(100000 * exp(-0.1 * seq_along(age)),
                                        times = 2),
                               sex = rep(c("Female", "Male"), each = length(age)),
                               age = rep(age, times = 2))
    ans <- ex_to_lifetab_brass(target = target,
                               standard = standard,
                               infant = "CD",
                               child = "CD")
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("sex", "beta", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_true(rvec::is_rvec(ans$qx))
})

test_that("'ex_to_lifetab_brass' works with valid inputs - beta is rvec", {
    target <- data.frame(ex = 60,
                         beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))))
    age <- age_labels(type = "lt")
    standard <- tibble::tibble(age = age,
                               lx = 100000 * exp(-0.5 * seq_along(age)))
    ans <- ex_to_lifetab_brass(target = target,
                               standard = standard)
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("beta", "age", "qx", "lx", "Lx", "dx", "ex"))
    expect_true(rvec::is_rvec(ans$qx))
})

test_that("'ex_to_lifetab_brass' gives correct error with invalid suffix", {
    target <- data.frame(ex = 60,
                         beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))))
    age <- age_labels(type = "lt")
    standard <- tibble::tibble(age = age,
                               lx = 100000 * exp(-0.5 * seq_along(age)))
    expect_error(ex_to_lifetab_brass(target = target,
                                     standard = standard,
                                     suffix = ""))
})

test_that("'ex_to_lifetab_brass' gives correct error when one set of values is incorrect", {
  target <- tibble::tibble(ex = rvec::rvec(list(c(70, 75), c(71, 76))),
                           beta = rvec::rvec(list(c(1, 1.1), c(1.2, 1.06))))
  age <- age_labels(type = "lt")
  standard <- tibble::tibble(lx = rep(100000 * exp(-0.1 * seq_along(age)),
                                      times = 2),
                             sex = rep(c("Female", "Male"), each = length(age)),
                             age = rep(age, times = 2))
  standard$lx[24:44] <- 0
  op <- options(warn = 2)
  on.exit(options(op), add = TRUE, after = FALSE)
  expect_error(ex_to_lifetab_brass(target = target,
                                   standard = standard,
                                   infant = "CD",
                                   child = "CD"),
               "Problem with calculations for `sex`=\"Male\".")
})



## 'combine_target_standard' --------------------------------------------------------

test_that("'combine_target_standard' works with valid inputs - beta not provided", {
    target <- data.frame(sex = c("F", "M"),
                         ex = 70:71)
    standard <- data.frame(sex = rep(c("F", "M"), each = 3),
                           age = rep(c("0", "1-4", "5+"), times = 2),
                           lx = c(1, 0.5, 0.2, 1, 0.4, 0.1))
    ans_obtained <- combine_target_standard(target = target,
                                            standard = standard)
    ans_expected <- vctrs::vec_split(data.frame(ex = rep(70:71, each = 3),
                                                beta = 1,
                                                age = rep(c("0", "1-4", "5+"), times = 2),
                                                lx = c(1, 0.5, 0.2, 1, 0.4, 0.1),
                                                ax = NA_real_),
                                     data.frame(sex = rep(c("F", "M"), each = 3)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'combine_target_standard' works with valid inputs - beta provided", {
    target <- data.frame(beta = c(1.1, 1.2),
                         ex = c(70, 70))
    standard <- data.frame(age = c("0", "1-4", "5+"),
                           lx = c(1, 0.5, 0.2))
    ans_obtained <- combine_target_standard(target = target,
                                            standard = standard)
    ans_expected <- vctrs::vec_split(data.frame(ex = 70,
                                                beta = rep(c(1.1, 1.2), each = 3),
                                                age = rep(c("0", "1-4", "5+"), times = 2),
                                                lx = c(1, 0.5, 0.2, 1, 0.5, 0.2),
                                                ax = NA_real_),
                                     data.frame(beta = rep(c(1.1, 1.2), each = 3)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'combine_target_standard' throws correct error when standard missing rows", {
    target <- data.frame(sex = c("F", "M", "D"),
                         ex = 70:72)
    standard <- data.frame(sex = rep(c("F", "M"), each = 3),
                           age = rep(c("0", "1-4", "5+"), times = 2),
                           lx = c(1, 0.5, 0.2, 1, 0.4, 0.1))
    expect_error(combine_target_standard(target = target,
                                         standard = standard),
                 "`standard` does not have values for case where `sex`=\"D\"")
})


## 'ex_to_lifetab_brass_one' --------------------------------------------------

test_that("'ex_to_lifetab_brass_one' works with valid inputs - not rvec", {
    val <- tibble::tibble(ex = 70,
                          beta = 1.1,
                          age = age_labels(type = "lt"),
                          lx = 100000 * exp(-0.2 * seq_along(age)),
                          ax = NA_real_)
    sex <- "F"
    methods <- c(infant = "CD", child = "CD", closed = "linear", open = "constant")
    radix <- 1000
    suffix <- "brass"
    ans <- ex_to_lifetab_brass_one(val = val,
                                   sex = sex,
                                   methods = methods,
                                   radix = radix,
                                   suffix = suffix)
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("age", paste(c("qx", "lx", "Lx", "dx", "ex"), "brass", sep = ".")))
    expect_equal(ans$ex.brass[1], 70, tolerance = 0.001)
    expect_equal(nrow(ans), nrow(val))
})

test_that("'ex_to_lifetab_brass_one' works with valid inputs - is rvec", {
    val <- tibble::tibble(ex = rvec::rvec(matrix(80:82, nr = 1)),
                          beta = 1,
                          age = age_labels(type = "lt"),
                          lx = 100000 * exp(-0.3 * seq_along(age)),
                          ax = NA_real_)
    sex <- "Fem"
    methods <- c(infant = "CD", child = "CD", closed = "linear", open = "constant")
    radix <- 1000
    suffix <- "brass"
    ans <- ex_to_lifetab_brass_one(val = val,
                                   sex = sex,
                                   methods = methods,
                                   radix = radix,
                                   suffix = suffix)
    expect_true(is.data.frame(ans))
    expect_setequal(names(ans), c("age", paste(c("qx", "lx", "Lx", "dx", "ex"), "brass", sep = ".")))
    expect_true(rvec::is_rvec(ans$ex.brass))
    expect_equal(nrow(ans), nrow(val))
})


## 'make_ex_beta_n_draw' ------------------------------------------------------

test_that("'make_ex_beta_n_draw' works with ex non-rvec, beta non-rvec", {
    ans_obtained <- make_ex_beta_n_draw(ex = 80:81, beta = c(1, 0.1))
    ans_expected <- list(ex = c(80, 81),
                         beta = c(1, 0.1),
                         n_draw = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta non-rvec", {
    ans_obtained <- make_ex_beta_n_draw(ex = rvec::rvec(list(80:81, 90:91)),
                                        beta = c(1, 1))
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 1, 1, 1),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex non-rvec, beta rvec", {
    ans_obtained <- make_ex_beta_n_draw(ex = 80:81,
                                        beta = rvec::rvec(list(c(1, 2), c(0.1, 0.2))))
    ans_expected <- list(ex = c(80, 81, 80, 81),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta rvec", {
    ans_obtained <- make_ex_beta_n_draw(ex = rvec::rvec(list(80:81, 90:91)),
                                        beta = rvec::rvec(list(c(1, 2), c(0.1, 0.2))))
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec 1 draw, beta rvec", {
    ans_obtained <- make_ex_beta_n_draw(ex = rvec::rvec(list(80, 90)),
                                        beta = rvec::rvec(list(c(1, 2), c(0.1, 0.2))))
    ans_expected <- list(ex = c(80, 90, 80, 90),
                         beta = c(1, 0.1, 2, 0.2),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works with ex rvec, beta rvec 1 draw", {
    ans_obtained <- make_ex_beta_n_draw(ex = rvec::rvec(list(80:81, 90:91)),
                                        beta = rvec::rvec(list(1, 0.1)))
    ans_expected <- list(ex = c(80, 90, 81, 91),
                         beta = c(1, 0.1, 1, 0.1),
                         n_draw = 2L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_ex_beta_n_draw' works throws correct error when ex, beta have different (non-1) draws", {
    expect_error(make_ex_beta_n_draw(ex = rvec::rvec(list(80:82, 90:92)),
                                     beta = rvec::rvec(list(c(1, 0.1), c(2, 0.2)))),
                 "`ex` and `beta` have different numbers of draws.")
})


## 'make_sex_ex_to_lifetab' ---------------------------------------------------

test_that("'make_sex_ex_to_lifetab' works with sex supplied, sex needed", {
    methods <- c(infant = "CD",
                 child = "CD",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(sex = c("M", "F"),
                                           methods = methods,
                                           nm_data = "standard")
    ans_expected <- c("Male", "Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_sex_ex_to_lifetab' works with sex supplied, sex not needed", {
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(sex = c("M", "F"),
                                           methods = methods,
                                           nm_data = "standard")
    ans_expected <- NA_character_
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_sex_ex_to_lifetab' throws correct error when sex not supplied, sex needed", {
    methods <- c(infant = "CD",
                 child = "CD",
                 closed = "constant",
                 open = "constant")
    expect_error(make_sex_ex_to_lifetab(sex = NULL,
                                        methods = methods,
                                        nm_data = "standard"),
                 "`standard` does not have a variable called \"sex\"")
})

test_that("'make_sex_ex_to_lifetab' works with sex not supplied, sex not needed", {
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    ans_obtained <- make_sex_ex_to_lifetab(sex = NULL,
                                           methods = methods,
                                           nm_data = "standard")
    ans_expected <- NA_character_
    expect_identical(ans_obtained, ans_expected)
})

