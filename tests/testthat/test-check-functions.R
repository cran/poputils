
## 'check_at_in_age' ----------------------------------------------------------

test_that("'check_at_in_age' returns TRUE with valid inputs", {
  age <- c("10+", "1-4", "0", "5-9")
  expect_true(check_at_in_age(at = 0, age = age))
  expect_true(check_at_in_age(at = 1, age = age))
  expect_true(check_at_in_age(at = 5, age = age))
  expect_true(check_at_in_age(at = 10, age = age))
})

test_that("'check_at_in_age' throws correct error with invalid inputs", {
  age <- c("10+", "1-4", "0", "5-9")
  expect_error(check_at_in_age(at = 2, age = age),
               "Invalid value for `at`.")
})



## 'check_at_most_one_colnum' -------------------------------------------------

test_that("'check_at_most_one_colnum' returns TRUE with valid inputs", {
    z <- integer()
    names(z) <- character()
    expect_true(check_at_most_one_colnum(list(x = c(a = 1L), y = c(b = 2L), z = z)))
    expect_true(check_at_most_one_colnum(list()))
})

test_that("'check_at_most_one_colnum' raises correct error with length-2 vector", {
    expect_error(check_at_most_one_colnum(list(x = c(a = 1L), y = c(b = 2:3))),
                 "2 variables specified for `y`.")
})


## 'check_ax' -----------------------------------------------------------------

test_that("'check_ax' returns TRUE with valid inputs", {
    expect_true(check_ax(ax = c(NA_real_, 2, 2.5, NA_real_),
                         age = c("0", "1-4", "5-9", "10+")))
    expect_true(check_ax(ax = numeric(),
                         age = character()))
})

test_that("'check_ax' returns correct error with non-numeric", {
    expect_error(check_ax(ax = c("1", 2, 2.5, NA_real_),
                          age = c("0", "1-4", "5-9", "10+")),
                 "`ax` is non-numeric.")
})

test_that("'check_ax' returns correct error with negative", {
    expect_error(check_ax(ax = c(-1, 2, 2.5, NA_real_),
                          age = c("0", "1-4", "5-9", "10+")),
                 "`ax` has negative value.")
})

test_that("'check_ax' returns correct error with too large", {
    expect_error(check_ax(ax = c(0.1, 2, 10, NA_real_),
                          age = c("0", "1-4", "5-9", "10+")),
                 "`ax` larger than width of corresponding age group.")
})


## 'check_duplicated_age' -----------------------------------------------------

test_that("'check_duplicated_age' returns TRUE with valid inputs", {
    expect_true(check_duplicated_age(c("0", "1", "2+")))
    expect_true(check_duplicated_age(c("0", "1", "0", "1", "2+")))
    expect_true(check_duplicated_age("0"))
    expect_true(check_duplicated_age(character()))
})

test_that("'check_duplicated_age' returns correct error with invalid inputs", {
    expect_error(check_duplicated_age(c("0", "1", "2+", "0", "1", "2+")),
                 "Age labels duplicated.")
    expect_error(check_duplicated_age(c("0", "0")),
                 "Age labels duplicated.")
})


## 'check_duplicated_rows' ----------------------------------------------------

test_that("'check_duplicated_rows' returns TRUE with valid input", {
    x <- data.frame(age = 80:81, sex = c("F", "F"))
    expect_true(check_duplicated_rows(x = x, nm_x = "x", nms_cols = c("age", "sex")))
    expect_true(check_duplicated_rows(data.frame()))
})


test_that("'check_duplicated_rows' throws expected error when duplicate 'by' variable", {
    x <- data.frame(ex = 80:81, sex = c("F", "F"), beta = c(0.9, 1.1))
    expect_error(check_duplicated_rows(x, nm_x = "target", nms_cols = "sex"),
                 "`target` has two rows with same value for `sex`.")
})

test_that("'check_duplicated_rows' throws expected error when duplicate 'by' variables", {
    x <- data.frame(ex = 80:81, sex = c("F", "F"), reg = c(1, 1), beta = c(0.9, 1.1))
    expect_error(check_duplicated_rows(x, nm_x = "target", nms_cols = c("sex", "reg")),
                 "`target` has two rows with same values for `sex` and `reg`.")
})


## 'check_equal_length' -------------------------------------------------------

test_that("'check_equal_length' returns TRUE with valid inputs", {
    expect_true(check_equal_length(x = integer(),
                                   y = character(),
                                   nm_x = "x",
                                   nm_y = "y"))
    expect_true(check_equal_length(x = 1:2,
                                   y = c("a", "b"),
                                   nm_x = "x",
                                   nm_y = "y"))
})

test_that("'check_equal_length' returns correct error with invalid inputs", {
    expect_error(check_equal_length(x = 1:3,
                                    y = c("a", "b"),
                                    nm_x = "x",
                                    nm_y = "y"),
                 "`x` and `y` have different lengths.")
})


## 'check_flag' ---------------------------------------------------------------

test_that("'check_flag' returns TRUE with valid inputs", {
    x <- TRUE
    expect_true(check_flag(x))
    x <- FALSE
    expect_true(check_flag(x))
})

test_that("'check_flag' throws expected error non-length-1", {
    y <- logical()
    expect_error(check_flag(y),
                 "`y` does not have length 1")
    z <- c(TRUE, TRUE)
    expect_error(check_flag(z),
                 "`z` does not have length 1")
})

test_that("'check_flag' throws expected error non-logical", {
    x <- "hello"
    expect_error(check_flag(x),
                 "`x` does not have class <logical>")
})

test_that("'check_flag' throws expected error NA", {
    x <- NA
    expect_error(check_flag(x),
                 "`x` is NA")
})


## 'check_life_colnums' -------------------------------------------------------

test_that("'check_life_colnums' returns TRUE with valid inputs - no groups", {
    empty_colnum <- integer()
    names(empty_colnum) <- character()
    expect_true(check_life_colnums(mx_colnum = c(mx = 1L),
                                   qx_colnum = empty_colnum,
                                   age_colnum = c(AGE = 3L),
                                   sex_colnum = empty_colnum,
                                   ax_colnum = c(AX = 2L),
                                   by_colnums = c(region = 5L, time = 4L),
                                   groups_colnums = empty_colnum))
})

test_that("'check_life_colnums' returns TRUE with valid inputs - with qx, groups", {
    empty_colnum <- integer()
    names(empty_colnum) <- character()
    expect_true(check_life_colnums(mx_colnum = empty_colnum,
                                   qx_colnum = c(qx = 1L),
                                   age_colnum = c(AGE = 3L),
                                   sex_colnum = empty_colnum,
                                   ax_colnum = c(AX = 2L),
                                   by_colnums = empty_colnum,
                                   groups_colnums = c(region = 5L, time = 4L)))
})

test_that("'check_life_colnums' returns correct error when no mx or qx", {
    empty_colnum <- integer()
    expect_error(check_life_colnums(mx_colnum = empty_colnum,
                                    qx_colnum = empty_colnum,
                                    age_colnum = c(AGE = 3L),
                                    sex_colnum = empty_colnum,
                                    ax_colnum = c(AX = 2L),
                                    by_colnums = c(region = 5L, time = 4L),
                                    groups_colnums = empty_colnum),
                 "No value supplied for `mx` or for `qx`.")
})

test_that("'check_life_colnums' returns correct error when mx and qx", {
    empty_colnum <- integer()
    expect_error(check_life_colnums(mx_colnum = c(mx = 1L),
                                    qx_colnum = c(qx = 7L),
                                    age_colnum = c(AGE = 3L),
                                    sex_colnum = empty_colnum,
                                    ax_colnum = c(AX = 2L),
                                    by_colnums = c(region = 5L, time = 4L),
                                    groups_colnums = empty_colnum),
                 "Values supplied for `mx` and for `qx`.")
})

test_that("'check_life_colnums' returns correct error when no age", {
    empty_colnum <- integer()
    expect_error(check_life_colnums(mx_colnum = c(mx = 3L),
                                    qx_colnum = empty_colnum,
                                    age_colnum = empty_colnum,
                                    sex_colnum = empty_colnum,
                                    ax_colnum = c(AX = 2L),
                                    by_colnums = c(region = 5L, time = 4L),
                                    groups_colnums = empty_colnum),
                 "No value supplied for `age`.")
})

test_that("'check_life_colnums' returns correct error with by, groups clash", {
    empty_colnum <- integer()
    names(empty_colnum) <- character()
    expect_error(check_life_colnums(mx_colnum = c(mx = 1L),
                                    qx_colnum = empty_colnum,
                                    age_colnum = c(AGE = 3L),
                                    sex_colnum = empty_colnum,
                                    ax_colnum = c(AX = 2L),
                                    by_colnums = c(strata = 7L),
                                    groups_colnums = c(region = 5L, time = 4L)),
                 "Can't supply `by` when `data` is a grouped data frame.")
})


## 'check_lx' --------------------------------------------------------

test_that("'check_lx' returns TRUE with valid inputs", {
    expect_true(check_lx(c(1, 0.5, 0), age = c("0", "1-4", "5+")))
    expect_true(check_lx(c(10000, 4000, 20, 1), age = c("0", "1-4", "50+")))
})

test_that("'check_lx' throws correct error when length 1", {
    expect_error(check_lx(1),
                 "`lx` has length 1.")
})

test_that("'check_lx' throws correct error when first element 0", {
    expect_error(check_lx(c(0, 0)),
                 "First element of `lx` is 0.")
})

test_that("'check_lx' throws correct error when increasing", {
    expect_error(check_lx(c(100, 90, 80, 81, 2),
                          age = c("0-4", "5-9", "10-14", "15-19", "20+")),
                 "`lx` for age \"15-19\" greater than `lx` for age \"10-14\"")
})


## 'check_mx' -----------------------------------------------------------------

test_that("'check_mx_rvec' returns TRUE with valid rvec inputs", {
    x <- rvec::rvec_dbl()
    expect_true(check_mx(x))
    x <- rvec::rvec_dbl(matrix(1:6))
    expect_true(check_mx(x))
    x <- rvec::rvec_int(matrix(1:6))
    expect_true(check_mx(x))
})

test_that("'check_mx_vec' returns TRUE with valid vector inputs", {
    x <- 1:3
    expect_true(check_mx(x))
    x <- c(0.2, 0.1, NA)
    expect_true(check_mx(x))
    x <- double()
    expect_true(check_mx(x))
})

test_that("'check_mx' throws correct error with non-numeric", {
    x <- rvec::rvec_lgl()
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
    x <- NULL
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
    x <- c(TRUE, FALSE)
    expect_error(check_mx(x),
                 "`mx` is non-numeric")
})

test_that("'check_mx' throws correct error with negative value", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, -0.1), nrow = 1))
    expect_error(check_mx(x),
                 "`mx` has negative value.")
    expect_error(check_mx(c(1, -1, 0, -1, 1)),
                 "`mx` has negative values.")
})


## 'check_no_overlap_colnums' -------------------------------------------------

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 3 elements", {
    z <- integer()
    names(z) <- character()
    x <- list(x = c(a = 1L, b = 2L), y = c(c = 3L), z = z)
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 1 element", {
    x <- list(x = c(a = 1L, b = 2L))
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' returns TRUE with valid inputs - 0 elements", {
    x <- list()
    expect_true(check_no_overlap_colnums(x))
})

test_that("'check_no_overlap_colnums' throws correct error with overlap", {
    x <- list(x = c(a = 1L, b = 2L), y = c(b = 2L, a = 1L))
    expect_error(check_no_overlap_colnums(x),
                 "`x` and `y` use the same variables.")
})


## 'check_not_rvec' -----------------------------------------------------------

test_that("'check_not_rvec' returns TRUE with valid inputs", {
  expect_true(check_not_rvec(NULL))
  expect_true(check_not_rvec(1:3))
  expect_true(check_not_rvec("hello"))
})

test_that("'check_not_rvec' raises error with rvec", {
  expect_error(check_not_rvec(rvec::rvec(matrix(1:5, nr = 1)), nm_x = "xx"),
               "`xx` is an rvec.")
  expect_error(check_not_rvec(rvec::rvec(matrix(letters, nr = 1)), nm_x = "y"),
               "`y` is an rvec.")
})


## 'check_n' ------------------------------------------------------------------

test_that("'check_n' returns TRUE with valid inputs", {
    expect_true(check_n(n = 4, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L))
    expect_true(check_n(n = 4, nm_n = "n", min = NULL, max = NULL, divisible_by = NULL))
})

test_that("'check_n' throws correct error with rvec", {
  n <- rvec::rvec(list(1:3))
  expect_error(check_n(n = n, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
               "`n` is an rvec.")
})

test_that("'check_n' throws correct error with non-numeric", {
    expect_error(check_n(n = "4", nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is non-numeric.")
})

test_that("'check_n' throws correct error with wrong length", {
    expect_error(check_n(n = integer(), nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` does not have length 1.")
    expect_error(check_n(n = 10:11, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` does not have length 1.")
})

test_that("'check_n' throws correct error with NA", {
    expect_error(check_n(n = NA_real_, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is NA.")
})

test_that("'check_n' throws correct error with Inf", {
    expect_error(check_n(n = Inf, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is Inf.")
})

test_that("'check_n' throws correct error with non-integer", {
    expect_error(check_n(n = 6.4, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is not an integer.")
})

test_that("'check_n' throws correct error when less than min", {
    expect_error(check_n(n = 3, nm_n = "n", min = 4L, max = NULL, divisible_by = 1L),
                 "`n` is less than 4.")
})

test_that("'check_n' throws correct error when greater than max", {
    expect_error(check_n(n = 60, nm_n = "n", min = 4, max = 10, divisible_by = 1L),
                 "`n` is greater than 10.")
})

test_that("'check_n' throws correct error when not divisible by 'divisible_by'", {
    expect_error(check_n(n = 61, nm_n = "n", min = 4, max = Inf, divisible_by = 5L),
                 "`n` is not divisible by 5.")
})


## 'check_no_overlap_colnums_pair' --------------------------------------------

test_that("'check_no_overlap_colnums_pair' returns TRUE with valid inputs - both nonempty", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(c = 3L))
    expect_true(check_no_overlap_colnums_pair(pair = pair))
})

test_that("'check_no_overlap_colnums_pair' returns TRUE with valid inputs - one empty", {
    pair <- list(x = c(a = 1L, b = 2L), y = integer())
    expect_true(check_no_overlap_colnums_pair(pair = pair))
})

test_that("'check_no_overlap_colnums_pair' throws correct error with one overlap", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(c = 3L, a = 1L))
    expect_error(check_no_overlap_colnums_pair(pair = pair),
                 "`x` and `y` use the same variable.")
})

test_that("'check_no_overlap_colnums_pair' throws correct error with two overlap", {
    pair <- list(x = c(a = 1L, b = 2L), y = c(b = 2L, a = 1L))
    expect_error(check_no_overlap_colnums_pair(pair = pair),
                 "`x` and `y` use the same variables.")
})


## 'check_number' --------------------------------------------------------------

test_that("'check_number' returns TRUE with valid inputs", {
    expect_true(check_number(1L,
                             x_arg = "x",
                             check_na = TRUE,
                             check_positive = TRUE,
                             check_nonneg = TRUE,
                             check_whole = TRUE))
    expect_true(check_number(0.001,
                             x_arg = "x",
                             check_na = TRUE,
                             check_positive = TRUE,
                             check_nonneg = TRUE,
                             check_whole = FALSE))
    expect_true(check_number(NA_integer_,
                             x_arg = "x",
                             check_na = FALSE,
                             check_positive = TRUE,
                             check_nonneg = TRUE,
                             check_whole = FALSE))
    expect_true(check_number(0,
                             x_arg = "x",
                             check_na = TRUE,
                             check_positive = FALSE,
                             check_nonneg = TRUE,
                             check_whole = TRUE))
    expect_true(check_number(-1,
                             x_arg = "x",
                             check_na = TRUE,
                             check_positive = FALSE,
                             check_nonneg = FALSE,
                             check_whole = TRUE))
    expect_true(check_number(rvec::rvec(list(1:3)),
                             x_arg = "x",
                             check_na = TRUE,
                             check_positive = FALSE,
                             check_nonneg = FALSE,
                             check_whole = TRUE))
})

test_that("'check_number' returns correct error with non-numeric", {
    expect_error(check_number("1",
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is non-numeric.")
})

test_that("'check_number' returns correct error with wrong length", {
    expect_error(check_number(1:2,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` does not have length 1.")
})

test_that("'check_number' returns correct error with NA", {
    expect_error(check_number(NA_real_,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is NA.")
})

test_that("'check_number' returns correct error with Inf", {
    expect_error(check_number(Inf,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is infinite.")
})

test_that("'check_number' returns correct error with check_positive", {
    expect_error(check_number(0,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is non-positive.")
    expect_error(check_number(-1, x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is non-positive.")
})

test_that("'check_number' returns correct error with check_nonneg", {
    expect_error(check_number(-1,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is negative.")
})

test_that("'check_number' returns correct error with check_whole", {
    expect_error(check_number(0.5,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` is not a whole number.")
})


## 'check_numeric' --------------------------------------------------------------

test_that("'check_numeric' returns TRUE with valid inputs", {
    expect_true(check_numeric(1L,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE))
    expect_true(check_numeric(c(0.001, NA),
                              x_arg = "x",
                              check_na = FALSE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = FALSE))
    expect_true(check_numeric(NA_integer_,
                              x_arg = "x",
                              check_na = FALSE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = FALSE))
    expect_true(check_numeric(c(0, 1),
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE))
    expect_true(check_numeric(-1,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = FALSE,
                              check_whole = TRUE))
    expect_true(check_numeric(rvec::rvec(list(1:3, 2:4)),
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = FALSE,
                              check_whole = TRUE))
})

test_that("'check_numeric' returns correct error with non-numeric", {
    expect_error(check_numeric(character(),
                               x_arg = "x",
                               check_na = TRUE,
                               check_positive = TRUE,
                               check_nonneg = TRUE,
                               check_whole = TRUE),
                 "`x` is non-numeric.")
})

test_that("'check_numeric' returns correct error with NA", {
    expect_error(check_numeric(NA_real_,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has NA.")
    expect_error(check_numeric(c(NA, 2, NA),
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has NAs.")
})

test_that("'check_numeric' returns correct error with Inf", {
    expect_error(check_numeric(Inf,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has non-finite value.")
    expect_error(check_numeric(c(Inf, Inf, 3),
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has non-finite values.")
})

test_that("'check_numeric' returns correct error with check_positive", {
    expect_error(check_numeric(0,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has non-positive value.")
    expect_error(check_numeric(c(-1, 0, -1),
                               x_arg = "x",
                              check_na = TRUE,
                              check_positive = TRUE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has non-positive values.")
})

test_that("'check_numeric' returns correct error with check_nonneg", {
    expect_error(check_numeric(-1,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has negative value.")
    expect_error(check_numeric(c(-1, -3, NA),
                              x_arg = "x",
                              check_na = FALSE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has negative values.")
})

test_that("'check_numeric' returns correct error with check_whole", {
    expect_error(check_numeric(0.5,
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has value that is not whole number.")
    expect_error(check_numeric(rvec::rvec(list(c(0.5, 0.3))),
                              x_arg = "x",
                              check_na = TRUE,
                              check_positive = FALSE,
                              check_nonneg = TRUE,
                              check_whole = TRUE),
                 "`x` has values that are not whole numbers.")
})


## 'check_qx' -----------------------------------------------------------------

test_that("'check_qx_rvec' returns TRUE with valid rvec inputs", {
    x <- rvec::rvec_dbl()
    expect_true(check_qx(x, nm_qx = "qx"))
    x <- rvec::rvec_int(matrix(1:0))
    expect_true(check_qx(x, nm_qx = "qx"))
    x <- rvec::rvec_dbl(matrix(c(0.5, 0.33)))
    expect_true(check_qx(x, nm_qx = "qx"))
})

test_that("'check_qx_vec' returns TRUE with valid vector inputs", {
    x <- 0:1
    expect_true(check_qx(x, nm_qx = "qx"))
    x <- c(0.2, 0.1, NA)
    expect_true(check_qx(x, nm_qx = "qx"))
    x <- double()
    expect_true(check_qx(x, nm_qx = "qx"))
})

test_that("'check_qx' throws correct error with non-numeric", {
    x <- rvec::rvec_lgl()
    expect_error(check_qx(x, nm_qx = "qx"),
                 "`qx` is non-numeric")
    x <- NULL
    expect_error(check_qx(x, nm_qx = "q0"),
                 "`q0` is non-numeric")
    x <- c(TRUE, FALSE)
    expect_error(check_qx(x, nm_qx = "qx"),
                 "`qx` is non-numeric")
})

test_that("'check_qx' throws correct error with negative value", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, -0.1), nrow = 1))
    expect_error(check_qx(x, nm_qx = "q0"),
                 "`q0` has negative value.")
    expect_error(check_qx(c(1, -1, 0, -1, 1), nm_qx = "q0"),
                 "`q0` has negative values.")
})

test_that("'check_qx' throws correct error with values greater than 1", {
    x <- rvec::rvec_dbl(matrix(c(1, 0, NA, 1.1), nrow = 1))
    expect_error(check_qx(x, nm_qx = "qx"),
                 "`qx` has value greater than 1.")
    expect_error(check_qx(c(1, 2, 0, 1.000001, 1), nm_qx = "qx"),
                 "`qx` has values greater than 1.")
})


## 'check_sex_not_needed' -----------------------------------------------------

test_that("'check_sex_not_needed' returns TRUE when methods don't require sex variable", {
    methods <- c(infant = "constant",
                 child = "linear",
                 closed = "linear",
                 open = "constant")
    expect_true(check_sex_not_needed(methods))
})

test_that("'check_sex_not_needed' returns correct error when methods do require sex variable", {
    methods <- c(infant = "constant",
                 child = "CD",
                 closed = "linear",
                 open = "constant")
    expect_error(check_sex_not_needed(methods),
                 "`child` is \"CD\" but no value supplied for `sex`")
})

    

## 'check_standard' -----------------------------------------------------------

test_that("'check_standard' returns TRUE with valid inputs - no ax", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           lx = c(1, 0.3, 0.2, 0.1, 1, 0.6))
    expect_true(check_standard(standard))
})

test_that("'check_standard' returns TRUE with valid inputs - has ax", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           ax = c(0.5, 2, 2.5, 2.5, 0.5, 2),
                           lx = c(1, 0.3, 0.2, 0.1, 1, 0.6))
    expect_true(check_standard(standard))
})

test_that("'check_standard' throws expected error when not data frame", {
    expect_error(check_standard(NULL),
                 "`standard` is not a data frame.")
})

test_that("'check_standard' throws expected error when `standard` includes an 'ex' variable", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           ex = c(1, 0.3, 0.2, 0.1, 1, 0.6))
    expect_error(check_standard(standard),
                 "`standard` has a variable called `ex`.")
})

test_that("'check_standard' throws expected error when does not have lx variable", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           wrong = c(1, 0.3, 0.2, 0.1, 1, 0.6))
    expect_error(check_standard(standard),
                 "`standard` does not have a variable called `lx`.")
})

test_that("'check_standard' throws expected error when lx is an rvec", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"))
    standard$lx <- rvec::rvec(matrix(1:12, nr = 6))
    expect_error(check_standard(standard),
                 "`lx` variable in `standard` is an rvec.")
})

test_that("'check_standard' throws expected error when age invalid - no 'by' variable", {
    standard <- data.frame(age = c("5+", "0", "wrong"),
                           lx = c(0.1, 1, 0.6))
    expect_error(check_standard(standard),
                 "Problem with `age` values.")
})

test_that("'check_standard' throws expected error when age invalid - one 'by' variable", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "wrong"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           lx = c(1, 0.3, 0.2, 0.1, 1, 0.6))
    expect_error(check_standard(standard),
                 "Problem with `age` values for `sex`=\"M\".")
})

test_that("'check_standard' throws expected error when lx invalid - no 'by' variable", {
    standard <- data.frame(age = c("5+", "0", "1-4"),
                           lx = c(0.1, 1, 3))
    expect_error(check_standard(standard),
                 "Problem with `lx` values.")
})

test_that("'check_standard' throws expected error when age invalid - one 'by' variable", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           lx = c(1, 0.3, 0.2, 0.1, 1, 2))
    expect_error(check_standard(standard),
                 "Problem with `lx` values for `sex`=\"M\".")
})

test_that("'check_standard' throws expected error when ax invalid - no 'by' variable", {
    standard <- data.frame(age = c("5+", "0", "1-4"),
                           lx = c(0.1, 1, 0.5),
                           ax = c(NA, 0.5, 10))
    expect_error(check_standard(standard),
                 "Problem with `ax` values.")
})

test_that("'check_standard' throws expected error when age invalid - one 'by' variable", {
    standard <- data.frame(age = c("0", "1-4", "5+", "5+", "0", "1-4"),
                           sex = c("F", "F", "F", "M", "M", "M"),
                           lx = c(1, 0.3, 0.2, 0.1, 1, 0.5),
                           ax = c(0.5, 0.3, 0.2, NA, 1, 10))
    expect_error(check_standard(standard),
                 "Problem with `ax` values for `sex`=\"M\".")
})


## 'check_string' -------------------------------------------------------------

test_that("'check_string' returns TRUE with valid input", {
    expect_true(check_string(x = "z", x_arg = "x"))
    expect_true(check_string(x = "helloworld", x_arg = "x"))
})

test_that("'check_string' returns error with non-character", {
    expect_error(check_string(x = 1, x_arg = "y"),
                 "`y` is non-character.")
})

test_that("'check_string' returns error with length 2", {
    expect_error(check_string(x = c("a", "b"), x_arg = "y"),
                 "`y` does not have length 1.")
})

test_that("'check_string' returns error with NA", {
    expect_error(check_string(x = NA_character_, x_arg = "y"),
                 "`y` is NA.")
})

test_that("'check_string' returns error with nchar = 0", {
    expect_error(check_string(x = "", x_arg = "y"),
                 "`y` is blank.")
})

test_that("'check_string' returns error with blanks", {
    expect_error(check_string(x = "hello world", x_arg = "y"),
                 "`y` contains blanks.")
})


## 'check_target_ex_to_lifetab_brass' -----------------------------------------

test_that("'check_target_ex_to_lifetab_brass' returns TRUE with valid inputs", {
    target <- data.frame(ex = 80:81, sex = c("F", "M"), beta = c(0.9, 1.1))
    expect_true(check_target_ex_to_lifetab_brass(target))
    target <- data.frame(ex = 80)
    expect_true(check_target_ex_to_lifetab_brass(target))
    target <- data.frame(ex = 81:84,
                         sex = c("F", "M", "F", "M"),
                         beta = c(0.9, 1.1, 0.9, 1.1),
                         reg = c(1, 1, 2, 2))
    expect_true(check_target_ex_to_lifetab_brass(target))
})

test_that("'check_target_ex_to_lifetab_brass' throws expected error when not data frame", {
    expect_error(check_target_ex_to_lifetab_brass(NULL),
                 "`target` is not a data frame.")
})

test_that("'check_target_ex_to_lifetab_brass' throws expected error when `target` includes a 'lx' variable", {
    target <- data.frame(ex = 80:81, lx = c(1, 2), sex = c("F", "M"), beta = c(0.9, 1.1))
    expect_error(check_target_ex_to_lifetab_brass(target),
                 "`target` has a variable called `lx`.")
})

test_that("'check_target_ex_to_lifetab_brass' throws expected error when does not have ex variable", {
    target <- data.frame(wrong = 80:81, sex = c("F", "M"), beta = c(0.9, 1.1))
    expect_error(check_target_ex_to_lifetab_brass(target),
                 "`target` does not have a variable called `ex`.")
})

test_that("'check_target_ex_to_lifetab_brass' throws expected error when no index variables", {
    target <- data.frame(ex = 80:81)
    expect_error(check_target_ex_to_lifetab_brass(target),
                 "`target` does not have index variables.")
})



## 'check_valid_colnum_list' --------------------------------------------------

test_that("'check_valid_colnum_list' returns TRUE with valid inputs - 3 elements", {
    z <- integer()
    names(z) <- character()
    x <- list(x = c(a = 1L, b = 2L), y = c(c = 3L), z = z)
    expect_true(check_valid_colnum_list(x))
})

test_that("'check_valid_colnum_list' returns TRUE with valid inputs - 0 elements", {
    x <- list()
    expect_true(check_valid_colnum_list(x))
})

test_that("'check_valid_colnum_list' throws expected error when non-list", {
    expect_error(check_valid_colnum_list(NULL),
                 "Internal error: `x` is not a list.")
})

test_that("'check_valid_colnum_list' throws expected error when x does not have names", {
    expect_error(check_valid_colnum_list(list(c(a = 1L))),
                 "Internal error: `x` does not have names.")
})

test_that("'check_valid_colnum_list' throws expected error when x has duplicated names", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), x = c(b = 2L))),
                 "Internal error: names for `x` have duplicates.")
})

test_that("'check_valid_colnum_list' throws expected error when x not all integer", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), y = c(b = 2))),
                 "Internal error: elements of `x` are not all integer vectors.")
})

test_that("'check_valid_colnum_list' throws expected error when x not named", {
    expect_error(check_valid_colnum_list(list(x = c(a = 1L), y = 2L)),
                 "Internal error: elements of `x` are not all named.")
})



    


    




