
## 'remove_existing_tfr_col' --------------------------------------------------

test_that("'remove_existing_tfr_col' returns data untouched if no tfr col present, or if suffix used", {
  data <- expand.grid(sex = c("f", "m"), age = age_labels("five", min = 15, max = 50))
  ans_obtained <- remove_existing_tfr_col(data, suffix = NULL)
  ans_expected <- data
  expect_identical(ans_obtained, ans_expected)
  data$tfr <- runif(n = nrow(data))
  ans_obtained <- remove_existing_tfr_col(data, suffix = "x")
  ans_expected <- data
  expect_identical(ans_obtained, ans_expected)
})

test_that("'remove_existing_tfr_col' removes columns if tfr present", {
  data <- expand.grid(sex = c("f", "m"), age = age_labels("five", min = 15, max = 50))
  data$tfr <- runif(n = nrow(data))
  suppressMessages(ans_obtained <- remove_existing_tfr_col(data, suffix = NULL))
  ans_expected <- data[1:2]
  expect_identical(ans_obtained, ans_expected)
  data <- expand.grid(sex = c("f", "m"), age = age_labels("five", min = 15, max = 50))
  data$tfr.x <- runif(n = nrow(data))
  suppressMessages(ans_obtained <- remove_existing_tfr_col(data, suffix = "x"))
  ans_expected <- data[1:2]
  expect_identical(ans_obtained, ans_expected)
})


## 'tfr_inner' ----------------------------------------------------------------

test_that("'tfr' works with by = 1, no sex", {
  data <- data.frame(age = age_labels("five", min = 15, max = 50),
                     asfr = 1:7)
  ans_obtained <- tfr(data = data,
                      asfr = asfr,
                      denominator = 2,
                      suffix = "xx")
  ans_expected <- tibble::tibble(tfr.xx = 5 * sum(1:7) / 2)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'tfr' works with sex, by = 2", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 4),
                     sex = rep(rep(c("f", "m"), each = 7), 2),
                     reg = rep(c("a", "b"), each = 14),
                     asfr = runif(n = 28))
  ans_obtained <- tfr(data,
                      asfr = asfr,
                      sex = sex,
                      by = reg)
  ans_expected <- tibble::tibble(reg = c("a", "b"),
                                 tfr = 5 * c(sum(data$asfr[1:14]), sum(data$asfr[15:28])))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'tfr' with 'by' and tfr with 'group_by' give same answer", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 4),
                     sex = rep(rep(c("f", "m"), each = 7), 2),
                     reg = rep(c("a", "b"), each = 14),
                     asfr = runif(n = 28))
  ans_by <- tfr(data,
                      asfr = asfr,
                      sex = sex,
                by = reg)
  ans_group_by <- data |>
    dplyr::group_by(reg) |>
    tfr(asfr = asfr, sex = sex)
  expect_identical(ans_by, ans_group_by)
})

test_that("'tfr' throws appopriate error message by = 1", {
  data <- data.frame(age = age_labels("five", min = 15, max = 50),
                     asfr = c(1:6, -1))
  expect_error(tfr(data = data,
                   asfr = asfr,
                   denominator = 2,
                   suffix = "xx"),
               "`asfr` has negative value.")
})

test_that("'tfr' throws appropriate error when by = 2", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 4),
                     sex = rep(rep(c("f", "m"), each = 7), 2),
                     reg = rep(c("a", "b"), each = 14),
                     asfr = runif(n = 28))
  data$asfr[10] <- -1
  expect_error(tfr(data,
                   asfr = asfr,
                   sex = sex,
                   by = reg),
               "Problem calculating total fertility rate.")
})

test_that("'tfr' gives warning when TFR too high", {
  data <- data.frame(age = age_labels("five", min = 15, max = 50),
                     asfr = 1000:1006)
  expect_warning(tfr(data = data,
                     asfr = asfr,
                     denominator = 2,
                     suffix = "xx"),
                 "Value for TFR over 100.")
  expect_warning(tfr(data = data,
                     asfr = asfr,
                     denominator = 2),
                 "Value for TFR over 100.")
})


## 'tfr_inner' ----------------------------------------------------------------

test_that("'tfr_inner' works with no sex", {
  data <- data.frame(age = age_labels("five", min = 15, max = 50),
                     asfr = 1:7)
  empty_colnum <- integer()
  names(empty_colnum) <- character()
  ans_obtained <- tfr_inner(data,
                            asfr_colnum = c(asfr = 2L),
                            age_colnum = c(age = 1L),
                            sex_colnum = empty_colnum,
                            denominator = 2,
                            suffix = NULL)
  ans_expected <- tibble::tibble(tfr = 5 * sum(1:7) / 2)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'tfr_inner' works with sex, suffix", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 2),
                     sex = rep(c("f", "m"), each = 7),
                     asfr = c(1:7, 2:8))[c(1,3,2,4,5:14),]
  empty_colnum <- integer()
  names(empty_colnum) <- character()
  ans_obtained <- tfr_inner(data,
                            asfr_colnum = c(asfr = 3L),
                            age_colnum = c(age = 1L),
                            sex_colnum = c(sex = 2L),
                            denominator = 1,
                            suffix = "x")
  ans_expected <- tibble::tibble(tfr.x = 5 * sum(1:7, 2:8))
  expect_identical(ans_obtained, ans_expected)
})

test_that("'tfr_inner' works with rvec, no sex", {
  data <- data.frame(age = 12:44,
                     asfr = rvec::runif_rvec(n = 33, n_draw = 10))
  empty_colnum <- integer()
  names(empty_colnum) <- character()
  ans_obtained <- tfr_inner(data,
                            asfr_colnum = c(asfr = 2L),
                            age_colnum = c(age = 1L),
                            sex_colnum = empty_colnum,
                            denominator = 1,
                            suffix = NULL)
  ans_expected <- tibble::tibble(tfr = sum(data$asfr))
  expect_identical(ans_obtained, ans_expected)
  expect_true(rvec::is_rvec(ans_obtained$tfr))
})

test_that("'tfr_inner' throws correct error with sex", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 2),
                     sex = rep(c("f", "m"), each = 7),
                     asfr = c(1:7, 2:8))
  data$asfr[[3]] <- -1
  empty_colnum <- integer()
  names(empty_colnum) <- character()
  expect_error(tfr_inner(data,
                         asfr_colnum = c(asfr = 3L),
                         age_colnum = c(age = 1L),
                         sex_colnum = c(sex = 2L),
                         denominator = 1,
                         suffix = "x"),
               "`asfr` has negative value")
})

test_that("'tfr_inner' checks age sexparately within sex", {
  data <- data.frame(age = rep(age_labels("five", min = 15, max = 50), 2),
                     sex = rep(c("f", "m"), each = 7),
                     asfr = c(1:7, 2:8))
  data <- data[-3,]
  empty_colnum <- integer()
  names(empty_colnum) <- character()
  expect_error(tfr_inner(data,
                         asfr_colnum = c(asfr = 3L),
                         age_colnum = c(age = 1L),
                         sex_colnum = c(sex = 2L),
                         denominator = 1,
                         suffix = "x"),
               "Age group \"25-29\" is missing.")
})






    
