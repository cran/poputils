## 'lifetab' --------------------------------------------------------

test_that("'lifetab' works with valid inputs, mx, no 'by'", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"))
    ans <- lifetab(data = data,
                   mx = mx)
    expect_true(tibble::is_tibble(ans))
    expect_identical(names(ans), c("age", "qx", "lx", "dx", "Lx", "ex"))
})

test_that("'lifetab' works with valid inputs, qx, no 'by'", {
    data <- tibble::tibble(qx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"))
    ans <- lifetab(data = data,
                   qx = qx,
                   age = age)
    expect_true(tibble::is_tibble(ans))
    expect_identical(names(ans), c("age", "qx", "lx", "dx", "Lx", "ex"))
})

test_that("'lifetab' works with valid inputs, with 'by', mx", {
    data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans <- lifetab(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region,
                   infant = "AK",
                   child = "CD",
                   radix = 1)
    expect_identical(ans[1:3], data[2:4])
})

test_that("'lifetab' works with valid inputs, with 'by', qx", {
    data <- tibble::tibble(qx = rep(c(0.02, 0.01, 0.015, 1), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans <- lifetab(data = data,
                   qx = qx,
                   age = age,
                   sex = gender,
                   by = region,
                   infant = "AK",
                   child = "CD",
                   radix = 1)
    expect_identical(ans[1:3], data[2:4])
})

test_that("'lifetab' gives same answer with 'by' and 'group_by', mx", {
    data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans_by <- lifetab(data = data,
                      mx = mx,
                      age = age,
                      sex = gender,
                      by = region)
    data_group <- dplyr::group_by(data, region)
    ans_group <- lifetab(data = data_group,
                         mx = mx,
                         age = age,
                         sex = gender)
    expect_identical(ans_by, ans_group)
})

test_that("'lifetab' allows overlap between 'sex' and 'by' args", {
    data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans_sex <- lifetab(data = data,
                      mx = mx,
                      age = age,
                      sex = gender,
                      by = c(region, gender))
    ans_nosex <- lifetab(data = data,
                         mx = mx,
                         age = age,
                         sex = gender,
                         by = region)
    expect_identical(ans_sex, ans_nosex)
})

test_that("'lifetab' works with n_core = 2", {
  data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                         age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                         gender = rep(rep(c("f", "m"), each = 4), times = 2),
                         region = rep(c("a", "b"), each = 8))
  ans_1 <- lifetab(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region)
  ans_2 <- lifetab(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region,
                   n_core = 2)
  expect_equal(ans_1, ans_2)
})


## 'lifeexp' --------------------------------------------------------

test_that("'lifeexp' works with valid inputs, no 'by'", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"))
    ans <- lifeexp(data = data,
                   mx = mx,
                   age = age)
    expect_true(tibble::is_tibble(ans))
    expect_identical(names(ans), "ex")
})

test_that("'lifetab' works with valid inputs, with 'by'", {
    data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans <- lifeexp(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region,
                   infant = "AK",
                   child = "CD")
    expect_identical(ans[1:2], unique(data[c("region", "gender")]))
})

test_that("'lifetab' works with valid inputs, with n_core = 2", {
    data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                           age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                           gender = rep(rep(c("f", "m"), each = 4), times = 2),
                           region = rep(c("a", "b"), each = 8))
    ans1 <- lifeexp(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region,
                   infant = "AK",
                   child = "CD")
    ans2 <- lifeexp(data = data,
                   mx = mx,
                   age = age,
                   sex = gender,
                   by = region,
                   infant = "AK",
                   child = "CD",
                   n_core = 2)
    expect_equal(ans1, ans2)
})

test_that("'lifeexp' works with at = 5", {
  data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                         age = c("0", "1-4", "5-9", "10+"))
  ans_obtained <- lifeexp(data = data,
                          mx = mx,
                          at = 5,
                          age = age)
  ans_expected <- lifetab(data = data,
                          mx = mx,
                          age = age)[3,"ex"]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'lifeexp' works with valid inputs, with 'by', length(at) > 1", {
  data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                         age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                         gender = rep(rep(c("f", "m"), each = 4), times = 2),
                         region = rep(c("a", "b"), each = 8))
  ans <- lifeexp(data = data,
                 mx = mx,
                 age = age,
                 sex = gender,
                 by = region,
                 at = c(0, 5),
                 infant = "AK",
                 child = "CD")
  expect_identical(ans[1:3],
                   tibble::tibble(region = rep(c("a", "b"), each = 4),
                                  gender = rep(rep(c("f", "m"), each = 2), times = 2),
                                  at = rep(c(0L, 5L), times = 4)))
})

test_that("'lifeexp' works with existing 'ex' column and multiple 'at'", {
  suppressMessages(ans <- west_lifetab[west_lifetab$level == 10, ] |>
                     lifeexp(mx = mx, at = c(0, 60), sex = sex))
  expect_identical(ans[1:2],
                   tibble::tibble(sex = rep(c("Female", "Male"), each = 2),
                                  at = rep(c(0L, 60L), times = 2)))
})


## 'q0_to_m0' -----------------------------------------------------------------

test_that("'q0_to_m0' works with valid inputs - all defaults", {
  q0 <- 0.23
  ans_obtained <- q0_to_m0(q0)
  data <- data.frame(qx = c(q0, 1), age = c("0", "1+"))
  lifetab <- lifetab(data, qx = qx)
  ans_expected <- lifetab$dx[1] / lifetab$Lx[1]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0' works with valid inputs - a0 supplied", {
  q0 <- 0.23
  a0 <- 0.7
  ans_obtained <- q0_to_m0(q0, a0 = a0)
  data <- data.frame(qx = c(q0, 1), age = c("0", "1+"), ax = c(a0, 0.4))
  lifetab <- lifetab(data, qx = qx, ax = ax)
  ans_expected <- lifetab$dx[1] / lifetab$Lx[1]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0' works with valid inputs - infant is linear", {
  q0 <- 0.23
  ans_obtained <- q0_to_m0(q0, infant = "linear")
  data <- data.frame(qx = c(q0, 1), age = c("0", "1+"))
  lifetab <- lifetab(data, qx = qx, infant = "linear")
  ans_expected <- lifetab$dx[1] / lifetab$Lx[1]
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0' works with valid inputs - qx length 2 rvec", {
  q0 <- rvec::rvec(matrix((1:4)/10, nr = 2))
  ans_obtained <- q0_to_m0(q0, infant = "linear")
  data1 <- data.frame(qx = c(q0[1], 1), age = c("0", "1+"))
  data2 <- data.frame(qx = c(q0[2], 1), age = c("0", "1+"))
  lifetab1 <- lifetab(data1, qx = qx, infant = "linear")
  lifetab2 <- lifetab(data2, qx = qx, infant = "linear")
  ans_expected <- c(lifetab1$dx[1] / lifetab1$Lx[1],
                    lifetab2$dx[1] / lifetab2$Lx[1])
  expect_equal(ans_obtained, ans_expected)
})

test_that("'q0_to_m0' works with valid inputs - qx length 2 rvec", {
  q0 <- west_lifetab$qx[west_lifetab$age == "0"]
  sex <- west_lifetab$sex[west_lifetab$age == "0"]
  ans_obtained <- q0_to_m0(q0, sex = sex, infant = "CD")
  ans_expected <- west_lifetab$mx[west_lifetab$age == "0"]
  expect_equal(ans_obtained, ans_expected)
})


test_that("'q0_to_m0' throws correct error when 'a0' is an rvec", {
  q0 <- west_lifetab$qx[west_lifetab$age == "0"]
  sex <- west_lifetab$sex[west_lifetab$age == "0"]
  a0 <- rvec::rvec(west_lifetab$ax[west_lifetab$age == "0"])
  expect_error(q0_to_m0(q0, sex = sex, a0 = a0),
               "`a0` is an rvec.")
})

test_that("'q0_to_m0' throws correct error when 'a0' has values greater than 1", {
  q0 <- west_lifetab$qx[west_lifetab$age == "0"]
  sex <- west_lifetab$sex[west_lifetab$age == "0"]
  a0 <- west_lifetab$ax[west_lifetab$age == "0"]
  a0[3] <- 1.1
  expect_error(q0_to_m0(q0, sex = sex, a0 = a0),
               "`a0` has value greater than 1.")
  a0[2] <- 1.1
  expect_error(q0_to_m0(q0, sex = sex, a0 = a0),
               "`a0` has values greater than 1.")
})

test_that("'q0_to_m0' throws correct error when q0 > 1", {
  q0 <- west_lifetab$qx[west_lifetab$age == "0"]
  sex <- west_lifetab$sex[west_lifetab$age == "0"]
  q0[2] <- 3
  expect_error(q0_to_m0(q0, sex = sex, a0 = a0),
               "`q0` has value greater than 1.")
  q0[3] <- 3
  expect_error(q0_to_m0(q0, sex = sex, a0 = a0),
               "`q0` has values greater than 1.")
})


## 'get_methods_need_sex' -----------------------------------------------------

test_that("'get_methods_need_sex' returns correct value", {
    expect_setequal(get_methods_need_sex(), c("CD", "AK"))
})


## 'lifetab_inner' ------------------------------------------------------------


test_that("'life_inner' throws correct error when passed non-dataframe", {
  expect_error(lifeexp(data = "wrong",
                       mx = mx,
                       age = age,
                       sex = gender,
                       by = region,
                       infant = "AK",
                       child = "CD"),
               "`data` is not a data frame.")
})

test_that("'life_inner' gives correct error when one set of values is incorrect", {
  data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                         age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                         gender = rep(rep(c("f", "m"), each = 4), times = 2),
                         region = rep(c("a", "b"), each = 8))
  data$mx[4] <- -1
  expect_error(lifeexp(data = data,
                       mx = mx,
                       age = age,
                       sex = gender,
                       by = region,
                       infant = "AK",
                       child = "CD"),
               "Problem calculating life table functions.")
})


test_that("'life_inner' gives correct error when one set of values is incorrect - n_core = 2", {
  data <- tibble::tibble(mx = rep(c(0.02, 0.01, 0.015, 0.5), times = 4),
                         age = rep(c("0", "1-4", "5-9", "10+"), times = 4),
                         gender = rep(rep(c("f", "m"), each = 4), times = 2),
                         region = rep(c("a", "b"), each = 8))
  data$mx[4] <- -1
  expect_error(lifeexp(data = data,
                       mx = mx,
                       age = age,
                       sex = gender,
                       by = region,
                       infant = "AK",
                       child = "CD",
                       n_core = 2),
               "Problem calculating life table functions.")
})




## 'life_inner_one' -----------------------------------------------------------

test_that("'lifetab_inner_one' works with valid inputs - lifetable, not rvec", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:3], data[2:4])
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, not rvec", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(1L, 1L))
    expect_identical(names(ans), "ex")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, not rvec - with suffix", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(1L, 1L))
    expect_identical(names(ans), "ex.lt")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, not rvec - with suffix, 'at' length 2", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = c(0, 10),
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(2L, 2L))
    expect_identical(names(ans), c("at", "ex.lt"))
})


test_that("'lifetab_inner_one' works with valid inputs - lifeexp, not rvec; qx", {
    data <- tibble::tibble(qx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum = integer()
    names(mx_colnum) <- character()
    qx_colnum <- c(mx = 1L)
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(1L, 1L))
    expect_identical(names(ans), "ex")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, not rvec; qx - 'at' has length 2", {
    data <- tibble::tibble(qx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           sex= rep("Female", 4))
    mx_colnum = integer()
    names(mx_colnum) <- character()
    qx_colnum <- c(mx = 1L)
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = c(1, 10),
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(2L, 2L))
    expect_identical(names(ans), c("at", "ex"))
})


test_that("'lifetab_inner_one' works with valid inputs - lifetable, rvec", {
    data <- tibble::tibble(mx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                                 c(0.021, 0.011, 0.0151, 0.51))),
                                           age = c("0", "1-4", "5-9", "10+"),
                                           region = rep("A", 4),
                                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:3], data[2:4])
    expect_s3_class(ans[["ex.lt"]], "rvec_dbl")
})

test_that("'lifetab_inner_one' works with valid inputs - lifetable, rvec, qx", {
  data <- tibble::tibble(qx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                               c(0.021, 0.011, 0.0151, 0.51))),
                         age = c("0", "1-4", "5-9", "10+"),
                         region = rep("A", 4),
                         sex= rep("Female", 4))
  mx_colnum <- c(mx = 1L)
  qx_colnum = integer()
  names(qx_colnum) <- character()
  age_colnum <- c(age = 2L)
  sex_colnum <- integer()
  names(sex_colnum) <- character()
  ax_colnum <- integer()
  names(ax_colnum) <- character()
  methods <- c(infant = "constant",
               child = "constant",
               closed = "constant",
               open = "constant")
  radix <- 10
  suffix <- "lt"
  ans <- life_inner_one(data = data,
                        mx_colnum = mx_colnum,
                        qx_colnum = qx_colnum,
                        at = 0,
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:3], data[2:4])
  expect_s3_class(ans[["ex.lt"]], "rvec_dbl")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, is rvec, mx", {
    data <- tibble::tibble(mx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                                 c(0.021, 0.011, 0.0151, 0.51))),
                                           age = c("0", "1-4", "5-9", "10+"),
                                           region = rep("A", 4),
                                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(1L, 1L))
    expect_s3_class(ans[["ex"]], "rvec_dbl")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, is rvec, mx - 'at' has length 2", {
    data <- tibble::tibble(mx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                                 c(0.021, 0.011, 0.0151, 0.51))),
                                           age = c("0", "1-4", "5-9", "10+"),
                                           region = rep("A", 4),
                                           sex= rep("Female", 4))
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = c(0, 10),
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(2L, 2L))
    expect_s3_class(ans[["ex"]], "rvec_dbl")
    expect_identical(ans[[1]], c(0L, 10L))
})


test_that("'lifetab_inner_one' works with valid inputs - lifeexp, is rvec, qx", {
    data <- tibble::tibble(qx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                                 c(0.021, 0.011, 0.0151, 0.51))),
                                           age = c("0", "1-4", "5-9", "10+"),
                                           region = rep("A", 4),
                                           sex= rep("Female", 4))
    mx_colnum = integer()
    names(mx_colnum) <- character()
    qx_colnum <- c(mx = 1L)
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(1L, 1L))
    expect_s3_class(ans[["ex"]], "rvec_dbl")
})

test_that("'lifetab_inner_one' works with valid inputs - lifeexp, is rvec, qx - 'at' has length 2", {
    data <- tibble::tibble(qx = rvec::rvec(cbind(c(0.02, 0.01, 0.015, 0.5),
                                                 c(0.021, 0.011, 0.0151, 0.51))),
                                           age = c("0", "1-4", "5-9", "10+"),
                                           region = rep("A", 4),
                                           sex= rep("Female", 4))
    mx_colnum = integer()
    names(mx_colnum) <- character()
    qx_colnum <- c(mx = 1L)
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- integer()
    names(sex_colnum) <- character()
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- NULL
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = c(0, 5),
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = FALSE)
    expect_true(is.data.frame(ans))
    expect_identical(dim(ans), c(2L, 2L))
    expect_identical(ans[[1]], c(0L, 5L))
    expect_s3_class(ans[["ex"]], "rvec_dbl")
})


test_that("'lifetab_inner_one' works with valid inputs - lifetable, not rvec, out of order, non-standard sex", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"),
                           region = rep("A", 4),
                           gender = rep("F", 4))
    data <- data[c(3, 1, 2, 4), ]
    mx_colnum <- c(mx = 1L)
    qx_colnum = integer()
    names(qx_colnum) <- character()
    age_colnum <- c(age = 2L)
    sex_colnum <- c(gender = 4L)
    ax_colnum <- integer()
    names(ax_colnum) <- character()
    methods <- c(infant = "CD",
                 child = "CD",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = 0,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:3], data[c(2,3,1,4),2:4])
})

test_that("'lifetab_inner_one' works with valid inputs - ax supplied", {
  data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                         ax = c(0.2, 2, 2.5, 3),
                         age = c("0", "1-4", "5-9", "10+"),
                         region = rep("A", 4),
                         sex= rep("Female", 4))
  mx_colnum <- c(mx = 1L)
  qx_colnum = integer()
  names(qx_colnum) <- character()
  age_colnum <- c(age = 3L)
  sex_colnum <- c(sex = 5L)
  ax_colnum <- c(ax = 2L)
  methods <- c(infant = "constant",
               child = "constant",
               closed = "constant",
               open = "constant")
  radix <- 10
  suffix <- "lt"
  ans <- life_inner_one(data = data,
                        mx_colnum = mx_colnum,
                        qx_colnum = qx_colnum,
                        at = 0,
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:4], data[-1])
})

test_that("'lifetab_inner_one' works with valid inputs - is qx", {
  data <- tibble::tibble(qx = c(0.02, 0.01, 0.015, 0.5),
                         ax = c(0.2, 2, 2.5, 3),
                         age = c("0", "1-4", "5-9", "10+"),
                         region = rep("A", 4),
                         sex= rep("Female", 4))
  mx_colnum = integer()
  names(mx_colnum) <- character()
  qx_colnum = c(qx = 1L)
  names(qx_colnum) <- character()
  age_colnum <- c(age = 3L)
  sex_colnum <- c(sex = 5L)
  ax_colnum <- c(ax = 2L)
  methods <- c(infant = "constant",
               child = "constant",
               closed = "constant",
               open = "constant")
  radix <- 10
  suffix <- "lt"
  ans <- life_inner_one(data = data,
                        mx_colnum = mx_colnum,
                        qx_colnum = qx_colnum,
                        at = 0,
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:4], data[2:5])
})


test_that("'lifetab_inner_one' works with valid inputs - is qx", {
  data <- tibble::tibble(qx = c(0.02, 0.01, 0.015, 0.5),
                         ax = c(0.2, 2, 2.5, 3),
                         age = c("0", "1-4", "5-9", "10+"),
                         region = rep("A", 4),
                         sex= rep("Female", 4))
  mx_colnum = integer()
  names(mx_colnum) <- character()
  qx_colnum = c(qx = 1L)
  names(qx_colnum) <- character()
  age_colnum <- c(age = 3L)
  sex_colnum <- c(sex = 5L)
  ax_colnum <- c(ax = 2L)
  methods <- c(infant = "constant",
               child = "constant",
               closed = "constant",
               open = "constant")
  radix <- 10
  suffix <- "lt"
  ans <- life_inner_one(data = data,
                        mx_colnum = mx_colnum,
                        qx_colnum = qx_colnum,
                        at = 0,
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:4], data[2:5])
})



## 'mx_to_lifetab' ------------------------------------------------------------

test_that("'mx_to_lifetab' works with valid inputs", {
    mx <- matrix(c(0.02, 0.01, 0.015, 0.5), ncol = 1)
    age_group_categ <- c("0", "1-4", "five", "open")
    sex <- rep(NA_character_, 5)
    ax <- rep(NA_real_, 5)
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- mx_to_lifetab(mx = mx,
                         age_group_categ = age_group_categ,
                         sex = sex,
                         ax = ax,
                         methods = methods,
                         radix = radix,
                         suffix = suffix)
    expect_identical(names(ans),
                     c("qx.lt", "lx.lt", "dx.lt", "Lx.lt", "ex.lt"))
    expect_equal(ans$lx.lt[[1]], radix)
    expect_equal(ans$ex.lt[[1]], sum(ans$Lx.lt) / radix)
})


## 'qx_to_lifetab' ------------------------------------------------------------

test_that("'qx_to_lifetab' works with valid inputs", {
    qx <- matrix(c(0.02, 0.01, 0.015, 0.5), ncol = 1)
    age_group_categ <- c("0", "1-4", "five", "open")
    sex <- rep(NA_character_, 5)
    ax <- rep(NA_real_, 5)
    methods <- c(infant = "constant",
                 child = "constant",
                 closed = "constant",
                 open = "constant")
    radix <- 10
    suffix <- "lt"
    ans <- qx_to_lifetab(qx = qx,
                         age_group_categ = age_group_categ,
                         sex = sex,
                         ax = ax,
                         methods = methods,
                         radix = radix,
                         suffix = suffix)
    expect_identical(names(ans),
                     c("qx.lt", "lx.lt", "dx.lt", "Lx.lt", "ex.lt"))
    expect_equal(ans$lx.lt[[1]], radix)
    expect_equal(ans$ex.lt[[1]], sum(ans$Lx.lt) / radix)
})


## 'remove_existing_lifetab_cols' ---------------------------------------------

test_that("'remove_existing_lifetab_cols' returns data untouched if no liftab cols present - mx", {
  data <- west_lifetab[c("level", "sex", "age", "mx")]
  ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = NULL)
  ans_expected <- data
  expect_identical(ans_obtained, ans_expected)
})


test_that("'remove_existing_lifetab_cols' returns data untouched if no liftab cols present - qx", {
  data <- west_lifetab[c("qx", "level", "sex", "age")]
  ans_obtained <- remove_existing_lifetab_cols(data, is_table = FALSE, suffix = NULL)
  ans_expected <- data
  expect_identical(ans_obtained, ans_expected)
})

test_that("'remove_existing_lifetab_cols' removes columns if liftab cols present - mx, is_table = TRUE", {
  data <- west_lifetab[c("mx", "level", "sex", "age", "Lx", "lx")]
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = NULL))
  ans_expected <- data[1:4]
  expect_identical(ans_obtained, ans_expected)
  data <- west_lifetab[c("mx", "level", "sex", "age", "qx", "Lx")]
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = NULL))
  ans_expected <- data[1:5]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'remove_existing_lifetab_cols' removes columns if liftab cols present - mx, is_table = FALSE", {
  data <- west_lifetab[c("mx", "level", "sex", "age", "Lx", "lx", "ex")]
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = FALSE, suffix = NULL))
  ans_expected <- data[1:6]
  expect_identical(ans_obtained, ans_expected)
  data <- west_lifetab[c("mx", "level", "sex", "age", "qx", "ex", "Lx")]
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = FALSE, suffix = NULL))
  ans_expected <- data[-6]
  expect_identical(ans_obtained, ans_expected)
})

test_that("'remove_existing_lifetab_cols' removes columns if liftab cols present - mx, has suffix", {
  data <- west_lifetab[c("mx", "level", "sex", "age", "Lx", "lx")]
  names(data)[5:6] <- paste(names(data)[5:6], "lt", sep = ".")
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = "lt"))
  ans_expected <- data[1:4]
  expect_identical(ans_obtained, ans_expected)
  data <- west_lifetab[c("mx", "level", "sex", "age", "qx", "Lx")]
  names(data)[6] <- paste(names(data)[6], "lt", sep = ".")
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = "lt"))
  ans_expected <- data[1:5]
  expect_identical(ans_obtained, ans_expected)
  data <- west_lifetab[c("mx", "level", "sex", "age", "qx", "Lx")]
  suppressMessages(ans_obtained <- remove_existing_lifetab_cols(data, is_table = TRUE, suffix = "lt"))
  ans_expected <- data
  expect_identical(ans_obtained, ans_expected)
})

    
