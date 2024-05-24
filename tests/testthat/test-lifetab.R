## 'lifetab' --------------------------------------------------------

test_that("'lifetab' works with valid inputs, no 'by'", {
    data <- tibble::tibble(mx = c(0.02, 0.01, 0.015, 0.5),
                           age = c("0", "1-4", "5-9", "10+"))
    ans <- lifetab(data = data,
                   mx = mx,
                   age = age)
    expect_true(tibble::is_tibble(ans))
    expect_identical(names(ans), c("mx", "age", "qx", "lx", "dx", "Lx", "ex"))
})

test_that("'lifetab' works with valid inputs, with 'by'", {
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
    expect_identical(ans[1:4], data)
})

test_that("'lifetab' gives same answer with 'by' and 'group_by'", {
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
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:4], data)
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
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:4], data)
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
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = TRUE)
    expect_true(is.data.frame(ans))
    expect_identical(ans[1:4], data[c(2, 3, 1, 4), ])
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
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:5], data)
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
                        age_colnum = age_colnum,
                        sex_colnum = sex_colnum,
                        ax_colnum = ax_colnum,
                        methods = methods,
                        radix = radix,
                        suffix = suffix,
                        is_table = TRUE)
  expect_true(is.data.frame(ans))
  expect_identical(ans[1:5], data)
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
