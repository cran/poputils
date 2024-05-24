
## 'groups_colnums' -----------------------------------------------------------

test_that("'groups_colnums' works with grouped data frame", {
    data <- data.frame(a = -1, b = 99, c = "x")
    data <- dplyr::group_by(data, c, a)
    ans_obtained <- groups_colnums(data)
    ans_expected <- c(c = 3L, a = 1L)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'groups_colnums' works with non-grouped data frame", {
    data <- data.frame(a = -1, b = 99, c = "x")
    ans_obtained <- groups_colnums(data)
    ans_expected <- integer()
    names(ans_expected) <- character()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'groups_colnums' throws appopriate error with non data frame", {
    expect_error(groups_colnums("a"),
                 "`data` is not a data frame.")
})


## 'make_str_key' -------------------------------------------------------------

test_that("'make_str_key' works with valid input", {
    row <- data.frame(a = 1, b = "x")
    row$c <- list(1:3)
    row$d <- rvec::rvec(matrix(1:3, 1))
    ans <- make_str_key(row)
    expect_true(is.character(ans))
    expect_identical(length(ans), 1L)
})
    
    
## 'matrix_to_list_of_cols' ---------------------------------------------------

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3)
    colnames(m) <- c("a", "b", "c")
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(a = 1:4, b = 5:8, c = 9:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_cols(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_cols' raises expected error message when m is not a matrix", {
    expect_error(matrix_to_list_of_cols("a"),
                 "`m` is not a matrix.")
})


## 'matrix_to_list_of_rows' ---------------------------------------------------

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol > 0", {
    m <- matrix(1:12, nr = 4, nc = 3, byrow = TRUE)
    rownames(m) <- c("a", "b", "c", "d")
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol > 0", {
    m <- matrix(NA, nr = 0, nc = 3)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow > 0, ncol = 0", {
    m <- matrix(NA, nr = 3, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list(logical(), logical(), logical())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' works with nrow = 0, ncol = 0", {
    m <- matrix(1, nr = 0, nc = 0)
    ans_obtained <- matrix_to_list_of_rows(m)
    ans_expected <- list()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'matrix_to_list_of_rows' raises expected error message when m is not a matrix", {
    expect_error(matrix_to_list_of_rows("a"),
                 "`m` is not a matrix.")
})
