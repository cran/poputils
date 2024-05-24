
test_that("'reformat_sex' works with valid inputs - factor, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(c("Female", "Male", "Male", NA, "Female"),
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- NA
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(NA,
                           levels = c("Female", "Male", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length > 0", {
    x <- c("fem", "MA", "  boys  ", NA, "F")
    ans_obtained <- reformat_sex(x, factor = FALSE)
    ans_expected <- c("Female", "Male", "Male", NA, "Female")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - factor, length == 0", {
    ans_obtained <- reformat_sex(character())
    ans_expected <- factor(character(), levels = c("Female", "Male"))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with valid inputs - character, length == 0", {
    ans_obtained <- reformat_sex(character(), factor = FALSE)
    ans_expected <- character()
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with vector except", {
    x <- c("fem", "MA", "  boys  ", NA, "F", "X", "Y")
    ans_obtained <- reformat_sex(x, factor = FALSE, except = c("X", "Y"))
    ans_expected <- c("Female", "Male", "Male", NA, "Female", "X", "Y")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works with factor except", {
    x <- c("fem", "MA", "  boys  ", NA, "F", "X", "Y")
    ans_obtained <- reformat_sex(x, factor = FALSE, except = factor(c("X", "Y")))
    ans_expected <- c("Female", "Male", "Male", NA, "Female", "X", "Y")
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works when x is factor", {
    x <- factor(c("F", "M", NA))
    ans_obtained <- reformat_sex(x)
    ans_expected <- factor(c("Female", "Male", NA), exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' works when x is factor, with except", {
    x <- factor(c("F", "M", NA, "D"))
    ans_obtained <- reformat_sex(x, except = "D")
    ans_expected <- factor(c("Female", "Male", NA, "D"),
                           levels = c("Female", "Male", "D", NA),
                           exclude = NULL)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_sex' throws correct error with invalid label", {
    expect_error(reformat_sex("wrong"),
                 "Can't parse label \"wrong\".")
})

test_that("'reformat_sex' throws correct error with invalid except", {
    expect_error(reformat_sex("M", except = lm),
                 "`except` is not a vector or a factor.")
})






    
