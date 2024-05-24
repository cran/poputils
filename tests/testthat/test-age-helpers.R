
## Helpers for age labels

## 'age_group_categ' -----------------------------------------------------------

test_that("'age_group_categ' works with life table age groups", {
    x <- c("5-9", "0", "10+", "1-4", NA)
    ans_obtained <- age_group_categ(x)
    ans_expected <- c("five", "0", "open", "1-4", NA)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_group_categ' works with single age groups", {
    x <- c("5", "0", "10+", NA)
    ans_obtained <- age_group_categ(x)
    ans_expected <- c("single", "0", "open", NA)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_group_categ' works with five-year age groups", {
    x <- c("5-9", "0-4", "10+", NA)
    ans_obtained <- age_group_categ(x)
    ans_expected <- c("five", "five", "open", NA)
    expect_identical(ans_obtained, ans_expected)
})


## 'age_labels_single' --------------------------------------------------------

test_that("'age_labels_single' gives correct answer with all defaults", {
    expect_identical(age_labels_single(min = 0L, max = 100L, open = TRUE),
                     c(0:99, "100+"))
})

test_that("'age_labels_single' gives correct answer with non-default 'open'", {
    expect_identical(age_labels_single(min = 0L, max = 100L, open = FALSE),
                     as.character(0:99))
})

test_that("'age_labels_single' gives correct answer with non-default 'min'", {
    expect_identical(age_labels_single(min = 20L, max = 100L, open = FALSE),
                     as.character(20:99))
})

test_that("'age_labels_single' gives correct answer with non-default 'min' and 'open'", {
    expect_identical(age_labels_single(min = 20L, max = 100L, open = TRUE),
                     c(20:99, "100+"))
})

test_that("'age_labels_single' gives correct answer with single open age group", {
    expect_identical(age_labels_single(min = 0L, max = 0L, open = TRUE),
                     "0+")
    expect_identical(age_labels_single(min = 100L, max = 100L, open = TRUE),
                     "100+")
})

test_that("'age_labels_single' gives correct answer with single closed age group", {
    expect_identical(age_labels_single(min = 0L, max = 1L, open = FALSE),
                     "0")
    expect_identical(age_labels_single(min = 100, max = 101, open = FALSE),
                     "100")
})

test_that("'age_labels_single' gives correct answer with non-default 'min', 'max', and 'open'", {
    expect_identical(age_labels_single(min = 20L, max = 30L, open = TRUE),
                     c(20:29, "30+"))
})

test_that("'age_labels_single' gives correct answer with negative 'min', 'max'", {
    expect_identical(age_labels_single(min = -20L, max = -10L, open = FALSE),
                     as.character((-20):(-11)))
})


## 'age_labels_five' ----------------------------------------------------------

test_that("'age_labels_five' gives correct answer with all defaults", {
    expect_identical(age_labels_five(min = 0L, max = 100L, open = TRUE),
                     c(paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"), "100+"))
})

test_that("'age_labels_five' gives correct answer with non-default 'open'", {
    expect_identical(age_labels_five(min = 0L, max = 100L, open = FALSE),
                     paste(seq(0, 95, 5), seq(4, 99, 5), sep = "-"))
})

test_that("'age_labels_five' gives correct answer with non-default 'min'", {
    expect_identical(age_labels_five(min = 20L, max = 100L, open = FALSE),
                     paste(seq(20, 95, 5), seq(24, 99, 5), sep = "-"))
})

test_that("'age_labels_five' gives correct answer with non-default 'min' and 'open'", {
    expect_identical(age_labels_five(min = 20L, max = 100L, open = TRUE),
                     c(paste(seq(20, 95, 5), seq(24, 99, 5), sep = "-"), "100+"))
})

test_that("'age_labels_five' gives correct answer with single open age group", {
    expect_identical(age_labels_five(min = 0L, max = 0L, open = TRUE),
                     "0+")
    expect_identical(age_labels_five(min = 100L, max = 100L, open = TRUE),
                     "100+")
})

test_that("'age_labels_five' gives correct answer with single closed age group", {
    expect_identical(age_labels_five(min = 0L, max = 5L, open = FALSE),
                     "0-4")
    expect_identical(age_labels_five(min = 100L, max = 105L, open = FALSE),
                     "100-104")
})

test_that("'age_labels_five' gives correct answer with negative 'min', 'max'", {
    expect_identical(age_labels_five(min = -20L, max = -10L, open = FALSE),
                     c("-20--16", "-15--11"))
})

test_that("'age_labels_five' gives correct answer with non-default 'min', 'max', and 'open'", {
    expect_identical(age_labels_five(min = 20L, max = 30L, open = TRUE),
                     c("20-24", "25-29", "30+"))
})

test_that("'age_labels_five' throws correct error when 'min' not divisible by 5", {
    expect_error(age_labels_five(min = 19L, max = 30L, open = FALSE),
                 "'min' \\[19\\] not divisible by 5")
})

test_that("'age_labels_five' throws correct error when 'max' not divisible by 5", {
    expect_error(age_labels_five(min = 15L, max = 31L, open = FALSE),
                 "'max' \\[31\\] not divisible by 5")
})


## 'age_labels_lt' ----------------------------------------------------------------

test_that("'age_labels_lt' gives correct answer with all defaults", {
    expect_identical(age_labels_lt(min = 0L, max = 100L, open = TRUE),
                     c("0", "1-4", paste(seq(5, 95, 5), seq(9, 99, 5), sep = "-"), "100+"))
})

test_that("'age_labels_lt' gives correct answer with 'max' equals 0", {
    expect_identical(age_labels_lt(min = 0L, max = 0L, open = TRUE),
                     "0+")
})

test_that("'age_labels_lt' gives correct answer with 'max' equals 1", {
    expect_identical(age_labels_lt(min = 0L, max = 1L, open = TRUE),
                     c("0", "1+"))
    expect_identical(age_labels_lt(min = 0L, max = 1L, open = FALSE),
                     "0")
})

test_that("'age_labels_lt' gives correct answer with 'max' equals 5", {
    expect_identical(age_labels_lt(min = 0L, max = 5L, open = TRUE),
                     c("0", "1-4", "5+"))
    expect_identical(age_labels_lt(min = 0L, max = 5L, open = FALSE),
                     c("0", "1-4"))
})

test_that("'age_labels_lt' gives correct answer when 'max' equals 10", {
    expect_identical(age_labels_lt(min = 0L, max = 10L, open = TRUE),
                     c("0", "1-4", "5-9", "10+"))
    expect_identical(age_labels_lt(min = 0L, max = 10, open = FALSE),
                     c("0", "1-4", "5-9"))
})

test_that("'age_labels_lt' gives correct answer when 'min' equals 1L", {
    expect_identical(age_labels_lt(min = 1L, max = 1L, open = TRUE),
                     "1+")
    expect_identical(age_labels_lt(min = 1L, max = 5L, open = TRUE),
                     c("1-4", "5+"))
    expect_identical(age_labels_lt(min = 1L, max = 5L, open = FALSE),
                     "1-4")
    expect_identical(age_labels_lt(min = 1L, max = 10L, open = TRUE),
                     c("1-4", "5-9", "10+"))
    expect_identical(age_labels_lt(min = 1L, max = 10L, open = FALSE),
                     c("1-4", "5-9"))
})

test_that("'age_labels_lt' gives correct answer when 'min' equals 5L", {
    expect_identical(age_labels_lt(min = 5L, max = 5L, open = TRUE),
                     "5+")
    expect_identical(age_labels_lt(min = 5L, max = 10L, open = TRUE),
                     c("5-9", "10+"))
    expect_identical(age_labels_lt(min = 5L, max = 10L, open = FALSE),
                     "5-9")
})

test_that("'age_labels_lt' throws correct error when 'min' negative", {
    expect_error(age_labels_lt(min = -1L, max = 100L, open = TRUE),
                 "'min' equals -1 : negative values not allowed in life table age groups")
})

test_that("'age_labels_lt' throws correct error when 'min', 'max' not life table ages", {
    expect_error(age_labels_lt(min = 1L, max = 2L, open = TRUE),
                 "age group derived from 'min' \\[1\\] and 'max' \\[2\\] not a valid life table age group")
})

test_that("'age_labels_lt' throws correct error when 'min', 'max' not life table ages", {
    expect_error(age_labels_lt(min = 2L, max = 5L, open = TRUE),
                 "'min' \\[2\\] not valid for a life table age group")
    expect_error(age_labels_lt(min = 2L, max = 6L, open = TRUE),
                 "'min' \\[2\\] not valid for a life table age group")
    expect_error(age_labels_lt(min = 12L, max = 15L, open = TRUE),
                 "'min' \\[12\\] not divisible by 5")
})

test_that("'age_labels_lt' throws correct error when 'min' not divisible by 5", {
    expect_error(age_labels_lt(min = 11L, max = 100L, open = TRUE),
                 "'min' \\[11\\] not divisible by 5")
})

test_that("'age_labels_lt' throws correct error when 'max' not divisible by 5", {
    expect_error(age_labels_lt(min = 0L, max = 29L, open = FALSE),
                 "'max' \\[29\\] not divisible by 5")
})

test_that("'age_labels_lt' throws correct error when 'max' is 3", {
    expect_error(age_labels_lt(min = 0L, max = 3, open = FALSE),
                 "age group derived from 'min' \\[0\\] and 'max' \\[3\\] not a valid life table age group")
})



## Helpers for age_mid, age_lower, age_upper, age_group_type %%%%%%%%%%%%%%%%%%%%%%%%

## age_limits -----------------------------------------------------------------

test_that("'age_limits' gives correct answer valid 1-year age groups - factor", {
    ans_obtained <- age_limits(reformat_age(c("60+", "5", NA, "0")))
    ans_expected <- list(lower = c(60, 5, NA, 0),
                         upper = c(Inf, 6, NA, 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer valid 1-year age groups - non-factor", {
    ans_obtained <- age_limits(c("60+", "5", NA, "0"))
    ans_expected <- list(lower = c(60, 5, NA, 0),
                         upper = c(Inf, 6, NA, 1))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer valid 5-year age groups - factor", {
    ans_obtained <- age_limits(reformat_age(c("60+", "5-9", NA, "0-4")))
    ans_expected <- list(lower = c(60, 5, NA, 0),
                         upper = c(Inf, 10, NA, 5))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer valid 5-year age groups - non-factor", {
    ans_obtained <- age_limits(c("60+", "5-9", NA, "0-4"))
    ans_expected <- list(lower = c(60, 5, NA, 0),
                         upper = c(Inf, 10, NA, 5))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer valid life table age groups - factor", {
    ans_obtained <- age_limits(reformat_age(c("60+", "5-9", NA, "1-4")))
    ans_expected <- list(lower = c(60, 5, NA, 1),
                         upper = c(Inf, 10, NA, 5))
    expect_identical(ans_obtained, ans_expected)
})


test_that("'age_limits' gives correct answer valid life table age groups - non-factor", {
    ans_obtained <- age_limits(c("60+", "5-9", NA, "1-4"))
    ans_expected <- list(lower = c(60, 5, NA, 1),
                         upper = c(Inf, 10, NA, 5))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer with zero-length input - factor", {
    ans_obtained <- age_limits(reformat_age(character()))
    ans_expected <- list(lower = numeric(),
                         upper = numeric())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer with zero-length input - non-factor", {
    ans_obtained <- age_limits(character())
    ans_expected <- list(lower = numeric(),
                         upper = numeric())
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer with all-NA input - factor", {
    ans_obtained <- age_limits(reformat_age(c(NA, NA)))
    ans_expected <- list(lower = as.numeric(c(NA, NA)),
                         upper = as.numeric(c(NA, NA)))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'age_limits' gives correct answer with all-NA input - non-factor", {
    ans_obtained <- age_limits(c(NA, NA))
    ans_expected <- list(lower = as.numeric(c(NA, NA)),
                         upper = as.numeric(c(NA, NA)))
    expect_identical(ans_obtained, ans_expected)
})


## Helpers for reformat_age %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## 'reformat_age_five' -----------------------------------------------------------

test_that("'reformat_age_five' returns cleaned 'x' when 'x' denotes 5-year age groups - no NAs, numeric", {
    x <- seq(0, 100, 5)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    ans_obtained <- reformat_age_five(x)
    ans_expected <- paste(x, x+4, sep = "-")
    ans_expected[x == 100] <- "100+"
    levels <- age_labels_five(min = 0, max = 100, open = TRUE)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_age_five' returns cleaned 'x' when 'x' denotes 5-year age groups - with NAs, character", {
    x <- c(seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x <- as.character(x)
    ans_obtained <- reformat_age_five(x)
    labels <- c(age_labels(type = "five", max = 50), NA)
    levels <- c(seq(0, 50, 5), NA)
    ans_expected <- labels[match(x, levels)]
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_age_five' returns NULL when 'x' has length 0", {
    expect_null(reformat_age_five(character()))
})

test_that("'reformat_age_five' returns NULL when 'x' has non-numeric elements - no NA", {
    x <- seq(0, 100, 5)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x[[10L]] <- "wrong"
    expect_null(reformat_age_five(x))
})

test_that("'reformat_age_five' returns NULL when 'x' has non-numeric elements - with NA", {
    x <- c(seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x <- as.character(x)
    x[[10L]] <- "wrong"
    expect_null(reformat_age_five(x))
})

test_that("'reformat_age_five' returns NULL when 'x' not five-year intervals", {
    x <- c(seq(0, 40, 4), NA)
    expect_null(reformat_age_five(x))
})



## reformat_age_lt ---------------------------------------------------------------

test_that("'reformat_age_lt' returns cleaned 'x' when 'x' denotes life table age groups - numeric, no NA", {
    x <- c(1L, seq(0, 100, 5))
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    ans_obtained <- reformat_age_lt(x)
    ans_expected <- paste(x, x+4, sep = "-")
    ans_expected[x == 0] <- "0"
    ans_expected[x == 1] <- "1-4"
    ans_expected[x == 100] <- "100+"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_age_lt' returns cleaned 'x' when 'x' denotes life table age groups - character, with NA", {
    x <- c(1L, seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x_int <- x
    x <- as.character(x)
    ans_obtained <- reformat_age_lt(x)
    ans_expected <- x
    is_0 <- !is.na(x) & (x_int == 0)
    is_1 <- !is.na(x) & (x_int == 1)
    is_mid <- !is.na(x) & (x_int > 1) & (x_int < 50)
    is_50 <- !is.na(x) & (x_int == 50)
    ans_expected[is_0] <- "0"
    ans_expected[is_1] <- "1-4"
    ans_expected[is_mid] <- paste(x_int[is_mid], x_int[is_mid] + 4, sep = "-")
    ans_expected[is_50] <- "50+"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'reformat_age_lt' returns NULL when 'x' has length 0", {
    expect_null(reformat_age_lt(character()))
})

test_that("'reformat_age_lt' returns NULL when 'x' has non-numeric elements - no NAs", {
    x <- c(1, seq(0, 100, 5))
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x[[10L]] <- "wrong"
    expect_null(reformat_age_lt(x))
})

test_that("'reformat_age_lt' returns NULL when 'x' has non-numeric elements - with NAs", {
    x <- c(1, seq(0, 50, 5), NA)
    x <- rep(x, each = 10)
    x <- sample(x, size = length(x))
    x <- as.character(x)
    x[[10L]] <- "wrong"
    expect_null(reformat_age_lt(x))
})

test_that("'reformat_age_lt' returns NULL when 'x' is non-life-table age groups", {
    x <- c(0, 1, 5, 10, 14, seq(20, 80, 5))
    expect_null(reformat_age_lt(x))
})


## translate_age_labels ----------------------------------------------------------

test_that("'translate_age_labels' correctly interprets valid labels", {
    x <- c("0", "1", seq(5, 60, 5))
    ans_obtained <- translate_age_labels(x)
    ans_expected <- age_labels("lt", max = 60)
    expect_identical(ans_obtained, ans_expected)
    x <- seq(0, 60, 5)
    ans_obtained <- translate_age_labels(x)
    ans_expected <- age_labels("five", max = 60)
    expect_identical(ans_obtained, ans_expected)
    x <- c("0 Year", "1 to 4 Years", "5 to 9 Years", "10 Years And Over")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0 yr", "1--4 yrs", "5--9 yrs", "10plus")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("0", "1-4", "5-9", "10+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("infants", "one", "two", "three")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("0", "1", "2", "3")
    expect_identical(ans_obtained, ans_expected)
    x <- c("00", "01.04", "05.09", "10.14")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("0", "1-4", "5-9", "10-14")
    expect_identical(ans_obtained, ans_expected)
    x <- c("one month", "2 months", "zero months", "100 m and over")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("1month", "2months", "0months", "100m+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("11 qtrs", "five quarters or more", "0 qu", "100  quarter")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("11qtrs", "5quarters+", "0qu", "100quarter")
    expect_identical(ans_obtained, ans_expected)
    x <- "10 "
    ans_obtained <- translate_age_labels(x)
    ans_expected <- "10"
    expect_identical(ans_obtained, ans_expected)
    x <- c("0-0", "0_0")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("0", "0")
    expect_identical(ans_obtained, ans_expected)
    x <- c("100-", "100_", "100--", "100__")
    ans_obtained <- translate_age_labels(x)
    ans_expected <- c("100+", "100+", "100+", "100+")
    expect_identical(ans_obtained, ans_expected)
    x <- "In 1st year"
    ans_obtained <- translate_age_labels(x)
    ans_expected <- "0"
    expect_identical(ans_obtained, ans_expected)
    x <- "80 years or older"
    ans_obtained <- translate_age_labels(x)
    ans_expected <- "80+"
    expect_identical(ans_obtained, ans_expected)
    x <- "80 and older"
    ans_obtained <- translate_age_labels(x)
    ans_expected <- "80+"
    expect_identical(ans_obtained, ans_expected)
    x <- "80 or older"
    ans_obtained <- translate_age_labels(x)
    ans_expected <- "80+"
    expect_identical(ans_obtained, ans_expected)
})


    
