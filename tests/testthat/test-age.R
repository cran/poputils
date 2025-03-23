
## 'age_labels' ---------------------------------------------------------------

test_that("'age_labels' throws correct error when 'max' smaller than 'min'", {
    expect_error(age_labels(type = "five", min = 20L, max = 10L, open = FALSE),
                 "'max' \\[10\\] is less than 'min' \\[20\\]")
})

test_that("'age_labels' throws correct error when 'max' equals 'min' and 'open' is FALSE", {
    expect_error(age_labels(type = "single", min = 20, max = 20, open = FALSE),
                 "'max' \\[20\\] equals 'min' \\[20\\] but 'open' is FALSE")
})

test_that("'age_labels' throws correct error when 'type' invalid", {
    expect_error(age_labels(type = "wrong", min = 0, max = 20, open = FALSE))
})



## age_mid, age_lower, age_upper ----------------------------------------------

test_that("'age_lower' gives correct answer with valid inputs", {
    expect_identical(age_lower(c("0", "1-4", "5-9", "10+")),
                     c(0, 1, 5, 10))
})

test_that("'age_mid' gives correct answer with 5-year age groups", {
    expect_identical(age_mid(c("0-4", "5-9", "10+")),
                     c(2.5, 7.5, 12.5))
})

test_that("'age_mid' gives correct answer with 1-year age groups", {
    expect_identical(age_mid(c("0", "5", "10+")),
                     c(0.5, 5.5, 10.5))
})

test_that("'age_mid' gives correct answer with life table age groups", {
    expect_identical(age_mid(c("0", "1-4", "5-9", "10+")),
                     c(0.5, 3, 7.5, 12.5))
})

test_that("'age_mid' raises answer with all open", {
    expect_error(age_mid("100+"),
                 "unclear whether 'x' consists of 1-year, 5-year, or life-table age groups")
})

test_that("'age_mid' raises answer with all 0 and open", {
    expect_error(age_mid(c("0", "100+")),
                 "unclear whether 'x' consists of 1-year or life-table age groups")
})

test_that("'age_upper' gives correct answer with valid inputs", {
    expect_identical(age_upper(c("0", "1-4", "5-9", "10+")),
                     c(1, 5, 10, Inf))
})


## 'age_group_type' -----------------------------------------------------------------

test_that("'age_group_type' recognises 'single' type", {
    expect_identical(age_group_type(c("0", "100+", "33")),
                     "single")
    expect_identical(age_group_type(character()),
                     "single")
    expect_identical(age_group_type("0"),
                     "single")
    expect_identical(age_group_type("10+"),
                     "single")
    expect_identical(age_group_type(c("10+", NA)),
                     "single")
    expect_identical(age_group_type(NA_character_),
                     "single")
    expect_identical(age_group_type(c("0", "5+")),
                     "single")
})

test_that("'age_group_type' recognises 'five' type", {
    expect_identical(age_group_type(c("0-4", "100+", "30-34")),
                     "five")
    expect_identical(age_group_type("5-9"),
                     "five")
    expect_identical(age_group_type("0-4"),
                     "five")
    expect_identical(age_group_type(c("5-9", "10+")),
                     "five")
    expect_identical(age_group_type(c("10+", NA, "5-9")),
                     "five")
})

test_that("'age_group_type' recognises 'lt' type", {
    expect_identical(age_group_type(c("1-4", "100+", "30-34")),
                     "lt")
    expect_identical(age_group_type(c("1-4", NA, "100+")),
                     "lt")
    expect_identical(age_group_type(c("0", "5-9")),
                     "lt")
    expect_identical(age_group_type(c("10+", NA, "5-9", "0")),
                     "lt")
})


## check_age ------------------------------------------------------------------

test_that("'check_age' gives correct answer with all tests turned off", {
    expect_true(check_age(c("1", "10+")))
    expect_true(check_age(c("0-4", "10+", NA)))
    expect_true(check_age(c("1-4", "10+")))
    expect_true(check_age(character()))
})

test_that("'check_age' works - 'complete'", {
    expect_true(check_age(c("1-4", "5-9", "0"),
                          complete = TRUE))
    expect_error(check_age(c("0", "5-9"),
                           complete = TRUE),
                 "Age group \"1-4\" is missing.")
})

test_that("'check_age' works - 'unique'", {
    expect_true(check_age(c("0", "5-9", "1-4"),
                          unique = TRUE))
    expect_error(check_age(c("1-4", "0", "1-4", "5-9"),
                           unique = TRUE),
                 "Age group \"1-4\" is duplicated.")
})

test_that("'check_age' works - 'zero'", {
    expect_true(check_age(c("0", "5-9", "1-4"),
                          zero = TRUE))
    expect_error(check_age(c("1--4", "10--14", "5--9"),
                           zero = TRUE),
                 "Youngest age group does not start at 0.")
})

test_that("'check_age' works - 'open'", {
    expect_true(check_age(c("0", "5-9", "1-4", "10+"),
                          open = TRUE))
    expect_error(check_age(c("1--4", "10--14", "5--9"),
                           open = TRUE),
                 "Oldest age group is not open.")
})

test_that("'check_age' works - 'closed'", {
    expect_true(check_age(c("0", "5-9", "1-4", "10-14"),
                          closed = TRUE))
    expect_error(check_age(c("1--4", "10+", "5--9"),
                           closed = TRUE),
                 "Oldest age group is not closed.")
})



## combine_age ----------------------------------------------------------------

test_that("'combine_age' works with valid inputs - single to lt", {
    x <- c("0", "2", "12", "100+")
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- c("0", "1-4", "10-14", "100+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0", "2", NA, "12", "100+", NA)
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- c("0", "1-4", NA, "10-14", "100+", NA)
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("0", "2", NA, "12", "100+", NA),
                levels = c(age_labels("single"), NA),
                exclude = character())
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- factor(c("0", "1-4", NA, "10-14", "100+", NA),
                           levels = c(age_labels("lt"), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("1", "2", NA, "12", "100+", NA),
                levels = c(age_labels("single"), NA),
                exclude = character())
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- factor(c("1-4", "1-4", NA, "10-14", "100+", NA),
                           levels = c(age_labels("lt", min = 1, open = TRUE), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("5", "8", NA, "12", "100+", NA),
                levels = c(age_labels("single"), NA),
                exclude = character())
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- factor(c("5-9", "5-9", NA, "10-14", "100+", NA),
                           levels = c(age_labels("lt", min = 5, open = TRUE), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("5", "8", "80", "12", "100+"),
                levels = age_labels("single"),
                exclude = character())
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- factor(c("5-9", "5-9", "80-84", "10-14", "100+"),
                           levels = age_labels("lt", min = 5, open = TRUE),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- c(30, 35)
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- c("30-34", "35-39")
    expect_identical(ans_obtained, ans_expected)
    x <- "0"
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- "0"
    expect_identical(ans_obtained, ans_expected)
    x <- "3"
    ans_obtained <- combine_age(x, to = "lt")
    ans_expected <- "1-4"
    expect_identical(ans_obtained, ans_expected)
})

test_that("'combine_age' works with valid inputs - single to five", {
    x <- c("0", "2", "12", "100+")
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- c("0-4", "0-4", "10-14", "100+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0", "2", NA, "12", "100+", NA)
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- c("0-4", "0-4", NA, "10-14", "100+", NA)
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("0", "2", NA, "12", "100+", NA),
                levels = c(age_labels("single"), NA),
                exclude = character())
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- factor(c("0-4", "0-4", NA, "10-14", "100+", NA),
                           levels = c(age_labels("five"), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- c(30, 35)
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- c("30-34", "35-39")
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c(30, 35))
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- factor(c("30-34", "35-39"))
    expect_identical(ans_obtained, ans_expected)
    x <- 12:53
    ans_obtained <- combine_age(x, to = "five")
    ans_expected <- c(rep("10-14", times = 3),
                      rep(age_labels(type = "five", min = 15, max = 50), each = 5),
                      rep("50-54", times = 4))
    expect_identical(ans_obtained, ans_expected)                      
})

test_that("'combine_age' works with valid inputs - lt to five", {
    x <- c("0", "1-4", "10-14", "100+")
    ans_obtained <- combine_age(x)
    ans_expected <- c("0-4", "0-4", "10-14", "100+")
    expect_identical(ans_obtained, ans_expected)
    x <- c("0", "1-4", NA, "10-14", "100+", NA)
    ans_obtained <- combine_age(x)
    ans_expected <- c("0-4", "0-4", NA, "10-14", "100+", NA)
    expect_identical(ans_obtained, ans_expected)
    x <- factor(c("0", "1-4", NA, "10-14", "100+", NA),
                levels = c(age_labels("lt"), NA),
                exclude = character())
    ans_obtained <- combine_age(x)
    ans_expected <- factor(c("0-4", "0-4", NA, "10-14", "100+", NA),
                           levels = c(age_labels("five"), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    x <- "0"
    ans_obtained <- combine_age(x)
    ans_expected <- "0-4"
    x <- factor(c("1-4", "0"))
    ans_obtained <- combine_age(x)
    ans_expected <- factor(c("0-4", "0-4"))
    x <- factor(c("1-4", "5-9", "0"))
    ans_obtained <- combine_age(x)
    ans_expected <- factor(c("0-4", "5-9", "0-4"))
})

test_that("'combine_age' works with valid degenerate cases", {
    x <- c(NA, NA)
    expect_identical(combine_age(x), x)
    x <- c(NA, "1+")
    expect_identical(combine_age(x, to = "lt"), x)
    x <- c(NA, NA)
    expect_identical(combine_age(x), x)
    x <- c(NA, "5+")
    expect_identical(combine_age(x), x)
    x <- factor()
    expect_identical(combine_age(x), x)
    x <- "0-4"
    expect_identical(combine_age(x), x)
    x <- "1-4"
    expect_identical(combine_age(x, to = "lt"), x)
})

test_that("'combine_age' raises correct error with invalid degenerate cases", {
    x <- "1+"
    expect_error(combine_age(x),
                 "cannot convert to 5-year age groups : open age group starts at 1")
    x <- "6+"
    expect_error(combine_age(x, to = "lt"),
                 "cannot convert to life table age groups : open age group starts at 6")
})

test_that("'combine_age' raises correct when trying to convert from 5-year age groups", {
    x <- "5-9"
    expect_error(combine_age(x, to = "lt"),
                 "cannot convert 5-year age groups to life table age groups")
})    


## reformat_age ------------------------------------------------------------------

test_that("'reformat_age' gives correct answer with five-year age groups in order - factor", {
    expect_identical(reformat_age(c("0-4", "5-9", "10+")),
                     factor(c("0-4", "5-9", "10+"),
                            levels = c("0-4", "5-9", "10+")))
})

test_that("'reformat_age' gives correct answer with five-year age groups in order - non-factor", {
    expect_identical(reformat_age(c("0-4", "5-9", "10+"), factor = FALSE),
                     c("0-4", "5-9", "10+"))
})

test_that("'reformat_age' gives correct answer with five-year age groups with gap - factor", {
    expect_identical(reformat_age(c("0-4", "10+")),
                     factor(c("0-4", "10+"),
                            levels = c("0-4", "5-9", "10+")))
})

test_that("'reformat_age' gives correct answer with five-year age groups with NA - factor", {
    expect_identical(reformat_age(c("0-4", NA, "5-9", "10+")),
                     factor(c("0-4", NA, "5-9", "10+"),
                            levels = c("0-4", "5-9", "10+", NA),
                            exclude = character()))
})

test_that("'reformat_age' gives correct answer with five-year age groups with NA - non-factor", {
    expect_identical(reformat_age(c("0-4", NA, "5-9", "10+"), factor = FALSE),
                     c("0-4", NA, "5-9", "10+"))
})

test_that("'reformat_age' gives correct answer with five-year age groups needing translation - factor", {
    expect_identical(reformat_age(c("0--4", "5 to 9", "10 or more")),
                     factor(c("0-4", "5-9", "10+"),
                            levels = c("0-4", "5-9", "10+")))
})

test_that("'reformat_age' gives correct answer with infants", {
    expect_identical(reformat_age("Under 1 year"),
                     factor("0"))
})


test_that("'reformat_age' gives correct answer with life table age groups - factor", {
    expect_identical(reformat_age(c("1-4", "0", "5-9", "10+")),
                     factor(c("1-4", "0", "5-9", "10+"),
                            levels = c("0", "1-4", "5-9", "10+")))
})

test_that("'reformat_age' gives correct answer with life table age groups in order - non-factor", {
    expect_identical(reformat_age(c("0", "5-9", "10+"), factor = FALSE),
                     c("0", "5-9", "10+"))
})

test_that("'reformat_age' gives correct answer with life table age groups with gap - factor", {
    expect_identical(reformat_age(c("1-4", "10+")),
                     factor(c("1-4", "10+"),
                            levels = c("1-4", "5-9", "10+")))
})

test_that("'reformat_age' gives correct answer with life table age groups with NA - factor", {
    expect_identical(reformat_age(c("0", NA, "5-9", "10+")),
                     factor(c("0", NA, "5-9", "10+"),
                            levels = c("0", "1-4", "5-9", "10+", NA),
                            exclude = character()))
})

test_that("'reformat_age' gives correct answer with life table age groups with NA - non-factor", {
    expect_identical(reformat_age(c("0", NA, "5-9", "10+"), factor = FALSE),
                     c("0", NA, "5-9", "10+"))
})

test_that("'reformat_age' gives correct answer with life table age groups needing translation - factor", {
    expect_identical(reformat_age(c("zero", "1--4", "5 to 9", NA, "10 or more")),
                     factor(c("0", "1-4", "5-9", NA, "10+"),
                            levels = c("0", "1-4", "5-9", "10+", NA),
                            exclude = character()))
})

test_that("'reformat_age' gives correct answer with zero-length input - factor", {
    expect_identical(reformat_age(character()),
                     factor(character()))
    expect_identical(reformat_age(integer()),
                     factor(character()))
    expect_identical(reformat_age(logical()),
                     factor(character()))
})

test_that("'reformat_age' gives correct answer with zero-length input - non-factor", {
    expect_identical(reformat_age(character(), factor = FALSE),
                     character())
    expect_identical(reformat_age(integer(), factor = FALSE),
                     character())
})

test_that("'reformat_age' gives correct answer with all-NA input - factor", {
    expect_identical(reformat_age(as.character(c(NA, NA))),
                     factor(as.character(c(NA, NA)),
                            exclude = character()))
})

test_that("'reformat_age' gives correct answer with all-NA input - non-factor", {
    expect_identical(reformat_age(as.character(c(NA, NA)), factor = FALSE),
                     as.character(c(NA, NA)))
})

test_that("'reformat_age' throws correct error with non-vector x", {
    expect_error(reformat_age(NULL),
                 "`x` is not a vector or factor.")
})

test_that("'reformat_age' throws correct error with overlapping open age groups", {
    expect_error(reformat_age(c("1-4", "5+", "10+")),
                 "Open age groups have different lower limits.")
})

test_that("'reformat_age' throws correct error with overlapping age groups", {
    expect_error(reformat_age(c("5+", "0-4", "10-14")),
                 "Age groups \"5\\+\" and \"10-14\" overlap.")
})


## set_age_open ---------------------------------------------------------------

test_that("'set_age_open' works with valid inputs - single, non-factor", {
    expect_identical(set_age_open(c("0", "11", "100+", "88", NA), 20),
                     c("0", "11", "20+", "20+", NA))
    expect_identical(set_age_open(character(), 20),
                     character())
})

test_that("'set_age_open' works with valid inputs - five, non-factor", {
    expect_identical(set_age_open(c("0-4", "10-14", "100+", "85-89", NA), 20),
                     c("0-4", "10-14", "20+", "20+", NA))
    expect_identical(set_age_open(NA, 20),
                     NA_character_)
})

test_that("'set_age_open' works with valid inputs - lt, non-factor", {
    expect_identical(set_age_open(c("0", "10-14", "100+", "85-89", NA), 20),
                     c("0", "10-14", "20+", "20+", NA))
    expect_identical(set_age_open(c("0", "10-14", "100+", "85-89", NA), 0),
                     c(rep("0+", 4), NA))
})

test_that("'set_age_open' works with valid inputs - single, factor", {
    x <- factor(c("0", "11", "100+", "88", NA),
                levels = c(0:99, "100+", NA),
                exclude = character())
    ans_obtained <- set_age_open(x, 20)
    ans_expected <- factor(c("0", "11", "20+", "20+", NA),
                           levels = c(0:19, "20+", NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    expect_identical(set_age_open(factor(), 20),
                     factor())
})

test_that("'set_age_open' works with valid inputs - five, factor", {
    x <- factor(c("0-4", "10-14", "100+", "85-89", NA),
                levels = c(age_labels(type = "five"), NA),
                exclude = character())
    ans_obtained <- set_age_open(x, 20)
    ans_expected <- factor(c("0-4", "10-14", "20+", "20+", NA),
                           levels = c(age_labels(type = "five", max = 20), NA),
                           exclude = character())
    expect_identical(ans_obtained, ans_expected)
    expect_identical(set_age_open(factor(NA), 20),
                     factor(NA))
})

test_that("'set_age_open' gives expected error when 'lower' too high", {
    x <- c("0-4", "10-14", "100+", "85-89", NA)
    expect_error(set_age_open(x, lower = 120),
                 "'lower' \\[120\\] is greater than current lower limit for open age group \\[100\\]")
})

test_that("'set_age_open' gives expected error when not vector or factor", {
    expect_error(set_age_open(NULL, lower = 17),
                 "`x` is not a vector or a factor.")
})

test_that("'set_age_open' gives expected error when new age groups are invalid", {
    x <- c("0-4", "10-14", "100+", "85-89", NA)
    expect_error(set_age_open(x, lower = 17),
                 "new age groups are invalid")
})


    
