
## 'to_matrix' ----------------------------------------------------------------

test_that("'to_matrix' works with valid inputs", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    ans <- to_matrix(df, rows = a, cols = c(c, b), measure = z)
    expect_true(is.matrix(ans))
    ans_quoted <- to_matrix(df, measure = z, rows = "a", cols = c("c", "b"))
    expect_identical(ans, ans_quoted)
    ans_t <- to_matrix(df, measure = z, rows = c(c, b), cols = a)
    expect_identical(ans, t(ans_t))
})

test_that("'to_matrix' works with missing levels inputs", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df <- df[-(58:60), ]
    df$a <- factor(df$a, levels = letters[1:4])
    df$z <- seq_len(nrow(df))
    ans <- to_matrix(df, rows = a, cols = c(c, b), measure = z)
    expect_identical(nrow(ans), 3L)
    expect_identical(ncol(ans), nrow(unique(df[c("b", "c")])))
    expect_identical(sum(is.na(ans)), 0L)
    expect_true(is.matrix(ans))
    ans_quoted <- to_matrix(df, measure = z, rows = "a", cols = c("c", "b"))
    expect_identical(ans, ans_quoted)
    ans_t <- to_matrix(df, measure = z, rows = c(c, b), cols = a)
    expect_identical(ans, t(ans_t))
})

test_that("'to_matrix' raises correct error with non-data frame", {
    expect_error(to_matrix(NULL, rows = a, cols = c(c, b)),
                 "`x` is not a data frame")
})

test_that("'to_matrix' raises correct error with too few columns", {
    expect_error(to_matrix(data.frame(x = 1:3), rows = a, cols = c(c, b)),
                 "`x` has 1 column")
    expect_error(to_matrix(data.frame(x = 1:3, y = 3:1), rows = a, cols = c(c, b)),
                 "`x` has 2 columns")
})

test_that("'to_matrix' raises correct error with no measure var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b)),
                 "No measure variable supplied.")
})

test_that("'to_matrix' raises correct error with two measure vars", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    df$z2 <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = c(z, z2)),
                 "Attempt to select 2 measure variables.")
})

test_that("'to_matrix' raises correct error with no rows var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, cols = c(c, b), measure = z),
                 "No value supplied for `rows`.")
})

test_that("'to_matrix' raises correct error with measure and rows overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = a),
                 "Same variable selected by `measure` and `rows`: \"a\".")
})

test_that("'to_matrix' raises correct error with no cols var", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = c(c, b), measure = z),
                 "No value supplied for `cols`.")
})

test_that("'to_matrix' raises correct error with measure and cols overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b), measure = c),
                 "Same variable selected by `measure` and `cols`: \"c\".")
})

test_that("'to_matrix' raises correct error when rows and cols overlap", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    expect_error(to_matrix(df, rows = a, cols = c(c, b, a), measure = z),
                 "Same variable selected by `rows` and `cols`: \"a\".")
})

test_that("'to_matrix' raises correct error with duplicate classif vars", {
    df <- expand.grid(a = letters[1:3],
                      b = LETTERS[1:4],
                      c = letters[1:5],
                      KEEP.OUT.ATTRS = FALSE)
    df$z <- seq_len(nrow(df))
    df <- rbind(df, df[1, ])
    expect_error(to_matrix(df, rows = a, cols = c(b, c), measure = z),
                 "`x` has two rows with values a=\"a\", b=\"A\", c=\"a\"\\.")
})




