
## HAS_TESTS
#' Check that 'at' Picks Out the Lower Limit of Age Group in 'age'
#'
#' @param at An integer scalar
#' @param age A vector of age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_at_in_age <- function(at, age) {
  lower <- age_lower(age)
  if (!(at %in% lower))
    cli::cli_abort(c("Invalid value for {.arg at}.",
                     i = "{.arg at} must equal the lower limit of an age group in {.arg age}.",
                     i = "{.arg at}: {.val {at}}.",
                     i = "Age groups in {.arg age}: {.val {age}}.",
                     i = "Lower limits of age groups: {.val {lower}}."))
  invisible(TRUE)
}


## HAS_TESTS
#' Check that colnums vectors, as produced by
#' tidyselect::eval_select(), each point
#' to 0 or 1 columns
#'
#' @param Named list of named integer vectors
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_at_most_one_colnum <- function(x) {
    check_valid_colnum_list(x)
    nms <- names(x)
    for (i in seq_along(x)) {
        n_col <- length(x[[i]])
        if (n_col > 1L) {
            nm_arg <- nms[[i]]
            nms_cols <- names(x[[i]])
            cli::cli_abort(c("{n_col} variables specified for {.arg {nm_arg}}.",
                             i = "{.arg {nm_arg}} should be a single variable."))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check That 'ax' is Valid
#'
#' Check that 'ax' is numeric, non-negative, less than
#' corresponding 'nx'. NAs are allowed.
#' Cannot be an rvec.
#'
#' @param ax Numeric vector
#' @param ax Character vector of age group labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_ax <- function(ax, age) {
  check_not_rvec(x = ax, nm_x = "ax")
  if (!is.numeric(ax))
    cli::cli_abort(c("{.arg ax} is non-numeric.",
                     i = "{.arg ax} has class {.cls {class(ax)}}."))
  is_neg <- !is.na(ax) & ax < 0
  i_neg <- match(TRUE, is_neg, nomatch = 0L)
  if (i_neg > 0L) {
    age_neg <- age[[i_neg]]
    ax_neg <- ax[[i_neg]]
    cli::cli_abort(c("{.arg ax} has negative value.",
                     i = "Value of {.arg ax} for age group {.val {age_neg}} is {.val {ax_neg}}."))
  }
  check_equal_length(x = ax,
                     y = age,
                     nm_x = "ax",
                     nm_y = "age")
  age_group_categ <- age_group_categ(age)
  is_ax_le_nx <- is_ax_le_nx(ax, age_group_categ)
  i_gt <- match(FALSE, is_ax_le_nx, nomatch = 0L)
  if (i_gt > 0L) {
    age_gt <- age[[i_gt]]
    ax_gt <- ax[[i_gt]]
    width <- age_upper(age_gt) - age_lower(age_gt)
    cli::cli_abort(c("{.arg ax} larger than width of corresponding age group.",
                     i = "Value of {.arg ax} for age group {.val {age_gt}} is {.val {ax_gt}}.",
                     i = "Age group {.val {age_gt}} has width {.val {width}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check for duplicated age labels
#'
#' Check to see if age labels are repeated,
#' with each label repeated the same number
#' of times - which suggests that the 'by'
#' variables are incomplete
#'
#' @param age Character vector of age labels
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_duplicated_age <- function(age) {
    tab <- table(age)
    n_tab <- length(tab)
    is_all_length_1 <- all(tab == 1L)
    is_varying_counts <- (n_tab > 1L) && (any(tab[-1L] != tab[[1L]]))
    if (is_all_length_1 || is_varying_counts)
        invisible(TRUE)
    else
        cli::cli_abort(c("Age labels duplicated.",
                         i = "Do you need to modify `sex` or `by`?"))
}


## HAS_TESTS
## Might export this in future
#' Check that Selected Rows within Data Frame
#' do not have Duplicates
#'
#' Check that values in a subset of columns of
#' data frame `x` are unique. Can be used as
#' a validity check for cross-classifying
#' variables. 
#'
#' @param x A data frame.
#' @param nm_x The name for `x` to be used in
#' error messages.
#' @param nms_cols A character vector giving the
#' names of the variables within `x` to be checked.
#'
#' @returns `TRUE`, invisibly.
#'
#' @examples
#' x <- data.frame(a = 1:2, b = c("a", "a"), x = c(0.2, -0.1))
#' check_duplicated_rows(x = x,
#'                       nm_x = "x",
#'                       nms_cols = c("a", "b"))
#' @noRd
check_duplicated_rows <- function(x, nm_x, nms_cols) {
    vals_cols <- x[nms_cols]
    is_dup <- duplicated(vals_cols)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L) {
        str_key <- make_str_key(vals_cols[i_dup, nms_cols, drop = FALSE])
        n <- length(nms_cols)
        cli::cli_abort(c(paste("{.arg {nm_x}} has two rows with same {cli::qty(n)} value{?s}",
                               "for {.var {nms_cols}}."),
                         i = paste("Duplicated {cli::qty(n)} value{?s}:", str_key)))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check that Arguments have Same Length
#'
#' Check that `x` and `y` have the same length.
#'
#' @param x,y Arguments to compare
#' @param nm_x,nm_y Names to use in error message
#'
#' @returns 'TRUE', invisibly.
#'
#' @examples
#' x <- 1:3
#' y <- 3:1
#' check_equal_length(x = x,
#'                    y = y,
#'                    nm_x = "x",
#'                    nm_y = "y")
#' @export
check_equal_length <- function(x, y, nm_x, nm_y) {
    n_x <- length(x)
    n_y <- length(y)
    if (n_x != n_y)
        cli::cli_abort(c("{.arg {nm_x}} and {.arg {nm_y}} have different lengths.",
                         i = "Length of {.arg {nm_x}}: {.val {n_x}}.",
                         i = "Length of {.arg {nm_y}}: {.val {n_y}}."))
    invisible(TRUE)
}
    

## HAS_TESTS
#' Check a logical flag
#'
#' @param x TRUE or FALSE
#'
#' @returns TRUE, invisibly
#' 
#' @noRd
check_flag <- function(x) {
    nm <- deparse1(substitute(x))
    if (!identical(length(x), 1L))
        cli::cli_abort(c("{.arg {nm}} does not have length 1",
                         i = "{.arg {nm}} has length {length(x)}."))
    if (!is.logical(x))
        cli::cli_abort(c("{.arg {nm}} does not have class {.cls logical}.",
                         i = "{.arg {nm}} has class {.cls {class(x)}}"))
    if (is.na(x))
        cli::cli_abort("{.arg {nm}} is {.val {NA}}")
    invisible(TRUE)
}


## HAS_TESTS
#' Check that no columns of 'data' are used more than once,
#' and that required arguments are supplied
#'
#' @param mx_colnum Named integer vector
#' @param qx_colnum Named integer vector
#' @param age_colnum Named integer vector
#' @param sex_colnum Named integer vector
#' @param ax_colnum Named integer vector
#' @param by_colnums Named integer vector
#' @param groups_colnums Named integer vector
#'
#' @returns TRUE, invisibly
#'
#' @noRd        
check_life_colnums <- function(mx_colnum,
                               qx_colnum,
                               age_colnum,
                               sex_colnum,
                               ax_colnum,
                               by_colnums,
                               groups_colnums) {
    has_mx <- length(mx_colnum) > 0L
    has_qx <- length(qx_colnum) > 0L
    has_age <- length(age_colnum) > 0L
    has_sex <- length(sex_colnum) > 0L
    has_by <- length(by_colnums) > 0L
    has_groups <- length(groups_colnums) > 0L
    if (!has_mx && !has_qx)
        cli::cli_abort("No value supplied for {.arg mx} or for {.arg qx}.")
    if (has_mx && has_qx)
        cli::cli_abort("Values supplied for {.arg mx} and for {.arg qx}.")
    if (!has_age)
        cli::cli_abort("No value supplied for {.arg age}.")
    if (has_by && has_groups)
        cli::cli_abort("Can't supply {.arg by} when {.arg data} is a grouped data
  frame.")
    at_most_one <- list(mx = mx_colnum,
                        qx = qx_colnum,
                        age = age_colnum,
                        sex = sex_colnum,
                        ax = ax_colnum)
    check_at_most_one_colnum(at_most_one)
    no_overlap <- list(mx = mx_colnum,
                       qx = qx_colnum,
                       age = age_colnum,
                       sex = sex_colnum,
                       ax = ax_colnum)
    check_no_overlap_colnums(no_overlap)
    invisible(TRUE)
}


## HAS_TESTS
#' Check Life Table lx Column
#'
#' Assume that age groups are in correct order.
#' Standard numeric checks, plus check for
#' non-increasing, and non-zero radix.
#'
#' Assume age has already been checked.
#'
#' @param lx Life table column
#' @param age Age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_lx <- function(lx, age) {
    check_numeric(x = lx,
                  x_arg = "lx",
                  check_na = TRUE,
                  check_positive = FALSE,
                  check_nonneg = TRUE,
                  check_whole = FALSE)
    if (length(lx) < 2L)
        cli::cli_abort("{.arg lx} has length {length(lx)}.")
    if (isTRUE(all.equal(lx[[1L]], 0)))
        cli::cli_abort("First element of {.arg lx} is 0.")
    is_incr <- diff(lx) > 0
    i_incr <- match(TRUE, is_incr, nomatch = 0L)
    if (i_incr > 0) {
        age1 <- age[[i_incr + 1L]]
        age0 <- age[[i_incr]]
        cli::cli_abort(c(paste("{.arg lx} for age {.val {age1}} greater than {.arg lx} for",
                               "age {.val {age0}}."),
                         i = "{.arg lx} for age {.val {age1}}: {.val {lx[[i_incr + 1L]]}}.",
                         i = "{.arg lx} for age {.val {age0}}: {.val {lx[[i_incr]]}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check That a Vector (Possibly Rvec) orfRates is Valid
#'
#' Check that is rvec or double and
#' all non-negative. NAs are allowed.
#'
#' @param mx An rvec_dbl or double.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_mx <- function(mx) {
  if (rvec::is_rvec(mx))
    is_numeric <- is.numeric(as.matrix(mx))
  else
    is_numeric <- is.numeric(mx)
  if (!is_numeric)
    cli::cli_abort(c("{.arg mx} is non-numeric.",
                     i = "{.arg mx} has class {.cls {class(mx)}}."))
  if (rvec::is_rvec(mx))
    mx <- as.numeric(mx)
  n_neg <- sum(mx < 0, na.rm = TRUE)
  if (n_neg > 0L)
    cli::cli_abort("{.arg mx} has negative {cli::qty(n_neg)} value{?s}.")
  invisible(TRUE)
}


## HAS_TESTS
#' Check Whole Number
#'
#' Check that `n` is  finite, non-NA scalar that
#' is an integer or integerish (ie is equal to `round(n)`),
#' and optionally within a specified range
#' and divisible by a specified number.
#'
#' @param n A whole number
#' @param nm_n Name for 'n' to be used in error messages
#' @param min Minimum value 'n' can take. Can be NULL.
#' @param max Maximum values 'n' can take. Can be NULL.
#' @param divisible_by 'n' must be divisible by this. Can be NULL.
#'
#' @returns
#' If all tests pass, `check_n()` returns `TRUE` invisibly.
#' Otherwise it throws an error.
#'
#' @examples
#' check_n(10, nm_n = "count", min = 0, max = NULL, divisible_by = 1)
#' check_n(10, nm_n = "count", min = NULL, max = NULL, divisible_by = NULL)
#' check_n(10, nm_n = "n", min = 5, max = 10, divisible_by = 2)
#' @export
check_n <- function(n, nm_n, min, max, divisible_by) {
  if (rvec::is_rvec(n))
    cli::cli_abort("{.arg {nm_n}} is an rvec.")
  if (!is.numeric(n))
    cli::cli_abort(c("{.arg {nm_n}} is non-numeric.",
                     i = "{.arg {nm_n}} has class {.cls {class(n)}}."))
  if (length(n) != 1L)
    cli::cli_abort(c("{.arg {nm_n}} does not have length 1.",
                     i = "{.arg {nm_n}} has length {length(n)}."))
  if (is.na(n))
    cli::cli_abort("{.arg {nm_n}} is {.val {NA}}.")
  if (is.infinite(n))
    cli::cli_abort("{.arg {nm_n}} is {.val {Inf}}.")
  if (!isTRUE(all.equal(round(n), n)))
    cli::cli_abort(c("{.arg {nm_n}} is not an integer.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (!is.null(min) && (n < min))
    cli::cli_abort(c("{.arg {nm_n}} is less than {min}.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (!is.null(max) && (n > max))
    cli::cli_abort(c("{.arg {nm_n}} is greater than {max}.",
                     i = "{.arg {nm_n}} is {.val {n}}."))
  if (!is.null(divisible_by) && (n %% divisible_by != 0L))
    cli::cli_abort(c("{.arg {nm_n}} is not divisible by {divisible_by}.",
                     i = "{.arg nm_n}: {.val {n}}."))
  invisible(TRUE)
}




## HAS_TESTS
#' Check that Colnum Vectors do not Overlap
#'
#' Given a named list of colnum vectors, like those
#' produced by [tidyselect::eval_select()],
#' throw an error if there is an overlap.
#'
#' @param x A named list of integer vectors.
#'
#' @return `TRUE`, invisibly
#'
#' @seealso [tidyselect::eval_select()]
#'
#' @examples
#' x <- list(arg1 = c(age = 1L),
#'           arg2 = c(gender = 4L, region = 5L))
#' check_no_overlap_colnums(x)
#' @export
check_no_overlap_colnums <- function(x) {
    check_valid_colnum_list(x)
    n <- length(x)
    if (n >= 2L) {
        i_pairs <- utils::combn(x = n, m = 2L, simplify = FALSE)
        for (i_pair in i_pairs)
            check_no_overlap_colnums_pair(pair = x[i_pair])
    }
    invisible(TRUE)
}

## HAS_TESTS
#' Check that a pair of colnum vectors do not overlap
#'
#' Check that a pair of colnum vectors, with the format
#' produced by tidyselect::eval_select(), do not have
#' any elements in common.
#'
#' @param pair A named list of two colnum vectors.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_no_overlap_colnums_pair <- function(pair) {
    intersection <- vctrs::vec_set_intersect(x = pair[[1L]],
                                             y = pair[[2L]])
    n <- length(intersection)
    if (n > 0L) {
        nms <- names(pair)
        nm1 <- nms[[1L]]
        nm2 <- nms[[2L]]
        cli::cli_abort(c("{.arg {nm1}} and {.arg {nm2}} use the same {cli::qty(n)} variable{?s}.",
                         i = "{.arg {nm1}}: {.val {names(pair[[1L]])}}.",
                         i = "{.arg {nm2}}: {.val {names(pair[[2L]])}}.",
                         i = "Overlap: {.val {names(intersection)}}."))
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Check That Not An Rvec
#'
#' @param x A vector
#'
#' @returns TRUE, ivisibly
#'
#' @noRd
check_not_rvec <- function(x, nm_x) {
  if (rvec::is_rvec(x))
    cli::cli_abort("{.arg {nm_x}} is an rvec.")
  invisible(TRUE)
}
        

## HAS_TESTS
#' Check a number
#'
#' Check that `x` is valid integer or double scalar or rvec scalar
#'
#' @param x A numeric scalar
#' @param x_arg Name for `x` to be
#' used in error messages.
#' @param check_na Whether 'x' must not be NA
#' @param check_positive Whether 'x' must be positive.
#' @param check_nonneg Whether 'x' must be non-negative.
#' @param check_whole Whether 'x' must be a whole number.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_number <- function(x,
                         x_arg,
                         check_na,
                         check_positive,
                         check_nonneg,
                         check_whole) {
  if (rvec::is_rvec(x))
    is_numeric <- is.numeric(as.matrix(x))
  else
    is_numeric <- is.numeric(x)
  if (!is_numeric)
    cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                     i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
  if (length(x) != 1L)
    cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                     i = "{.arg {x_arg}} has length {.val {length(x)}}."))
  if (rvec::is_rvec(x)) {
    x <- as.numeric(x)
    return(check_numeric(x = x,
                         x_arg = x_arg,
                         check_na = check_na,
                         check_positive = check_positive,
                         check_nonneg = check_nonneg,
                         check_whole = check_whole))
  }
  if (check_na) {
    if (is.na(x))
      cli::cli_abort("{.arg {x_arg}} is {.val {NA}}.")
  }
  if (is.infinite(x))
    cli::cli_abort("{.arg {x_arg}} is infinite.")
  if (check_positive && !is.na(x)) {
    if (x <= 0)
      cli::cli_abort(c("{.arg {x_arg}} is non-positive.",
                       i = "{.arg {x_arg}} equals {.val {x}}."))
  }
  if (check_nonneg && !is.na(x)) {
    if (x < 0)
      cli::cli_abort(c("{.arg {x_arg}} is negative.",
                       i = "{.arg {x_arg}} equals {.val {x}}."))
  }
  if (check_whole && !is.na(x)) {
    if (x != round(x))
      cli::cli_abort(c("{.arg {x_arg}} is not a whole number.",
                       i = "{.arg {x_arg}} is {.val {x}}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check a 'Numeric' Vector (Including Rvec)
#'
#' Check that `x` is valid integer or double vector.
#'
#' Works with rvec.
#'
#' @param x A numeric vector
#' @param x_arg Name for `x` to be
#' used in error messages.
#' @param check_na Whether 'x' must not include NA
#' @param check_positive Whether all elements of  'x' must be positive.
#' @param check_nonneg Whether all elements of 'x' must be non-negative.
#' @param check_whole Whether all elements of 'x' must be whole numbers.
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_numeric <- function(x,
                          x_arg,
                          check_na,
                          check_positive,
                          check_nonneg,
                          check_whole) {
  if (rvec::is_rvec(x))
    is_numeric <- is.numeric(as.matrix(x))
  else
    is_numeric <- is.numeric(x)
  if (!is_numeric)
    cli::cli_abort(c("{.arg {x_arg}} is non-numeric.",
                     i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
  x <- as.numeric(x)
  if (check_na) {
    n_na <- sum(is.na(x))
    if (n_na > 0L)
      cli::cli_abort("{.arg {x_arg}} has {cli::qty(n_na)} NA{?s}.")
  }
  n_inf <- sum(is.infinite(x))
  if (n_inf > 0L)
    cli::cli_abort("{.arg {x_arg}} has non-finite {cli::qty(n_inf)} value{?s}.")
  if (check_positive) {
    n_nonpos <- sum(x <= 0, na.rm = TRUE)
    if (n_nonpos > 0L)
      cli::cli_abort("{.arg {x_arg}} has non-positive {cli::qty(n_nonpos)} value{?s}.")
  }
  if (check_nonneg) {
    n_neg <- sum(x < 0, na.rm = TRUE)
    if (n_neg > 0L)
      cli::cli_abort("{.arg {x_arg}} has negative {cli::qty(n_neg)} value{?s}.")
  }
  if (check_whole) {
    n_frac <- sum(x != round(x), na.rm = TRUE)
    if (n_frac > 0L)
      cli::cli_abort(paste("{.arg {x_arg}} has {cli::qty(n_frac)} value{?s}",
                           "that {?is/are} not whole number{?s}."))
  }
  invisible(TRUE)
}


## HAS_TESTS
#' Check that Vector of Probalities (Possibly Rvec) is Valid
#'
#' Check that rvec or double and
#' all in [0, 1]. NAs are allowed.
#'
#' @param qx An rvec or numeric vector.
#' @param nm_qx Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_qx <- function(qx, nm_qx) {
  if (rvec::is_rvec(qx))
    is_numeric <- is.numeric(as.matrix(qx))
  else
    is_numeric <- is.numeric(qx)
  if (!is_numeric)
    cli::cli_abort(c("{.arg {nm_qx}} is non-numeric.",
                     i = "{.arg {nm_qx}} has class {.cls {class(qx)}}."))
  if (rvec::is_rvec(qx))
    qx <- as.numeric(qx)
  n_neg <- sum(qx < 0, na.rm = TRUE)
  if (n_neg > 0L)
    cli::cli_abort("{.arg {nm_qx}} has negative {cli::qty(n_neg)} value{?s}.")
  n_high <- sum(qx > 1, na.rm = TRUE)
  if (n_high > 0L)
    cli::cli_abort("{.arg {nm_qx}} has {cli::qty(n_high)} value{?s} greater than 1.")
  invisible(TRUE)
}


## HAS_TESTS
#' Given that specified methods do not need
#' a sex variable to be supplied
#'
#' @param methods
#'
#' @return TRUE, invisibly
#'
#' @noRd
check_sex_not_needed <- function(methods) {
    methods_need_sex <- get_methods_need_sex()
    nms_arg <- names(methods)
    for (i in seq_along(methods)) {
        method <- methods[[i]]
        if (method %in% methods_need_sex) {
            nm_arg <- nms_arg[[i]]
            cli::cli_abort("{.arg {nm_arg}} is {.val {method}} but no value supplied for {.arg sex}.")
        }
    }
    invisible(TRUE)
}


#' Check 'standard'
#'
#' Check that 'standard' argument to 'ex_to_lifetab_brass()' is valid
#'
#' @param standard A data frame with columns "age" and "lx",
#' and, optionally, "ax", and optionally classification
#' variables.
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_standard <- function(standard) {
    nms_required <- c("age", "lx")
    nms_invalid <- "ex"
    if (!is.data.frame(standard))
        cli::cli_abort(c("{.arg standard} is not a data frame.",
                         i = "{.arg standard} has class {.cls {class(standard)}}."))
    nms_standard <- names(standard)
    for (nm in nms_invalid) {
        if (nm %in% nms_standard)
            cli::cli_abort(c("{.arg standard} has a variable called {.var {nm}}.",
                             i = "Only {.arg target} can have a variable called {.var {nm}}."))
    }
    for (nm in nms_required) {
        if (!(nm %in% nms_standard))
            cli::cli_abort(c("{.arg standard} does not have a variable called {.var {nm}}.",
                             i = "Variables in {.arg standard}: {.val {nms_standard}}."))
    }
    lx <- standard[["lx"]]
    if (rvec::is_rvec(lx))
        cli::cli_abort("{.var lx} variable in {.arg standard} is an rvec.")
    has_ax <- "ax" %in% nms_standard
    nms_vals <- nms_required
    if (has_ax)
        nms_vals <- c(nms_vals, "ax")
    nms_by <- setdiff(nms_standard, nms_vals)
    nms_cols <- c(nms_by, "age")
    check_duplicated_rows(x = standard,
                          nm_x = "standard",
                          nms_cols = nms_cols)
    split <- vctrs::vec_split(standard[nms_vals], standard[nms_by])
    n <- length(split$val)
    has_by <- length(nms_by) > 0L
    for (i in seq_len(n)) {
        val_i <- split$val[[i]]
        age_i <- val_i[["age"]]
        lx_i <- val_i[["lx"]]
        if (has_ax)
            ax_i <- val_i[["ax"]]
        if (has_by) {
            by_i <- split$key[i, , drop = FALSE]
            str_key <- make_str_key(by_i)
            str_key <- paste0(" for ", str_key, ".")
        }
        else
            str_key <- "."
        ## check 'age'
        return_val <- tryCatch(check_age(age_i), error = function(e) e)
        if (!isTRUE(return_val)) {
            cli::cli_abort(c(paste0("Problem with {.var age} values", str_key),
                             i = return_val$message,
                             return_val$body))
        }
        ord <- order(age_lower(age_i))
        age_i <- age_i[ord]
        ## check 'lx'
        lx_i <- lx_i[ord]
        return_val <- tryCatch(check_lx(lx = lx_i, age = age_i), error = function(e) e)
        if (!isTRUE(return_val)) {
            cli::cli_abort(c(paste0("Problem with {.var lx} values", str_key),
                             i = return_val$message,
                             return_val$body))
        }
        ## check 'ax'
        if (has_ax) {
            ax_i <- ax_i[ord]
            return_val <- tryCatch(check_ax(ax = ax_i, age = age_i), error = function(e) e)
            if (!isTRUE(return_val)) {
                cli::cli_abort(c(paste0("Problem with {.var ax} values", str_key),
                                 i = return_val$message,
                                 return_val$body))
            }
        }
    }
    invisible(TRUE)
}



## HAS_TESTS
#' Check that input is character, of length 1,
#' non-NA, at least one char,
#' with no blanks
#'
#' @param x A string
#' @param x_arg Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_string <- function(x, x_arg) {
    if (!is.character(x))
        cli::cli_abort(c("{.arg {x_arg}} is non-character.",
                         i = "{.arg {x_arg}} has class {.cls {class(x)}}."))
    n <- length(x)
    if (n != 1L)
        cli::cli_abort(c("{.arg {x_arg}} does not have length 1.",
                         i = "{.arg {x_arg}} has length {n}."))
    if (is.na(x))
        cli::cli_abort("{.arg {x_arg}} is {.val {NA_character_}}.")
    if (nchar(x) == 0L)
        cli::cli_abort("{.arg {x_arg}} is blank.")
    if (grepl("[[:blank:]]", x))
        cli::cli_abort("{.arg {x_arg}} contains blanks.")
    invisible(TRUE)
}                       


## HAS_TESTS
#' Check input data for 'ex_to_lifetab_brass'
#'
#' @param target A data frame, which must contain
#' a column called "ex"
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_target_ex_to_lifetab_brass <- function(target) {
    nms_invalid <- c("lx", "ax")
    if (!is.data.frame(target))
        cli::cli_abort(c("{.arg target} is not a data frame.",
                         i = "{.arg target} has class {.cls {class(target)}}."))
    nms_target <- names(target)
    for (nm in nms_invalid) {
        if (nm %in% nms_target)
            cli::cli_abort(c("{.arg target} has a variable called {.var {nm}}.",
                             i = "Only {.arg standard} can have a variable called {.var {nm}}."))
    }
    has_ex <- "ex" %in% nms_target
    if (!has_ex)
        cli::cli_abort(c("{.arg target} does not have a variable called {.var ex}.",
                         i = "Variables in {.arg target}: {.val {nms_target}}."))
    check_numeric(x = target[["ex"]],
                  x_arg = "ex",
                  check_na = TRUE,
                  check_positive = TRUE,
                  check_nonneg = FALSE,
                  check_whole = FALSE)
    has_beta <- "beta" %in% nms_target
    if (has_beta) {
        beta <- target[["beta"]]
        check_numeric(x = beta,
                      x_arg = "beta",
                      check_na = TRUE,
                      check_positive = TRUE,
                      check_nonneg = FALSE,
                      check_whole = FALSE)
    }
    nms_cols <- setdiff(nms_target, "ex")
    has_cols <- length(nms_cols) > 0L
    if ((nrow(target) > 1L) && !has_cols)
        cli::cli_abort("{.arg target} does not have index variables.")
    check_duplicated_rows(x = target,
                          nm_x = "target",
                          nms_cols = nms_cols)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that a list of colnum vectors, as produced by
#' tidyselect::eval_select(), is valid
#'
#' @param x A named list of named integer vectors
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_valid_colnum_list <- function(x) {
    if (!is.list(x))
        cli::cli_abort("Internal error: {.arg x} is not a list.")
    if (length(x) > 0L) {
        nms <- names(x)
        if (is.null(nms))
            cli::cli_abort("Internal error: {.arg x} does not have names.")
        if (any(duplicated(nms)))
            cli::cli_abort("Internal error: names for {.arg x} have duplicates.")
        if (!all(vapply(x, is.integer, TRUE)))
            cli::cli_abort("Internal error: elements of {.arg x} are not all integer vectors.")
        has_names <- function(y) !is.null(names(y))
        if (!all(vapply(x, has_names, TRUE)))
            cli::cli_abort("Internal error: elements of {.arg x} are not all named.")
    }
    invisible(TRUE)
}
