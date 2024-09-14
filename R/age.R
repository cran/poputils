
## HAS_TESTS
#' Create Age Labels
#'
#' @description
#' Create labels for age groups. The labels depend
#' on the `type` argument:
#'
#'   - `"single"`. One-year age groups, eg
#'      `"0"` or `"55"`, and possibly
#'      an open age group, eg `"90+"`.
#'   - `"five"`. Five-year age groups, eg
#'      `"0-4"` or `"55-59"`, and possibly
#'      an open age group, eg `"100+"`.
#'   - `"lt"`. Life table age groups, eg
#'      `"0"`, `"1-4"`, `"5-9"`,
#'      `"55-59"`, or `"80+"`.
#'
#' @details
#' The first age group starts at the age
#' specified by `min`. If `open` is `TRUE`,
#' then the final age group starts at the age
#' specified by `max`. Otherwise, the
#' final age group ends at the age specified
#' by `max`.
#'
#' `open` defaults to `TRUE` when
#' `min` equals zero, and to `FALSE`
#' otherwise.
#'
#' @param type Type of age group labels:
#' `"single"`, `"five"`, or `"lt"`.
#' @param min Minimum age. Defaults to 0.
#' @param max Maximum age for closed age groups.
#' Defaults to 100.
#' @param open Whether the last age group is
#' "open", ie has no upper limit.
#'
#' @returns A character vector.
#'
#' @seealso [reformat_age()]
#'
#' @examples
#' age_labels(type = "single", min = 15, max = 40)
#' age_labels(type = "five")
#' age_labels(type = "lt", max = 80)
#' @export
age_labels <- function(type, min = 0, max = 100, open = NULL) {
    type <- match.arg(type, choices = c("single", "five", "lt"))
    check_number(x = min,
                 x_arg = "min",
                 check_na = TRUE,
                 check_positive = FALSE,
                 check_nonneg = TRUE,
                 check_whole = FALSE)
    check_number(x = max,
                 x_arg = "max",
                 check_na = TRUE,
                 check_positive = FALSE,
                 check_nonneg = TRUE,
                 check_whole = FALSE)
    min <- as.integer(min)
    max <- as.integer(max)
    if (max < min)
        stop(gettextf("'%s' [%d] is less than '%s' [%d] ",
                      "max", max, "min", min),
             call. = FALSE)
    if (is.null(open))
        open <- identical(min, 0L)
    else
        check_flag(open)
    if ((max == min) && !open)
        stop(gettextf("'%s' [%d] equals '%s' [%d] but '%s' is %s",
                      "max", max, "min", min, "open", "FALSE"),
             call. = FALSE)
    if (identical(type, "single"))
        age_labels_single(min = min, max = max, open = open)
    else if (identical(type, "five"))
        age_labels_five(min = min, max = max, open = open)
    else if (identical(type, "lt"))
        age_labels_lt(min = min, max = max, open = open)
    else
        stop(gettextf("invalid 'type' : \"%s\"", type),  ## nocov - ruled out by 'match.arg'
             call. = FALSE)                              ## nocov   
}


## HAS_TESTS
#' Lower Limits, Midpoints, and Upper Limits of Age Groups
#'
#' @description
#' Given a vector `x` of age group labels, return
#' a numeric vector.
#' - `age_lower()` returns the lower limits of each age group,
#' - `age_mid()` returns the midpoints, and
#' - `age_upper()` returns the upper limits.
#'
#' Vector `x` must describe 1-year, 5-year or life-table
#' age groups: see [age_labels()] for examples. `x` can
#' format these age groups in any way understood by
#' [reformat_age()].
#'
#' @details
#' These functions can make age groups easier to work
#' with. Lower and upper limits can be used for
#' selecting on age. Replacing age group with midpoints
#' can improve graphs. 
#'
#' @param x A vector of age group labels.
#'
#' @return A numeric vector, the same length as `x`.
#'
#' @seealso [reformat_age()] [age_labels()]
#'
#' @examples
#' x <- c("15-19", "5-9", "50+")
#' age_lower(x)
#' age_mid(x)
#' age_upper(x)
#'
#' ## non-standard formats are OK
#' age_lower(c("infants", "100 and over"))
#'
#' df <- data.frame(age = c("1-4", "10-14", "5-9", "0"),
#'                  rate = c(0.023, 0.015, 0.007, 0.068))
#' df
#' subset(df, age_lower(age) >= 5)
#' @export
age_lower <- function(x) {
    age_limits(x)$lower
}

#' @export
#' @rdname age_lower
age_mid <- function(x) {
    limits <- age_limits(x)
    lower <- limits$lower
    upper <- limits$upper
    ans <- 0.5 * (lower + upper)
    is_open <- is.infinite(upper)
    if (any(is_open)) {
        if (all(is_open))
            stop(gettextf("unclear whether '%s' consists of 1-year, 5-year, or life-table age groups",
                          "x"),
                 call. = FALSE)
        is_zero <- (lower == 0) & (upper == 1)
        if (all(is_open | is_zero))
            stop(gettextf("unclear whether '%s' consists of 1-year or life-table age groups",
                          "x"),
                 call. = FALSE)
        is_nonzero_single <- !is_zero & (upper - lower == 1)
        increment_open <- if (any(is_nonzero_single)) 0.5 else 2.5
        ans[is_open] <- lower[is_open] + increment_open
    }
    ans
}

#' @export
#' @rdname age_lower
age_upper <- function(x) {
    age_limits(x)$upper
}


## HAS_TESTS
#' Infer Age Label Type
#'
#' Determine whether a set of age labels
#' refer to one-year, five-year, or
#' life-table age groups.
#'
#' The valid types of age labels are:
#'   - `"single"`. One-year age groups, eg
#'      `"0"` or `"55"`, and possibly
#'      an open age group, eg `"90+"`.
#'   - `"five"`. Five-year age groups, eg
#'      `"0-4"` or `"55-59"`, and possibly
#'      an open age group, eg `"100+"`.
#'   - `"lt"`. Life table age groups, eg
#'      `"0"`, `"1-4"`, `"5-9"`,
#'      `"55-59"`, or `"80+"`.
#'
#' If `x` does not fit any of these
#' descriptions, `then age_group_type()` throws
#' an error.
#'
#' If `x` could belong to
#' more than one type, then `age_group_type()`
#' prefers `"single"` to `"five"` and `"lt"`,
#' and prefers `"five"` to `"lt"`.
#'
#' @param x A vector of age labels
#'
#' @returns `"single"`, `"five"`, or `"lt"`.
#'
#' @examples
#' age_group_type(c("5-9", "0-4", "100+"))
#' age_group_type(c("2", "5", "1"))
#' age_group_type(c("0", "1-4"))
#'
#' ## could be any "single" or "lt"
#' age_group_type("0")
#'
#' ## could be "five" or "lt"
#' age_group_type("80-84")
#' @export
age_group_type <- function(x) {
    age <- unique(x)
    age_limits <- age_limits(age)
    ## single 'age_limits()' did not throw an error, we can 
    ## assume that 'age' is one of "single", "lt", "five"
    lower <- age_limits$lower
    upper <- age_limits$upper
    is_not_na <- !is.na(lower) & !is.na(upper)
    lower <- lower[is_not_na]
    upper <- upper[is_not_na]
    ord <- order(lower)
    lower <- lower[ord]
    upper <- upper[ord]
    n <- length(lower)
    if (n == 0L)
        return("single")
    is_open <- is.infinite(upper[[n]])
    if (is_open) {
        if (n == 1L)
            return("single")
        else
            diff <- upper[-n] - lower[-n]
    }
    else
        diff <- upper - lower
    if (all(diff == 1L))
        return("single")
    if (all(diff == 5L))
        return("five")
    if (any(diff == 1L) || any(diff == 4L))
        return("lt")
    cli::cli_abort("Internal error: Unexpected combination of values for 'lower', 'upper'") ## nocov
}


## HAS_TESTS
#' Validity Checks for Age Labels
#'
#' Check that age labels can be parsed and,
#' optionally, whether the labels are
#' complete, unique, start at zero,
#' and end with an open age group.
#'
#' By default, `check_age()` only tests whether
#' a set of labels can be parsed as single-year,
#' five-year, or life table age groups.
#' (See [age_group_type()] for more on the three
#' types of age group.) However, it can
#' also apply the following tests:
#'
#' - `complete`. Whether `x` includes
#'   all intermediate age groups, with no gaps.
#'   For instance, the labels `c("10-14", "15-19", "5-9")`
#'   are complete, while the labels`c("15-19", "5-9")`
#'   are not (because they are missing `"10-14"`.)
#' - `unique`. Whether `x` has duplicated labels.
#' - `zero`. Whether the youngest age group in `x` starts
#'   at age 0, ie whether it includes `"0"` or `"0-4"`.
#' - `open`. Whether the oldest age group in `x` has an "open"
#'   age group, such as `"100+"` or `"65+"`, that has no
#'   upper limit.
#'
#' @param x A vector of age labels.
#' @param complete If `TRUE`,
#' test whether `x` has gaps.
#' @param unique If `TRUE`,
#' test whether `x` has duplicates.
#' @param zero If `TRUE`,
#' test whether youngest age group in
#' `x` starts at 0.
#' @param open If `TRUE`,
#' test whether oldest age group in `x`
#' is open.
#'
#' @returns `TRUE`, invisibly, or raises an
#' error if a test fails.
#'
#' @seealso
#' - [reformat_age()] to convert age labels to
#'   the format used by **poputils**.
#'
#' @examples
#' try(
#'   check_age(c("10-14", "0-4", "15+"),
#'             complete = TRUE)  
#' )
#'
#' try(
#'   check_age(c("10-14", "5-9", "0-4", "5-9", "15+"),
#'             unique = TRUE)
#' )
#'
#' try(
#'   check_age(c("10-14", "5-9", "15+"),
#'             zero = TRUE)
#' )
#'
#' try(
#'   check_age(c("10-14", "0-4", "5-9"),
#'             open = TRUE)
#' )
#' @export
check_age <- function(x,
                      complete = FALSE,
                      unique = FALSE,
                      zero = FALSE,
                      open = FALSE) {
    check_flag(complete)
    check_flag(unique)
    check_flag(zero)
    check_flag(open)
    age <- reformat_age(x)
    levels_age <- levels(age)
    n_age <- length(levels_age)
    age_limits <- age_limits(levels_age)
    if (complete) {
        is_present <- levels_age %in% age
        i_missing <- match(FALSE, is_present, nomatch = 0L)
        if (i_missing > 0L) {
            age_missing <- levels_age[[i_missing]]
            cli::cli_abort("Age group {.val {age_missing}} is missing.")
        }
    }
    if (unique) {
        is_dup <- duplicated(x)
        i_dup <- match(TRUE, is_dup, nomatch = 0L)
        if (i_dup > 0L) {
            x_dup <- x[[i_dup]]
            cli::cli_abort("Age group {.val {x_dup}} is duplicated.")
        }
    }
    if (zero) {
        lower <- age_limits$lower
        if (lower[[1L]] != 0L) {
            i_youngest <- match(levels_age[[1L]], age)
            youngest <- x[[i_youngest]]
            cli::cli_abort(c("Youngest age group does not start at 0.",
                             i = "Youngest age group is {.val {youngest}}."))
        }
    }
    if (open) {
        upper <- age_limits$upper
        if (is.finite(upper[[n_age]])) {
            i_oldest <- match(levels_age[[n_age]], age)
            oldest <- x[[i_oldest]]
            cli::cli_abort(c("Oldest age group is not open.",
                             i = "Oldest age group is {.val {oldest}}."))
        }
    }
    invisible(TRUE)
}


## HAS_TESTS
#' Aggregate Age Group Labels
#'
#' @description
#' Convert age group labels to a less detailed classification.
#' The three classifications recognized by `combine_age()`
#' are `"single"`, `"five"`, and `"lt"`, as defined on
#' [age_labels()]. The following conversions are permitted:
#'
#' - `"single"` ---> `"lt"`
#' - `"single"` ---> `"five"`
#' - `"lt"` ---> `"five"`
#' 
#' @param x A vector of age labels
#' @param to Type of age classification
#' to convert to: `"five"` or `"lt"`.
#' Defaults to `"five"`.
#'
#' @returns If `x` is a factor, then `combine_age()`
#' returns a factor; otherwise it returns a
#' character vector.
#'
#' @seealso
#' - [age_labels()] to create age group labels
#' - [reformat_age()] to convert existing age group labels
#' to a standard format
#' - [set_age_open()] to set the lower limit
#' of the open age group
#'
#' @examples
#' x <- c("0", "5", "3", "12")
#' combine_age(x)
#' combine_age(x, to = "lt")
#' @export
combine_age <- function(x, to = c("five", "lt")) {
  ## extract values
  to <- match.arg(to)
  limits_old <- age_limits(x)
  lower_old <- limits_old$lower
  upper_old <- limits_old$upper
  open <- any(is.infinite(upper_old))
  ## deal with degenerate cases
  n_non_na <- sum(!is.na(x))
  if (n_non_na == 0L)
    return(x)
  if ((n_non_na == 1L) && open) {
    age_open <- lower_old[is.infinite(upper_old)][[1L]]
    if ((to == "five") && (age_open %% 5L != 0L))
      stop(gettextf(paste("cannot convert to %s age groups :",
                          "open age group starts at %d"),
                    "5-year",
                    age_open))
    if ((to == "lt") && (age_open %% 5L != 0L) && (age_open != 1L))
      stop(gettextf(paste("cannot convert to %s age groups :",
                          "open age group starts at %d"),
                    "life table",
                    age_open))
    return(x)
  }
  ## determine type of 'x'
  diff_old <- upper_old - lower_old
  diff_old <- diff_old[!is.na(diff_old)]
  if (open)
    diff_old <- diff_old[is.finite(diff_old)]
  if (all(diff_old == 5L))
    type_from <- "five"
  else if (all(diff_old == 1L))
    type_from <- "single"
  else
    type_from <- "lt"
  ## deal with trivial or impossible conversions
  if (type_from == to)
    return(x)
  if (type_from == "five")
    stop(gettextf("cannot convert %s age groups to %s age groups",
                  "5-year",
                  "life table"),
         call. = FALSE)
  ## make new lower bounds
  lower_min_old <- min(lower_old, na.rm = TRUE)
  lower_max_old <- max(lower_old, na.rm = TRUE)
  if (to == "five") {
    lower_new <- seq.int(from = lower_min_old,
                         to = lower_max_old,
                         by = 5L)
  }
  else {
    if (lower_min_old == 0L) {
      lower_new <- 0L
      if (lower_max_old >= 1L)
        lower_new <- c(lower_new,
                       1L)
      if (lower_max_old >= 5L) {
        lower_new <- c(lower_new,
                       seq.int(from = 5L,
                               to = lower_max_old,
                               by = 5L))
      }
    }
    else if (lower_min_old %in% 1:4) {
      lower_new <- 1L
      if (lower_max_old >= 5L) {
        lower_new <- c(lower_new,
                       seq.int(from = 5L,
                               to = lower_max_old,
                               by = 5L))
      }
    }
    else {
      lower_min_new <- (lower_min_old %/% 5L) * 5L
      lower_new <- seq.int(from = lower_min_new,
                           to = lower_max_old,
                           by = 5L)
    }
  }
  ## make new labels
  min <- min(lower_new)
  max <- max(lower_new)
  if (!open) {
    if (to == "five")
      max <- max + 5L
    else {
      if (max == 0L)
        max <- 1L
      else if (max == 1L)
        max <- 5L
      else
        max <- max + 5L
    }
  }
  labels_new <- age_labels(type = to,
                           min = min,
                           max = max,
                           open = open)
  ## assign elements of 'x' to new labels
  vec <- c(lower_new, Inf)
  i <- findInterval(lower_old, vec)
  ## construct result and return
  ans <- labels_new[i]
  if (is.factor(x)) {
    if (NA %in% levels(x))
      ans <- factor(ans,
                    levels = c(labels_new, NA),
                    exclude = character())
    else
      ans <- factor(ans,
                    levels = labels_new)
  }
  ans
}


## HAS_TESTS
#' Reformat Age Group Labels
#'
#' @description
#' Convert age group labels to one of three formats:
#'
#' - Single-year age groups, eg
#'      `"0"`, `"1"`, ..., `"99"`, `"100+"`.
#' - Life table age groups, eg
#'      `"0"`, `"1-4", `"5-9"`, ..., `"95-99"`, `"100+"`.
#' - Five-year age groups, eg
#'      `"0-4"`, `"5-9"`, ..., `"95-99"`, `"100+"`.
#'
#' By default `reformat_age()` returns a factor
#' that includes all intermediate age groups. 
#' See below for examples.
#'
#' @details
#' `reformat_age()` applies the following algorithm:
#' 
#'   1. Tidy and translate text,
#'     eg convert `"20 to 24 years"` to
#'     `"20-24"`, convert `"infant"` to
#'     `"0"`, or convert `"100 or more"` to
#'     `"100+"`.
#'   1. Check whether the resulting
#'     labels could have been produced by
#'     [age_labels()]. If not, throw an error.
#'   1. If `factor` is `TRUE`
#'     (the default), then return a factor. The levels of
#'     this factor include all intermediate age groups.
#'     Otherwise return a character vector.
#'
#' When `x` consists entirely of numbers, `reformat_age()`
#' also checks for two special cases:
#' 
#' - If every element of `x` is a multiple of 5,
#'   and if `max(x) >= 50`, then `x` is assumed to
#'   describe 5-year age groups
#' - If every element of `x` is 0, 1, or a multiple
#'   of 5, with `max(x) >= 50`, then `x` is assumed
#'   to describe life table age groups.
#' 
#' @param x A vector.
#' @param factor Whether the return value
#' should be a factor.
#'
#' @return If `factor` is `TRUE`,
#' then `reformat_age()` returns a factor;
#' otherwise it returns a character vector.
#'
#' @seealso [age_labels()], [reformat_sex()]
#' 
#' @examples
#' reformat_age(c("80 to 84", "90 or more", "85 to 89"))
#'
#' ## factor contains intermediate level missing from 'x'
#' reformat_age(c("80 to 84", "90 or more"))
#'
#' ## non-factor
#' reformat_age(c("80 to 84", "90 or more"),
#'           factor = FALSE)
#' 
#' ## single
#' reformat_age(c("80", "90plus"))
#'
#' ## life table
#' reformat_age(c("0",
#'             "30-34",
#'             "10--14",
#'             "1-4 years"))
#' @export
reformat_age <- function(x, factor = TRUE) {
  if (!is.vector(x) && !is.factor(x))
    cli::cli_abort(c("{.arg x} is not a vector or factor.",
                     i = "{.arg x} has class {.cls {class(x)}}."))
  check_flag(factor)
  ## constants
  p_single <- "^([0-9]+)$"
  p_low_up <- "^([0-9]+)-([0-9]+)$"
  p_open <- "^([0-9]+)\\+$"
  ## handle all-NA cases (which includes zero-length)
  if (all(is.na(x)) || (is.factor(x) && all(is.na(levels(x))))) {
    if (factor)
      ans <- factor(x, exclude = character())
    else
      ans <- as.character(x)
    return(ans)
  }
  ## for efficiency, work with unique values
  levels_old <- unique(x)
  n_level <- length(levels_old)
  ## attempt to transform to standard format
  ## using only string operations
  levels_new <- translate_age_labels(levels_old)
  ## classify levels
  is_na <- is.na(levels_new)
  is_single <- grepl(p_single, levels_new)
  is_low_up <- grepl(p_low_up, levels_new)
  is_open <- grepl(p_open, levels_new)
  is_level_valid <- is_na | is_single | is_low_up | is_open
  i_level_invalid <- match(FALSE, is_level_valid, nomatch = 0L)
  if (i_level_invalid > 0L) {
    lab <- levels_old[[i_level_invalid]]
    cli::cli_abort("Can't parse label {.val {lab}}.")
  }
  ## characterise bounds
  lower <- rep(NA, times = n_level)
  lower[is_single] <- as.integer(sub(p_single, "\\1", levels_new[is_single]))
  lower[is_low_up] <- as.integer(sub(p_low_up, "\\1", levels_new[is_low_up]))
  lower[is_open] <- as.integer(sub(p_open, "\\1", levels_new[is_open]))
  upper <- rep(NA, times = n_level)
  upper[is_single] <- lower[is_single] + 1L
  upper[is_low_up] <- as.integer(sub(p_low_up, "\\2", levels_new[is_low_up])) + 1L
  ## note that 'upper' is NA when 'is_open' is TRUE, so subsequent
  ## code needs to take precautions in calculations involving 'upper'
  has_na <- any(is_na)
  has_open <- any(is_open)
  min <- min(lower, na.rm = TRUE)
  max <- if (has_open) max(lower, na.rm = TRUE) else max(upper, na.rm = TRUE)
  ## check open interval
  if (has_open) {
    is_closed_max <- upper > max
    i_closed_max <- match(TRUE, is_closed_max, nomatch = 0L)
    if (i_closed_max > 0L) {
      age1 <- levels_old[is_open][[1L]]
      age2 <- levels_old[[i_closed_max]]
      cli::cli_abort("Age groups {.val {age1}} and {.val {age2}} overlap.")
    }
    if (any(lower[is_open] != max)) {
      levels_open <- levels_old[is_open]
      cli::cli_abort(c("Open age groups have different lower limits.",
                       i = "Open age groups: {.val {levels_open}}."))
    }
  }
  examples_invalid <- character(3L)
  ## check 5-year age groups
  is_lower_mult_five <- lower %% 5L == 0L
  is_diff_five <- upper - lower == 5L
  is_diff_five[is_open] <- FALSE
  is_low_up_mult_five <- is_low_up & is_lower_mult_five & is_diff_five
  is_open_mult_five <- is_open & is_lower_mult_five
  is_valid_five <- is_na | is_low_up_mult_five | is_open_mult_five
  i_invalid_five <- match(FALSE, is_valid_five, nomatch = 0L)
  is_all_valid_five <- i_invalid_five == 0L
  if (!is_all_valid_five) {
    examples_invalid[[1L]] <- levels_old[[i_invalid_five]]
    ## check life table age groups
    is_single_zero <- is_single & (lower == 0L)
    is_low_up_one_five <- is_low_up & (lower == 1L) & (upper == 5L)
    is_low_up_one_five[is_open] <- FALSE
    is_low_up_mult_five_above_five <- is_low_up_mult_five & (lower >= 5L)
    is_valid_lt <- (is_na
      | is_single_zero
      | is_low_up_one_five
      | is_low_up_mult_five_above_five
      | is_open_mult_five)
    i_invalid_lt <- match(FALSE, is_valid_lt, nomatch = 0L)
    is_all_valid_lt <- i_invalid_lt == 0L
    if (!is_all_valid_lt) {
      examples_invalid[[2L]] <- levels_old[[i_invalid_lt]]
      ## check single age groups
      is_valid_single <- (is_na
        | is_single
        | is_open)
      i_invalid_single <- match(FALSE, is_valid_single, nomatch = 0L)
      is_all_valid_single <- i_invalid_single == 0L
      if (!is_all_valid_single)
        examples_invalid[[3L]] <- levels_old[[i_invalid_single]]
    }
  }
  if (is_all_valid_five)
    type <- "five"
  else if (is_all_valid_lt)
    type <- "lt"
  else if (is_all_valid_single)
    type <- "single"
  else {
    stop(gettextf(paste("unable to parse '%s' as age group labels :",
                        "\n - label \"%s\" incompatible with 5-year age groups,",
                        "\n - label \"%s\" incompatible with life table age groups,",
                        "\n - label \"%s\" incompatible with 1-year age groups"),
                  "x",
                  examples_invalid[[1L]],
                  examples_invalid[[2L]],
                  examples_invalid[[3L]]),
         call. = FALSE)
  }
  levels_complete <- age_labels(type = type,
                                min = min,
                                max = max,
                                open = has_open)
  if (has_na)
    levels_complete <- c(levels_complete, NA)
  i_lev_to_lab <- match(levels_new, levels_complete)
  i_x_to_lev <- match(x, levels_old)
  i <- i_lev_to_lab[i_x_to_lev]
  ans <- levels_complete[i]
  if (factor)
    ans <- factor(ans,
                  levels = levels_complete,
                  exclude = character())
  ans
}


## HAS_TESTS
#' Specify Open Age Group
#'
#' Set the lower limit of the open age group.
#' Given a vector of age group labels,
#' recode all age groups with a lower limit
#' greater than or equal to `<lower>` to `<lower>+`.
#'
#' `set_age_open()` requires that `x` and
#' the return value have a
#' a five-year, single-year, or life table format,
#' as described in [age_labels()].
#'
#' @param x A vector of age labels.
#' @param lower An integer. The lower limit
#' for the open age group.
#'
#' @returns A modified version of `x`.
#'
#' @seealso
#' - `set_age_open()` uses [age_lower()] to identify
#' lower limits
#' - [age_labels()] for creating age labels from scratch
#'
#' @examples
#' x <- c("100+", "80-84", "95-99", "20-24")
#' set_age_open(x, 90)
#' set_age_open(x, 25)
#' @export
set_age_open <- function(x, lower) {
    if (!is.vector(x) && !is.factor(x))
        cli::cli_abort(c("{.arg x} is not a vector or a factor.",
                         i = "{.arg x} has class {.cls {class(x)}}."))
    check_number(x = lower,
                 x_arg = "lower",
                 check_na = TRUE,
                 check_positive = FALSE,
                 check_nonneg = TRUE,
                 check_whole = FALSE)
    lower <- as.integer(lower)
    if (is.factor(x)) {
        levels_old <- levels(x)
        levels_new <- set_age_open(x = levels_old, lower = lower)
        x <- levels_new[match(x, levels_old)]
        factor(x, levels = unique(levels_new), exclude = character())
    }
    else {
        x <- as.character(x)
        limits <- age_limits(x)
        lower_old <- limits$lower
        upper_old <- limits$upper
        if (all(is.na(lower_old)) && all(is.na(upper_old)))
            return(x)
        is_open_old <- is.infinite(limits$upper)
        if (any(is_open_old)) {
            lower_open_old <- lower_old[is_open_old][[1L]]
            if (lower > lower_open_old)
                stop(gettextf(paste("'%s' [%d] is greater than current lower limit",
                                    "for open age group [%d]"),
                              "lower",
                              lower,
                              lower_open_old),
                     call. = FALSE)
        }
        x[lower_old >= lower] <- paste0(lower, "+")
        val <- tryCatch(reformat_age(unique(x)),
                        error = function(e) e)
        if (inherits(val, "error"))
            stop(gettext("new age groups are invalid"),
                 call. = FALSE)
        x
    }
}




