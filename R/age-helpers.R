
## Helpers for 'age' functions.

## The functions in this file are not visible to end users.

## HAS_TESTS
#' Classify age group labels
#'
#' Classify age group labels into one of five categories:
#' "0", "1-4", "single", "five", and "open".
#'
#' This categorisation is useful for creating life tables.
#'
#' @param x A vector of age labels.
#'
#' @returns A vector of age group categories (a character vector).
#'
#' @noRd
age_group_categ <- function(x) {
    limits <- age_limits(x)
    lower <- limits$lower
    upper <- limits$upper
    ans <- character(length = length(x))
    is_na <- is.na(lower) & is.na(upper)
    ans[is_na] <- NA
    is_open <- !is.na(lower) & is.infinite(upper)
    ans[is_open] <- "open"
    ans[!is_na & !is_open & (lower == 0L) & (upper == 1L)] <- "0"
    ans[!is_na & !is_open & (lower == 1L) & (upper == 5L)] <- "1-4"
    ans[!is_na & !is_open & (lower > 0L) & (upper - lower == 1L)] <- "single"
    ans[!is_na & !is_open & (upper - lower == 5L)] <- "five"
    ans
}


## Helpers for 'age_labels' ---------------------------------------------------
    
## HAS_TESTS
#' Create one-year age labels
#'
#' Create labels for one-year age groups.
#'
#' @inheritParams age_labels
#'
#' @return A character vector of length
#' `max - min + open`.
#'
#' @noRd
age_labels_single <- function(min, max, open) {
    if (open)
        label_open <- paste0(max, "+")
    if (max == min)
        ans <- label_open
    else {
        ans <- seq.int(from = min, to = max - 1L)
        ans <- as.character(ans)
        if (open)
            ans <- c(ans, label_open)
    }
    ans
}


## HAS_TESTS
#' Create 5-year age labels
#'
#' Create labels for 5-year age groups.
#'
#' @inheritParams age_labels_single
#'
#' @return A character vector of length
#' `(max - min)/5 + open`.
#'
#' @noRd
age_labels_five <- function(min, max, open) {
    if ((min %% 5L) != 0L)
        stop(gettextf("'%s' [%d] not divisible by 5",
                      "min", min),
             call. = FALSE)
    if ((max %% 5L) != 0L) {
        stop(gettextf("'%s' [%d] not divisible by 5",
                      "max", max),
             call. = FALSE)
    }
    if (open)
        label_open <- paste0(max, "+")
    if (max == min)
        ans <- label_open
    else {
        lower <- seq.int(from = min, to = max - 5L, by = 5L)
        upper <- lower + 4L
        ans <- paste(lower, upper, sep = "-")
        if (open)
            ans <- c(ans, label_open)
    }
    ans
}


## HAS_TESTS
#' Create life table age labels
#'
#' Create labels for 'abridged' life table
#' age groups, ie `"0", "1-4", "5-9",
#' "10-14", \dots, "<max>+"`.
#'
#' @inheritParams age_labels_single
#'
#' @return A character vector.
#'
#' @noRd
age_labels_lt <- function(min, max, open) {
    if (open)
        label_open <- paste0(max, "+")
    msg <- gettextf("age group derived from '%s' [%d] and '%s' [%d] not a valid life table age group",
                    "min", min, "max", max)
    if (min < 0L)
        stop(gettextf("'%s' equals %d : negative values not allowed in life table age groups",
                      "min", min),
             call. = FALSE)
    else if (min == 0L) {
        if (max == 0L) {
            ans <- label_open
        }
        else if (max == 1L) {
            ans <- "0"
            if (open)
                ans <- c(ans, label_open)
        }
        else if (max %in% 2:4) {
            stop(msg, call. = FALSE)
        }
        else if (max == 5L) {
            ans <- c("0", "1-4")
            if (open)
                ans <- c(ans, label_open)
        }
        else {
            ans <- c("0", "1-4",
                     age_labels_five(min = 5L, max = max, open = open))
        }
    }
    else if (min == 1L) {
        if (max == 1L) {
            ans <- label_open
        }
        else if (max %in% 2:4) {
            stop(msg, call. = FALSE)
        }
        else if (max == 5L) {
            ans <- "1-4"
            if (open)
                ans <- c(ans, label_open)
        }
        else {
            ans <- c("1-4",
                     age_labels_five(min = 5L, max = max, open = open))
        }
    }
    else if (min %in% 2:4) {
        stop(gettextf("'%s' [%d] not valid for a life table age group",
                      "min", min),
             call. = FALSE)
    }
    else { ## min >= 5
        ans <- age_labels_five(min = min, max = max, open = open)
    }
    ans
}


## Helpers for 'age_mid', 'age_lower', 'age_upper', 'age_group_type' ----------------

## HAS_TESTS
#' Derive upper and lower limits for age groups
#'
#' Derive upper and lower limits for 1-year,
#' 5-year and life-table age groups. The age
#' groups are processed by function `reformat_age()`
#' first, so do not need to conform to standard
#' formats. The upper limit for open groups
#' is `Inf`.
#'
#' @param x A vector of age group labels.
#' Can be a factor.
#'
#' @return A named list with two double
#' vectors. The names are `"lower"` and `"upper"`.
#'
#' @seealso Called by [age_lower()]
#' and [age_upper()].
#'
#' @noRd
age_limits <- function(x) {
    ## reformat 'x', including checking for validity
    x <- reformat_age(x, factor = FALSE)
    ## handle all-NA case (which includes zero-length)
    n_x <- length(x)
    if (all(is.na(x))) {
        lower <- rep(NA_real_, times = n_x)
        upper <- rep(NA_real_, times = n_x)
        return(list(lower = lower, upper = upper))
    }
    ## constants
    p_single <- "^([0-9]+)$"
    p_low_up <- "^([0-9]+)-([0-9]+)$"
    p_open <- "^([0-9]+)\\+$"
    p_any <- "^([0-9]+).*$"
    ## work with levels, rather than full vector
    levels <- unique(x)
    n_levels <- length(levels)
    ## classify levels
    is_na <- is.na(levels)
    is_single <- grepl(p_single, levels)
    is_low_up <- grepl(p_low_up, levels)
    is_open <- grepl(p_open, levels)
    ## derive lower
    lower <- rep(NA_real_, times = n_levels)
    lower[!is_na] <- as.numeric(sub(p_any, "\\1", levels[!is_na]))
    ## derive upper
    upper <- rep(NA_real_, times = n_levels)
    upper[is_single] <- lower[is_single] + 1
    upper[is_low_up] <- as.numeric(sub(p_low_up, "\\2", levels[is_low_up])) + 1
    upper[is_open] <- Inf
    ## expand to original vector
    i <- match(x, levels)
    lower <- lower[i]
    upper <- upper[i]
    ## return
    list(lower = lower, upper = upper)
}


## Helpers for 'reformat_age' ----------------------------------------------------

## HAS_TESTS
#' Attempt to treat vector as lower limits
#' of 5-year age groups
#'
#' If `x` consists of integer-like values
#' where the unique, sorted
#' values form a series `0, 5, 10, ..., A`,
#' where `A >= 50`, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return `NULL`.
#'
#' `x` is allowed to contain `NA`s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A character vector with the same
#' length as `x`, or `NULL`.
#'
#' @noRd
reformat_age_five <- function(x) {
    not_na <- !is.na(x)
    if (sum(not_na) < 11L)
        return(NULL)
    x_int <- suppressWarnings(as.integer(x))
    is_integerish <- all(!is.na(x_int[not_na]) & (x_int[not_na] == x[not_na]))
    if (!is_integerish)
        return(NULL)
    breaks_obtained <- unique(x_int)
    breaks_obtained <- sort.int(breaks_obtained, na.last = TRUE)
    max <- max(breaks_obtained, na.rm = TRUE)
    breaks_expected <- seq.int(from = 0L, to = max, by = 5L)
    has_na <- anyNA(breaks_obtained)
    if (has_na)
        breaks_expected <- c(breaks_expected, NA)
    if (!identical(breaks_obtained, breaks_expected))
        return(NULL)
    labels <- age_labels_five(min = 0L, max = max, open = TRUE)
    if (has_na)
        labels <- c(labels, NA)
    i <- match(x_int, breaks_expected)
    labels[i]
}


## HAS_TESTS
#' Attempt to treat vector as lower limits
#' of life table age groups
#'
#' If `x` consists of integer-like values
#' where the unique, sorted
#' values form a series `0, 1, 5, 10, ..., A`,
#' where `A >= 50`, construct labels
#' "0-4", "5-9", "10-14", ..., "A+". 
#' Otherwise return `NULL`.
#'
#' `x` is allowed to contain `NA`s,
#' which are propagated through to the result.
#'
#' @param x A vector.
#'
#' @return A factor with the same
#' length as `x`, or `NULL`.
#'
#' @noRd
reformat_age_lt <- function(x) {
    not_na <- !is.na(x)
    if (sum(not_na) < 12L)
        return(NULL)
    x_int <- suppressWarnings(as.integer(x))
    is_integerish <- all(!is.na(x_int[not_na]) & (x_int[not_na] == x[not_na]))
    if (!is_integerish)
        return(NULL)
    breaks_obtained <- unique(x_int)
    breaks_obtained <- sort.int(breaks_obtained, na.last = TRUE)
    min <- min(breaks_obtained, na.rm = TRUE)
    max <- max(breaks_obtained, na.rm = TRUE)
    breaks_valid <- c(0L, 1L, seq.int(from = 5L, to = max, by = 5L))
    has_na <- anyNA(breaks_obtained)
    if (has_na)
        breaks_valid <- c(breaks_valid, NA)
    if (!all(breaks_obtained %in% breaks_valid))
        return(NULL)
    labels <- age_labels_lt(min = min, max = max, open = TRUE)
    if (has_na)
        labels <- c(labels, NA)
    i <- match(x_int, breaks_obtained)
    labels[i]
}


## HAS_TESTS
#' Try to translate age labels to
#' standard format
#'
#' Apply a series of text manipulations
#' transformations in an attempt to convert
#' `x` into age group labels in the
#' style of [age_labels_five()],
#' [age_labels_lt()], or
#' [age_labels_single()].
#'
#' `NA`s are permitted, and propagate
#' through to the results.
#' 
#' @param x A vector of possible labels.
#'
#' @return A character vector.
#'
#' @noRd
translate_age_labels <- function(x) {
    year <- "year|years|yr|yrs"
    infant <- "^infants$|^infant$|^in1st$|^lessthan1$|^under1$|^lessthanone$|^in1styear$|^0-0$|^0_0$"
    plus <- "andabove$|andmore$|andover$|andolder$|ormore$|orolder$|orover$|plus$|-$|_$|--$|__$"
    num <- c("zero", "one", "two", "three", "four",
             "five", "six", "seven", "eight", "nine")
    ## test whether 'x' consists of lower limits
    ## of 5-year or abridged life table age groups
    ans <- reformat_age_five(x)
    if (!is.null(ans))
        return(ans)
    ans <- reformat_age_lt(x)
    if (!is.null(ans))
        return(ans)
    ## put everything into lower case
    x <- tolower(x)
    ## trim leading zeros from any numbers
    x <- gsub("(?<![0-9])0+(?=[0-9])", "", x, perl = TRUE)
    ## remove "year" labels
    x <- sub(year, "", x)
    ## remove spaces
    x <- gsub(" ", "", x)
    ## translate synonyms for age group "0"
    x <- sub(infant, "0", x)
    ## translate synonyms for "+"
    x <- sub(plus, "+", x)
    ## translate synonyms for "-"
    x <- sub("^([0-9]+)to([0-9]+)$", "\\1-\\2", x)
    x <- sub("^([0-9]+)[[:punct:]]+([0-9]+)$", "\\1-\\2", x)
    ## translate numbers
    for (i in seq_along(num))
        x <- gsub(num[[i]], i - 1L, x)
    ## return result
    x
}
