
## HAS_TESTS
#' Calculate Total Fertility Rates
#'
#' Calculate the total fertility rate (TFR)
#' from age-specific fertility rates.
#'
#' The total fertility rate is a summary measures for
#' current fertility levels that removes the effect
#' of age structure. Is obtained by summing up age-specific
#' fertility rates, multiplying each rate by the
#' width of the corresponding age group. For instance,
#' the rate for age group "15-19" is multiplied by
#' 5, and the rate for age group "15" is multiplied by 1.
#'
#' The total fertility rate can be interpreted as the
#' number of average children that a
#' person would have, under prevailing fertility rates,
#' if the person survived to the maximum
#' age of reproduction.
#' The hypothetical person is normally a woman, since
#' age-specific fertility rates normally use
#' person-years lived by women as the denominator. But it
#' can apply to men, if the age-specific fertility rates
#' are "paternity rates", ie rates that use
#' person-years lived by men as the denominator.
#'
#' @section Sex-specific fertility rates:
#'
#' Age-specific fertility rates do not
#' normally specify the sex of the children
#' who are born. In cases where they do, however,
#' rates have to be summed across sexes to
#' give the total fertility rates. If `tfr()` is
#' supplied with a `sex` argument, it assumes that
#' `sex` applies to the births, and sums over the sexes.
#'
#' @section Denominator:
#'
#' Published tables of age-specific fertility rates
#' often express the rates as births per 1000 person-years
#' lived, rather than per person-year lived. (Sometimes
#' this is expressed as "births per 1000 women".)
#' In these cases 
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for the `asfr` argument to
#' `tfr()` is a way of representing
#' uncertainty. This uncertainty is propagated
#' through to the TFR, which will
#' also be rvecs.
#'
#' @param data Data frame with age-specific fertility rates
#' and age
#' @param asfr Age-specific fertility rates.
#' Possibly an [rvec][rvec::rvec()].
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. The labels must be
#' interpretable by functions
#' such as [reformat_age()] and [age_group_type()].
#' The age groups must not have gaps,
#' and the highest age group must be "closed"
#' (ie have an upper limit.)
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Sex/gender of the child (not the parent). 
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate total fertility rates are calculated
#' for each combination the `by` variables.
#' If `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, then the grouping variables
#' take precedence over `by`.
#' @param denominator The denominator used to
#' calculate `asfr`. Default is 1. 
#' @param suffix Optional suffix added to `"tfr"`
#' column in result.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [lifeexp()] Calculate life expectancy from age-specific
#'   mortality rates.
#'
#' @examples
#' iran_fertility |>
#'   tfr(asfr = rate,
#'       by = c(area, time),
#'       denominator = 1000)
#' @export
tfr <- function(data,
                asfr = NULL,
                age = age,
                sex = NULL,
                by = NULL,
                denominator = 1,
                suffix = NULL) {
  asfr_quo <- rlang::enquo(asfr)
  age_quo <- rlang::enquo(age)
  sex_quo <- rlang::enquo(sex)
  by_quo <- rlang::enquo(by)
  asfr_colnum <- tidyselect::eval_select(asfr_quo, data = data)
  age_colnum <- tidyselect::eval_select(age_quo, data = data)
  sex_colnum <- tidyselect::eval_select(sex_quo, data = data)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- groups_colnums(data)
  check_tfr_colnums(asfr_colnum = asfr_colnum,
                    age_colnum = age_colnum,
                    sex_colnum = sex_colnum,
                    by_colnums = by_colnums,
                    groups_colnums = groups_colnums)
  data <- remove_existing_tfr_col(data = data,
                                  suffix = suffix)
  is_by_supplied <- length(by_colnums) > 0L
  by_colnums <- if (is_by_supplied) by_colnums else groups_colnums
  has_by <- length(by_colnums) > 0L
  if (has_by) {
    tfr_by <- function(val, key) {
      tryCatch(tfr_inner(data = val,
                         asfr_colnum = asfr_colnum,
                         age_colnum = age_colnum,
                         sex_colnum = sex_colnum,
                         denominator = denominator,
                         suffix = suffix),
               error = function(cnd) {
                 str_key <- make_str_key(key)
                 msg1 <- "Problem calculating total fertility rate."
                 msg2 <- paste("Problem occurred where", str_key)
                 cli::cli_abort(c(msg1, i = msg2), parent = cnd)
               })
    }
    inputs <- vctrs::vec_split(x = data, by = data[by_colnums])
    vals <- inputs$val
    key <- inputs$key
    keys <- lapply(nrow(key), function(i) key[i, , drop = FALSE])
    ans <- .mapply(FUN = tfr_by,
                   dots = list(val = vals, key = keys),
                   MoreArgs = list())
    ans <- vctrs::vec_rbind(!!!ans)
    ans <- vctrs::vec_cbind(key, ans)
  }
  else {
    ans <- tfr_inner(data = data,
                     asfr_colnum = asfr_colnum,
                     age_colnum = age_colnum,
                     sex_colnum = sex_colnum,
                     denominator = denominator,
                     suffix = suffix)
  }
  nm_tfr <- "tfr"
  if (!is.null(suffix))
    nm_tfr <- paste(nm_tfr, suffix, sep = ".")
  tfr <- ans[[nm_tfr]]
  n_too_high <- sum(tfr > 100, na.rm = TRUE)
  if (n_too_high > 0L)
    cli::cli_warn(c("{cli::qty(n_too_high)}Value{?s} for TFR over 100.",
                    i = "{.arg denominator} currently equals {.val {denominator}}.",
                    i = "Does {.arg denominator} need to be set to a higher value?"))
  ans
}


## HAS_TESTS
#' Calculate TFR for One Combination of 'by' Variables
#'
#' @param data Data frame
#' @param asfr_colnu mNamed integer vector identifying 'asfr'
#' @param age_colnum Named integer vector identifying 'age'
#' @param sex_colnum Named integer vector identifying 'sex'
#' @param denominator Denominator used for calculating ASFR
#' @param suffix String added to the name of the tfr column
#'
#' @returns A tibble
#'
#' @noRd
tfr_inner <- function(data,
                      asfr_colnum,
                      age_colnum,
                      sex_colnum,
                      denominator,
                      suffix) {
  asfr <- data[[asfr_colnum]]
  check_numeric(x = asfr,
                x_arg = "asfr",
                check_na = FALSE,
                check_positive = FALSE,
                check_nonneg = TRUE,
                check_whole = FALSE)
  check_age_tfr <- function(x) {
    check_age(x = x,
              complete = TRUE,
              unique = TRUE,
              zero = FALSE,
              open = FALSE,
              closed = TRUE)
  }
  age <- data[[age_colnum]]
  has_sex <- length(sex_colnum) > 0L
  if (has_sex) {
    sex <- data[[sex_colnum]]
    ages <- split(age, sex)
    lapply(ages, check_age_tfr)
  }
  else
    check_age_tfr(age)
  check_number(x = denominator,
               x_arg = "denominator",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = FALSE,
               check_whole = FALSE)
  width <- age_upper(age) - age_lower(age)
  ans <- sum(asfr * width) / denominator
  ans <- tibble::tibble(tfr = ans)
  if (!is.null(suffix)) {
    check_string(x = suffix, x_arg = "suffix")
    names(ans) <- paste("tfr", suffix, sep = ".")
  }
  ans
}


## HAS_TESTS
#' Remove Exisiting TFR Column
#'
#' Remove column  "tfr" from data frame 'data', if present.
#'
#' @param data A data frame
#' @param suffix Optional suffix added to new
#' columns in result.
#' 
#' @returns A modified version of 'data'
#'
#' @noRd
remove_existing_tfr_col <- function(data, suffix) {
  nm_tfr_col <- "tfr"
  if (!is.null(suffix))
    nm_tfr_col <- paste(nm_tfr_col, suffix, sep = ".")
  nms_data <- names(data)
  nm_both <- intersect(nm_tfr_col, nms_data)
  if (length(nm_both) > 0L) {
    cli::cli_alert_info("Overwriting existing column {.var {nm_tfr_col}} in {.arg data}.")
    is_remove <- nms_data %in% nm_both
    data <- data[!is_remove]
  }
  data
}
