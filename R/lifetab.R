
## HAS_TESTS
#' Calculate Life Tables or Life Expectancies
#'
#' Calculate life table quantities. Function
#' `lifetab()` returns an entire life table.
#' Function `lifeexp()` returns life expectancy at birth.
#' The inputs can be mortality rates (`mx`) or
#' probabilities of dying (`qx`), though not both.
#'
#' @section Definitions of life table quantities:
#'
#' - `mx` Deaths per person-year lived.
#' - `qx` Probability of surviving from the start
#' of age group 'x' to the end.
#' - `lx` Number of people alive at
#' the start of age group `x`.
#' - `dx` Number of deaths in age group `x`
#' - `Lx` Expected number of person years lived in
#' age group `x`.
#' - `ex` Life expectancy, calculated at the start
#' of age group `x`.
#'
#' Mortality rates `mx` are sometimes expressed
#' as deaths per 1000 person-years lived, or per 100,000
#' person-years lived. `lifetab()` and `lifeexp()`
#' assumed that they are expressed as deaths per
#' person-year lived.
#'
#' @section Calculation methods:
#'
#' `lifetab()` and `lifeexp()` implement several
#' methods for calculating life table quantities
#' from mortality rates. Each method makes
#' different assumptions about
#' the way that mortality rates vary within
#' age intervals:
#' 
#' - `"constant"` Mortality rates are constant
#'   within each interval.
#' - `"linear"`. Life table quantity `lx`
#'   is a straight line within each interval. 
#'   Equivalently, deaths are distributed uniformly
#'   within each interval.
#' - `"CD"`. Used only with age groups "0"
#'   and "1-4". Mortality rates decline
#'   over the  age interval,
#'   with the slope depending on the absolute
#'   level of infant mortality. The formulas were
#'   developed by Coale and Demeny (1983),
#'   and used in Preston et al (2001).
#' - `"AK"`. Used only with age group "0".
#'   Mortality rates decline over the age interval,
#'   with the slope depending on the absolute
#'   level of infant mortality. The formulas
#'   were formulas developed by Andreev and Kingkade (2015),
#'   and are used in the Human Mortality Database
#'   [methods protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).
#'
#' For a detailed description of the methods,
#' see the vignette for **poputils**.
#'
#' @section ax:
#'
#' `ax` is the average number of years
#' lived in an age interval by people who
#' die in that interval. Demographers sometimes
#' refer to it as the 'separation factor'. If a non-`NA`
#' value of `ax` is supplied for an age group,
#' then the results for that age group are based
#' on the formula
#'
#' \deqn{m_x = d_x / (n_x l_x + a_x d_x)},
#'
#' (where `n_x` is the width of the age interval),
#' over-riding any methods specified via the `infant`, `child`,
#' `closed` and `open` arguments.
#'
#' @section Open age group when inputs are qx:
#'
#' The probability of dying, `qx`, is always 1 in the
#' final (open) age group. `qx` therefore provides
#' no direct information on mortality conditions
#' within the final age group. `lifetab()` and
#' `lifeexp()` use conditions in the second-to-final
#' age group as a proxy for conditions in the final
#' age group. When `open` is `"constant"` (which
#' is currently the only option), and no value
#' for `ax` in the final age group is provided,
#' `lifetab()` and `lifeexp()` assume
#' that \eqn{m_A = m_{A-1}}, and set
#' \eqn{L_{A} = l_A / m_A}.
#'
#' In practice, mortality is likely to be higher
#' in the final age group than in the second-to-final
#' age group, so the default procedure is likely to
#' lead to inaccuracies. When the size of the final
#' age group is very small, these inaccuracies will
#' be inconsequential. But in other cases, it may
#' be necessary to supply an explicit value for
#' `ax` for the final age group, or to use `mx`
#' rather than `qx` as inputs.
#'
#' @section Using rvecs to represent uncertainty:
#'
#' An [rvec][rvec::rvec()] is a 'random vector',
#' holding multiple draws from a distribution.
#' Using an rvec for the `mx` argument to
#' `lifetab()` or `lifeexp()` is a way of representing
#' uncertainty. This uncertainty is propagated
#' through to the life table values, which will
#' also be rvecs.
#'
#' @section Parallel processing:
#'
#' Calculations can be slow when working
#' with rvecs and many combinations of 'by' variables.
#' In these cases, setting `n_core` to a number
#' greater than 1, which triggers parallel processing,
#' may help.
#'
#' @param data Data frame with mortality data.
#' @param mx <[`tidyselect`][tidyselect::language]>
#' Mortality rates, expressed as deaths per 
#' person-year lived. Possibly an [rvec][rvec::rvec()].
#' @param qx <[`tidyselect`][tidyselect::language]>
#' Probability of dying within age interval.
#' An alternative to `mx`. Possibly an [rvec][rvec::rvec()].
#' @param at Age at which life expectancy is calculated
#' (`lifeexp() only). Default is `0`.
#' Can be a vector with length > 1.
#' @param age <[`tidyselect`][tidyselect::language]>
#' Age group labels. The labels must be
#' interpretable by functions
#' such as [reformat_age()] and [age_group_type()].
#' The first age group must start at age 0, and the
#' last age group must be "open", with no upper
#' limit.
#' @param sex <[`tidyselect`][tidyselect::language]>
#' Biological sex, with labels that can be
#' interpreted by [reformat_sex()]. Needed only when
#' `infant` is `"CD"` or `"AK"`, or `child` is
#' `"CD"`.
#' @param ax <[`tidyselect`][tidyselect::language]>
#' Average age at death within age group.
#' Optional. See Details. 
#' @param by <[`tidyselect`][tidyselect::language]>
#' Separate life tables, or life expectancies, 
#' calculated for each combination the `by` variables.
#' If a `sex` variable was specified, then that
#' variable is automatically included among the `by` 
#' variables. If `data` is a
#' [grouped](https://dplyr.tidyverse.org/reference/group_data.html)
#' data frame, then the grouping variables
#' take precedence over `by`.
#' @param infant Method used to calculate
#' life table values in age group `"0"`.
#' Ignored if `age` does not include age group `"0"`.
#' Default is `"constant"`.
#' @param child Method used to calculate
#' life table values in age group `"1-4"`.
#' Ignored if `age` does not include age group `"0"`.
#' Default is `"constant"`.
#' @param closed Method used to calculate
#' life table values in closed age intervals
#' other than `"0"` and `"1-4"` (ie intervals
#' such as "10-14" or "12"). Default is `"constant"`.
#' @param open Method used to calculate
#' life table values in the final, open age group
#' (eg `"80+"` or `"110+"`).
#' Currently the only option is `"constant".
#' @param radix Initial population for the
#' `lx` column. Default is `100000`.
#' @param suffix Optional suffix added to new
#' columns in result.
#' @param n_core Number of cores to use for parallel
#' processing. If `n_core` is `1` (the default),
#' no parallel processing is done.
#'
#' @returns A [tibble][tibble::tibble()].
#'
#' @seealso
#' - [ex_to_lifetab_brass()] Calculate life table from minimal inputs
#' - [q0_to_m0()] Convert between infant mortality measures
#' - [tfr()] Calculate total fertility rate
#'
#' @references
#' - Preston SH, Heuveline P, and Guillot M. 2001.
#' *Demography: Measuring and Modeling Population Processes*
#' Oxford: Blackwell.
#' - Coale AJ, Demeny P,  and Vaughn B. 1983.
#' *Regional model life tables and stable populations*
#' New York: Academic Press.
#' - Andreev, E.M. and Kingkade, W.W., 2015.
#' Average age at death in infancy and infant mortality level:
#' Reconsidering the Coale-Demeny formulas at current
#' levels of low mortality. *Demographic Research*,
#' 33, pp.363-390.
#' - Human Mortality Database [Methods Protocol](https://www.mortality.org/File/GetDocument/Public/Docs/MethodsProtocolV6.pdf).
#' - [Tools for Demographic Estimation](https://demographicestimation.iussp.org).
#'
#' @examples
#' library(dplyr)
#'
#' ## life table for females based on 'level 1'
#' ## mortality rates "West" model life table
#' west_lifetab |>
#'     filter(sex == "Female",
#'            level == 1) |>
#'     lifetab(mx = mx)
#'
#' ## change method for infant and children from
#' ## default ("constant") to "CD"
#' west_lifetab |>
#'     filter(sex == "Female",
#'            level == 1) |>
#'     lifetab(mx = mx,
#'             sex = sex,
#'             infant = "CD",
#'             child = "CD")
#'
#' ## calculate life expectancies
#' ## for all levels, using the 'by'
#' ## argument to distinguish levels
#' west_lifetab |>
#'     lifeexp(mx = mx,
#'             sex = sex,
#'             infant = "CD",
#'             child = "CD",
#'             by = level)
#'
#' ## obtain the same result using
#' ## 'group_by'
#' west_lifetab |>
#'   group_by(level) |>
#'   lifeexp(mx = mx,
#'           sex = sex,
#'           infant = "CD",
#'           child = "CD")
#'
#' ## calculations based on 'qx'
#' west_lifetab |>
#'   lifeexp(qx = qx,
#'           sex = sex,
#'           by = level)
#'
#' ## life expectancy at age 60
#' west_lifetab |>
#'   filter(level == 10) |>
#'   lifeexp(mx = mx,
#'           at = 60,
#'           sex = sex)
#'
#' ## life expectancy at ages 0 and 60
#' west_lifetab |>
#'   filter(level == 10) |>
#'   lifeexp(mx = mx,
#'           at = c(0, 60),
#'           sex = sex)
#' @export
lifetab <- function(data,
                    mx = NULL,
                    qx = NULL,
                    age = age,
                    sex = NULL,
                    ax = NULL,
                    by = NULL,
                    infant = c("constant", "linear", "CD", "AK"),
                    child = c("constant", "linear", "CD"),
                    closed = c("constant", "linear"),
                    open = "constant",
                    radix = 100000,
                    suffix = NULL,
                    n_core = 1) {
    mx_quo <- rlang::enquo(mx)
    qx_quo <- rlang::enquo(qx)
    age_quo <- rlang::enquo(age)
    sex_quo <- rlang::enquo(sex)
    ax_quo <- rlang::enquo(ax)
    by_quo <- rlang::enquo(by)
    infant <- match.arg(infant)
    child <- match.arg(child)
    closed <- match.arg(closed)
    open <- match.arg(open)
    methods <- c(infant = infant,
                 child = child,
                 closed = closed,
                 open = open)
    life_inner(data = data,
               mx_quo = mx_quo,
               qx_quo = qx_quo,
               at = 0,
               age_quo = age_quo,
               sex_quo = sex_quo,
               ax_quo = ax_quo,
               by_quo = by_quo,
               methods = methods,
               radix = radix,
               suffix = suffix,
               is_table = TRUE,
               n_core = n_core)
}


#' @export
#' @rdname lifetab
lifeexp <- function(data,
                    mx = NULL,
                    qx = NULL,
                    at = 0,
                    age = age,
                    sex = NULL,
                    ax = NULL,
                    by = NULL,
                    infant = c("constant", "linear", "CD", "AK"),
                    child = c("constant", "linear", "CD"),
                    closed = c("constant", "linear"),
                    open = "constant",
                    suffix = NULL,
                    n_core = 1) {
  mx_quo <- rlang::enquo(mx)
  qx_quo <- rlang::enquo(qx)
  age_quo <- rlang::enquo(age)
  sex_quo <- rlang::enquo(sex)
  ax_quo <- rlang::enquo(ax)
  by_quo <- rlang::enquo(by)
  infant <- match.arg(infant)
  child <- match.arg(child)
  closed <- match.arg(closed)
  open <- match.arg(open)
  methods <- c(infant = infant,
               child = child,
               closed = closed,
               open = open)
  life_inner(data = data,
             mx_quo = mx_quo,
             qx_quo = qx_quo,
             at = at,
             age_quo = age_quo,
             sex_quo = sex_quo,
             ax_quo = ax_quo,
             by_quo = by_quo,
             methods = methods,
             radix = 1,
             suffix = suffix,
             is_table = FALSE,
             n_core = n_core)
}



#' Convert q0 to m0
#'
#' Convert the probability of dying during infancy
#' (q0) to the mortality rate for infancy (m0).
#'
#' @section Warning:
#'
#' The term "infant mortality rate" is ambiguous.
#' Demographers sometimes use it to refer to m0
#' (which is an actual rate) and sometimes use
#' it to refer to q0 (which is a probability.)
#'
#' @param q0 Probability of dying in first
#' year of life. A numeric vector or an [rvec][rvec::rvec()].
#' @param sex Biological sex. A vector the same length
#' as `q0`, with labels that can be
#' interpreted by [reformat_sex()]. Needed only when
#' `infant` is `"CD"` or `"AK"`.
#' @param a0 Average age at death for infants who die.
#' Optional. See help for [lifetab()]. 
#' @param infant Calculation method.
#' See help for [lifetab()].
#' Default is `"constant"`.
#'
#' @returns A numeric vector or [rvec][rvec::rvec()].
#'
#' @seealso
#' - [lifetab()] Calculate a full life table.
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' west_lifetab |>
#'  filter(age == 0, level <= 5) |>
#'  select(level, sex, age, mx, qx) |>
#'  mutate(m0 = q0_to_m0(q0 = qx, sex = sex, infant = "CD"))
#' @export
q0_to_m0 <- function(q0,
                     sex = NULL,
                     a0 = NULL,
                     infant = c("constant", "linear", "CD", "AK")) {
  check_qx(qx = q0, nm_qx = "q0")
  n <- length(q0)
  infant <- match.arg(infant)
  if (is.null(sex)) {
    methods = c(infant = infant)
    check_sex_not_needed(methods)
    sex <- rep(NA_character_, times = n)
  }
  else {
    check_equal_length(x = q0, y = sex, nm_x = "q0", nm_y = "sex")
    sex <- reformat_sex(sex, factor = FALSE)
  }
  if (is.null(a0))
    a0 <- rep(NA_real_, times = n)
  else {
    if (rvec::is_rvec(a0))
      cli::cli_abort("{.arg a0} is an rvec.")
    check_numeric(x = a0,
                  x_arg = "a0",
                  check_na = FALSE,
                  check_positive = FALSE,
                  check_nonneg = TRUE,
                  check_whole = FALSE)
    n_gt_1 <- sum(a0 > 1, na.rm = TRUE)
    if (n_gt_1 > 0)
      cli::cli_abort("{.arg a0} has {cli::qty(n_gt_1)} value{?s} greater than 1.")
    a0 <- as.double(a0)
  }
  is_rvec <- rvec::is_rvec(q0)
  q0 <- as.matrix(q0)
  ans <- q0_to_m0_inner(q0 = q0,
                        sex = sex,
                        a0 = a0,
                        infant = infant)
  if (is_rvec)
    ans <- rvec::rvec_dbl(ans)
  else
    ans <- as.double(ans)
  ans
}
  


## Helper functions -----------------------------------------------------------



#' Names of life table methods that require a sex variable
#'
#' @returns A character vector
#'
#' @noRd
get_methods_need_sex <- function() c("CD", "AK")

## HAS_TESTS - via 'lifetab' and 'lifeexp',
#' which is more convenient because of quosures
#' Check structure of inputs, reformat, and pass to
#' function doing detailed checkings and calculation
#'
#' @param data Data frame with mortality data.
#' @param mx_quo Quosure identifying 'mx'
#' @param qx_quo Quosure identifying 'qx'
#' @param at Age at which life expectancy calculated.
#' @param age_quo Quosure identifying 'age'
#' @param sex_quo Quosure identifying 'sex'
#' @param ax_quo Quosure identifying 'ax'
#' @param by_quo Quosure identifying 'by'
#' @param methods Named character vector specifying
#' calculation methods
#' @param radix Initial population for 'lx'
#' @param suffix Suffix added to new columns
#' @param n_core Number of cores for parallel
#' processing
#'
#' @returns A tibble
#'
#' @noRd
life_inner <- function(data, 
                       mx_quo,
                       qx_quo,
                       at,
                       age_quo,
                       sex_quo,
                       ax_quo,
                       by_quo,
                       methods,
                       radix,
                       suffix,
                       is_table,
                       n_core) {
  if (!is.data.frame(data))
    cli::cli_abort(c("{.arg data} is not a data frame.",
                     i = "{.arg data} has class {.cls {class(data)}}."))
  mx_colnum <- tidyselect::eval_select(mx_quo, data = data)
  qx_colnum <- tidyselect::eval_select(qx_quo, data = data)
  age_colnum <- tidyselect::eval_select(age_quo, data = data)
  sex_colnum <- tidyselect::eval_select(sex_quo, data = data)
  ax_colnum <- tidyselect::eval_select(ax_quo, data = data)
  by_colnums <- tidyselect::eval_select(by_quo, data = data)
  groups_colnums <- groups_colnums(data)
  check_life_colnums(mx_colnum = mx_colnum,
                     qx_colnum = qx_colnum,
                     age_colnum = age_colnum,
                     sex_colnum = sex_colnum,
                     ax_colnum = ax_colnum,
                     by_colnums = by_colnums,
                     groups_colnums = groups_colnums)
  data <- remove_existing_lifetab_cols(data = data,
                                       is_table = is_table,
                                       suffix = suffix)
  is_sex_supplied <- length(sex_colnum) > 0L
  is_by_supplied <- length(by_colnums) > 0L
  is_groups_supplied <- length(groups_colnums) > 0L
  if (is_by_supplied)
    by_colnums <- unique(c(by_colnums, sex_colnum))
  else
    by_colnums <- unique(c(groups_colnums, sex_colnum))
  has_by <- length(by_colnums) > 0L
  if (has_by) {
    inputs <- vctrs::vec_split(x = data,
                               by = data[by_colnums])
    check_n(n = n_core,
            nm_n = "n_core",
            min = 1L,
            max = NULL,
            divisible_by = NULL)
    life_by <- function(val, key) {
      tryCatch(life_inner_one(data = val,
                              mx_colnum = mx_colnum,
                              qx_colnum = qx_colnum,
                              at = at,
                              age_colnum = age_colnum,
                              sex_colnum = sex_colnum,
                              ax_colnum = ax_colnum,
                              methods = methods,
                              radix = radix,
                              suffix = suffix,
                              is_table = is_table),
               error = function(cnd) {
                 str_key <- make_str_key(key)
                 msg1 <- "Problem calculating life table functions."
                 msg2 <- paste("Problem occurred where", str_key)
                 cli::cli_abort(c(msg1, i = msg2), parent = cnd)
               })
    }
    vals <- inputs$val
    key <- inputs$key
    keys <- lapply(nrow(key), function(i) key[i, , drop = FALSE])
    if (n_core > 1L) {
      iseed <- sample.int(n = .Machine$integer.max, size = 1L)
      cl <- parallel::makeCluster(n_core)
      on.exit(parallel::stopCluster(cl))
      parallel::clusterSetRNGStream(cl, iseed = iseed)
      ans <- parallel::clusterMap(cl = cl, fun = life_by, val = vals, key = keys)
    }
    else
      ans <- .mapply(FUN = life_by,
                     dots = list(val = vals, key = keys),
                     MoreArgs = list())
    ans <- vctrs::vec_rbind(!!!ans)
    if (!is_table) {
      n_at <- length(at)
      key <- vctrs::vec_rep_each(key, times = n_at)
      ans <- vctrs::vec_cbind(key, ans)
    }
  }
  else {
    ans <- life_inner_one(data = data,
                          mx_colnum = mx_colnum,
                          qx_colnum = qx_colnum,
                          at = at,
                          age_colnum = age_colnum,
                          sex_colnum = sex_colnum,
                          ax_colnum = ax_colnum,
                          methods = methods,
                          radix = radix,
                          suffix = suffix,
                          is_table = is_table)
  }
  ans
}


## HAS_TESTS
#' Check inputs, and do life table calculations,
#' for a single population (ie a single set
#' of age groups)
#'
#' @param data Data frame with mortality data.
#' @param mx_colnum Named integer vector identifying 'mx'
#' @param qx_colnum Named integer vector identifying 'qx'
#' @param at Age at which life expectancy calculated.
#' @param age_colnum Named integer vector identifying 'age'
#' @param sex_colnum Named integer vector identifying 'sex'
#' @param ax_colnum Named integer vector identifying 'ax'
#' @param by_colnums Named integer vector identifying 'by'
#' @param methods Named character vector specifying
#' calculation methods
#' @param radix Initial population for 'lx'
#' @param suffix Suffix added to new columns
#' @param is_table Whether to return full life table
#' or just life expectancy at birth
#'
#' @returns A tibble with
#' - `nrow(data)` rows and `(ncol(data) + 5)` columns,
#'    if `is_table` is `TRUE`, or
#' - `1` row and `1` column, if `is_table` is `FALSE`.
#'
#' @noRd
life_inner_one <- function(data,
                           mx_colnum,
                           qx_colnum,
                           at,
                           age_colnum,
                           sex_colnum,
                           ax_colnum,
                           methods,
                           radix,
                           suffix,
                           is_table) {
  check_age(x = data[[age_colnum]],
            complete = TRUE,
            unique = TRUE,
            zero = FALSE,
            open = TRUE)
  check_duplicated_age(data[[age_colnum]])
  ord <- order(age_lower(data[[age_colnum]]))
  data <- data[ord, , drop = FALSE]
  n <- nrow(data)
  age <- data[[age_colnum]]
  age_group_categ <- age_group_categ(age)
  has_sex <- length(sex_colnum) > 0L
  if (has_sex) {
    sex <- data[[sex_colnum]]
    sex <- reformat_sex(sex, factor = FALSE)
  }
  else {
    check_sex_not_needed(methods)
    sex <- rep(NA_character_, times = n)
  }
  has_ax <- length(ax_colnum) > 0L
  if (has_ax) {
    ax <- data[[ax_colnum]]
    check_ax(ax = ax, age = age)
    ax <- as.double(ax)
  }
  else
    ax <- rep(NA_real_, times = n)
  has_mx <- length(mx_colnum) > 0L
  if (has_mx) {
    mx <- data[[mx_colnum]]
    check_mx(mx)
    mx <- as.matrix(mx)
  }
  else {
    qx <- data[[qx_colnum]]
    check_qx(qx = qx, nm_qx = "qx")
    qx <- as.matrix(qx)
  }
  check_number(x = radix,
               x_arg = "radix",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = TRUE,
               check_whole = FALSE)
  if (!is.null(suffix))
    check_string(x = suffix, x_arg = "suffix")
  if (is_table) {
    check_age(x = age,
              complete = TRUE,
              unique = TRUE,
              zero = TRUE, ## checking for this
              open = TRUE)
    if (has_mx) {
      ans <- mx_to_lifetab(mx = mx,
                           age_group_categ = age_group_categ,
                           sex = sex,
                           ax = ax,
                           methods = methods,
                           radix = radix,
                           suffix = suffix)
      data <- data[-mx_colnum]
    }
    else {
      ans <- qx_to_lifetab(qx = qx,
                           age_group_categ = age_group_categ,
                           sex = sex,
                           ax = ax,
                           methods = methods,
                           radix = radix,
                           suffix = suffix)
      data <- data[-qx_colnum]
    }
    has_draws <- ncol(ans[[1L]]) > 1L
    if (has_draws)
      ans <- lapply(ans, rvec::rvec_dbl)
    else
      ans <- lapply(ans, as.double)
    ans <- tibble::as_tibble(ans)
    ans <- vctrs::vec_cbind(data, ans)
  }
  else { ## is lifeexp
    check_at(at = at, age = age)
    at <- as.integer(at)
    zero <- identical(min(at), 0L)
    check_age(x = age,
              complete = TRUE,
              unique = TRUE,
              zero = zero, ## checking for this
              open = TRUE)
    n_at <- length(at)
    ans <- vector(mode = "list", length = n_at)
    for (i in seq_len(n_at)) {
      at_i <- at[[i]]
      is_ge_at_i <- age_lower(age) >= at_i
      if (has_mx)
        ans[[i]] <- mx_to_ex(mx = mx[is_ge_at_i, , drop = FALSE],
                             age_group_categ = age_group_categ[is_ge_at_i],
                             sex = sex[is_ge_at_i],
                             ax = ax[is_ge_at_i],
                             methods = methods)
      else
        ans[[i]] <- qx_to_ex(qx = qx[is_ge_at_i, , drop = FALSE],
                             age_group_categ = age_group_categ[is_ge_at_i],
                             sex = sex[is_ge_at_i],
                             ax = ax[is_ge_at_i],
                             methods = methods)
    }
    ans <- do.call(rbind, ans)
    has_draws <- ncol(ans) > 1L
    if (has_draws)
      ans <- rvec::rvec_dbl(ans)
    else
      ans <- as.numeric(ans)
    ans <- tibble::tibble(ex = ans)
    if (!is.null(suffix))
      names(ans) <- paste(names(ans), suffix, sep = ".")
    if (n_at > 1L)
      ans <- vctrs::vec_cbind(tibble::tibble(at = at), ans)
  }
  ans
}


## HAS_TESTS
#' Calculate life table from values for 'mx'
#'
#' Assume all inputs have been checked and are valid.
#'
#' @param mx Numeric matrix with mortality rates
#' @param age_group_categ Character vector with
#' age group categories ("0", "1-4", "single", "five", "open")
#' @param sex Character vector with "Female", "Male", or NA_character_
#' repeated nrow(mx) times
#' @param ax Numeric vector
#' @param methods Named character vector
#' @param radix Positive number
#' @param suffix String or NULL
#'
#' @returns A named list
#'
#' @noRd
mx_to_lifetab <- function(mx,
                          age_group_categ,
                          sex,
                          ax,
                          methods,
                          radix,
                          suffix) {
    lx <- mx_to_lx(mx = mx,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    Lx <- mx_to_Lx(mx = mx,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    dx <- lx_to_dx(lx)
    qx <- lx_to_qx(lx)
    ex <- Lx_to_ex(Lx = Lx,
                   lx = lx)
    lx <- radix * lx
    dx <- radix * dx
    Lx <- radix * Lx
    ans <- list(qx = qx,
                lx = lx,
                dx = dx,
                Lx = Lx,
                ex = ex)
    if (!is.null(suffix))
        names(ans) <- paste(names(ans), suffix, sep = ".")
    ans
}
               

## HAS_TESTS
#' Calculate life table from values for 'qx'
#'
#' Assume all inputs have been checked and are valid.
#'
#' @param qx Numeric matrix with death probabilities
#' @param age_group_categ Character vector with
#' age group categories ("0", "1-4", "single", "five", "open")
#' @param sex Character vector with "Female", "Male", or NA_character_
#' repeated nrow(mx) times
#' @param ax Numeric vector
#' @param methods Named character vector
#' @param radix Positive number
#' @param suffix String or NULL
#'
#' @returns A named list
#'
#' @noRd
qx_to_lifetab <- function(qx,
                          age_group_categ,
                          sex,
                          ax,
                          methods,
                          radix,
                          suffix) {
    lx <- qx_to_lx(qx)
    Lx <- qx_to_Lx(qx = qx,
                   age_group_categ = age_group_categ,
                   sex = sex,
                   ax = ax,
                   methods = methods)
    dx <- lx_to_dx(lx)
    ex <- Lx_to_ex(Lx = Lx,
                   lx = lx)
    lx <- radix * lx
    dx <- radix * dx
    Lx <- radix * Lx
    ans <- list(qx = qx,
                lx = lx,
                dx = dx,
                Lx = Lx,
                ex = ex)
    if (!is.null(suffix))
        names(ans) <- paste(names(ans), suffix, sep = ".")
    ans
}

## HAS_TESTS
#' Remove Exisiting Life Table Columns
#'
#' Remove columns  "lx", "dx", "Lx", and "ex"
#' from data frame 'data' (if they are present).
#'
#' @param data A data frame
#' @param is_table Whether returning life table or life expectancy
#' @param suffix Optional suffix added to new
#' columns in result.
#' 
#' @returns A modified version of 'data'
#'
#' @noRd
remove_existing_lifetab_cols <- function(data, is_table, suffix) {
  if (is_table)
    nms_lifetab_cols <- c("lx", "dx", "Lx", "ex")
  else
    nms_lifetab_cols <- "ex"
  if (!is.null(suffix))
    nms_lifetab_cols <- paste(nms_lifetab_cols, suffix, sep = ".")
  nms_data <- names(data)
  nms_both <- intersect(nms_lifetab_cols, nms_data)
  if (length(nms_both) > 0L) {
    cli::cli_alert_info("Overwriting existing column{?s} {.var {nms_both}} in {.arg data}.")
    is_remove <- nms_data %in% nms_both
    data <- data[!is_remove]
  }
  data
}
                      
  
