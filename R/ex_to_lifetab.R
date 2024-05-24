
#' Derive Life Tables that Match Life Expectancies,
#' using a Brass Logit Model
#'
#' Turn life expectancies at birth into full life tables,
#' using the Brass logit model. The method is simple
#' and is designed for simulations or for
#' settings with little or no data on age-specific
#' mortality rates. In settings where data
#' on age-specific mortality is available,
#' other methods might be more appropriate.
#'
#' @section Method:
#'
#' The method implemented by `ex_to_lifetab_brass()` is
#' based on the observation that, if populations A and B
#' are demographically similar, then, in many cases,
#'
#' \deqn{\text{logit}(l_x^{\text{B}}) \approx \alpha + \beta \text{logit}(l_x^{\text{A}})}
#'
#' where \eqn{l_x} is the "survivorship probability" quantity
#' from a life table. When populations are
#' similar, \eqn{beta} is often close to 1.
#'
#' Given (i) target life expectancy,
#' (ii) a set of \eqn{l_x^{\text{A}}}),
#' (referred to as a "standard"), and
#' (iii) a value for \eqn{\beta},
#' `ex_to_lifetab_brass()` finds
#' a value for \eqn{\alpha} that yields a set of
#' \eqn{l_x^{\text{B}}}) with the required life expectancy.
#'
#' @section `target` argument:
#'
#' `target` is a data frame specifying
#' life expectancies for each population being modelled,
#' and, possibly, inputs to the calculations, and
#' index variables. Values in `target` are not age-specific.
#'
#' - A variable called `"ex"`, with life expectancy at birth
#'   must be included in `target`.
#' - A variable called `"beta"` with values
#'   for `beta` can be included in `target`.
#'   This variable can be an [rvec][rvec::rvec()].
#'   If no `"beta"` variable is included in `target`,
#'   then `ex_to_lifetab_brass()` assumes that
#'   \eqn{beta \equiv 1}.
#' - A variable called `"sex"`. If the `infant`
#'   argument to `ex_to_lifetab_brass()` is is `"CD"` or `"AK"`,
#'   or if the `child` argument is `"CD"`,
#'   `target` must include a `"sex" variable, and the
#'   labels for this variable must be interpretable
#'   by function [format_sex()]. Otherwise,
#'   the `"sex"` variable  is optional,
#'   and there is no restriction on labels.
#' - Other variables used to distinguish between
#'   life expectancies, such as time, region,
#'   or model variant.
#'
#' @section `standard` argument:
#'
#' `standard` is a data frame specifying
#' the \eqn{l_x} to be used with each life expectancy
#' in `ex`, and, optionally, values the average age
#' person-years lived by people who die in each group,
#' \eqn{_na_x}. Values in `standard` are age-specific.
#'
#' - A variable called `"age"`, with labels that
#'   can be parsed by [reformat_age()].
#' - A variable called `"lx"`.
#'   Internally each set of \eqn{l_x} is are standardized
#'   so that the value for age 0 equals 1.
#'   Within each set, values must be non-increasing.
#'   Cannot be an rvec.
#' - Additional variables used to match rows in `standard`
#'   to rows in `target`.
#'
#' Internally, `standard` is merged with
#' `target` using a left join from `target`,
#' on any variables that `target`
#' and `standard` have in common.
#'
#' @param target A data frame containing a variable called
#' `"ex"`, and possibly others. See Details.
#' @param standard A data frame containing variables
#' called `age` and `lx`, and possibly others.
#' See details.
#' @param infant,child,closed,open Methods used to
#' calculate life expectancy. See [lifetab()] for details.
#' @param radix Initial population for the
#' `lx` column in the derived life table(s).
#' Default is `100000`.
#' @param suffix Optional suffix added to life table columns.
#'
#' @returns
#' A data frame containing one or more life tables.
#'
#' @seealso
#' - [logit()], [invlogit()] Logit function
#' - [lifeexp()] Calculate life expectancy from detailed inputs
#'
#' @references
#' Brass W, Coale AJ. 1968. “Methods of analysis and estimation,”
#' in Brass, W,  Coale AJ, Demeny P, Heisel DF, et al. (eds).
#' The Demography of Tropical Africa. Princeton NJ:
#' Princeton University Press, pp. 88–139.
#' 
#' Moultrie TA, Timæus IM. 2013. Introduction to Model Life Tables.
#' In Moultrie T, Dorrington R, Hill A, Hill K, Timæus I, Zaba B.
#' (eds). Tools for Demographic Estimation.
#' Paris: International Union for the Scientific Study of Population.
#' [online version](https://demographicestimation.iussp.org/content/using-models-derive-life-tables-incomplete-data).
#'
#' @examples
#' ## create new life tables based on level-1
#' ## 'West' model life tables, but with lower
#' ## life expectancy
#'
#' library(dplyr, warn.conflicts = FALSE)
#' 
#' target <- data.frame(sex = c("Female", "Male"), 
#'                      ex = c(17.5, 15.6))
#' 
#' standard <- west_lifetab |>
#'     filter(level == 1) |>
#'     select(sex, age, lx)
#'     
#' ex_to_lifetab_brass(target = target,
#'                     standard = standard,
#'                     infant = "CD",
#'                     child = "CD")
#' @export
ex_to_lifetab_brass <- function(target,
                                standard,
                                infant = c("constant", "linear", "CD", "AK"),
                                child = c("constant", "linear", "CD"),
                                closed = c("constant", "linear"),
                                open = "constant",
                                radix = 100000,
                                suffix = NULL) {
  ## check inputs
  check_target_ex_to_lifetab_brass(target)
  check_standard(standard)
  infant <- match.arg(infant)
  child <- match.arg(child)
  closed <- match.arg(closed)
  open <- match.arg(open)
  methods <- c(infant = infant,
               child = child,
               closed = closed,
               open = open)
  check_number(x = radix,
               x_arg = "radix",
               check_na = TRUE,
               check_positive = TRUE,
               check_nonneg = TRUE,
               check_whole = FALSE)
  if (!is.null(suffix))
    check_string(suffix, x_arg = "suffix")
  combined <- combine_target_standard(target = target,
                                      standard = standard)
  key <- combined$key
  n_val <- nrow(combined)
  ans_val <- vector(mode = "list", length = n_val)
  for (i in seq_len(n_val)) {
    val_i <- combined$val[[i]]
    sex_i <- key$sex[[i]] ## possibly NULL
    by_i <- key[i, , drop = FALSE]
    str_key <- make_str_key(by_i)
    return_val <- tryCatch(ex_to_lifetab_brass_one(val = val_i,
                                                   sex = sex_i,
                                                   methods = methods,
                                                   radix = radix,
                                                   suffix = suffix),
                           error = function(e) e)
    if (inherits(return_val, "error")) {
      cli::cli_abort(c(paste0("Problem with calculations for ", str_key, "."),
                       i = return_val$message,
                       return_val$body))
    }
    ans_val[[i]] <- return_val
  }
  sizes_vals <- vapply(ans_val, nrow, 0L)
  ans_val <- vctrs::vec_rbind(!!!ans_val)
  ans_by <- vctrs::vec_rep_each(key, times = sizes_vals)
  ans <- vctrs::vec_cbind(ans_by, ans_val)
  ans
}


## Helper functions -----------------------------------------------------------

## HAS_TEST
#' Merge 'target' and 'standard' data frames
#'
#' Includes
#' - checking for gaps in 'standard',
#' - adding 'beta' and 'ax' columns if necessary
#' - calling 'vec_split' at the end
#'
#' @param target,standard Data frames
#'
#' @returns A data frame with columns 'key' and 'value'
#'
#' @noRd
combine_target_standard <- function(target, standard) {
    nms_by <- intersect(names(target), names(standard))
    ans <- merge(x = target,
                 y = standard,
                 by = nms_by,
                 all.x = TRUE)
    age <- ans[["age"]]
    i_na <- match(TRUE, is.na(age), nomatch = 0L)
    if (i_na > 0L) {
        str_key <- make_str_key(ans[i_na, nms_by, drop = FALSE])
        cli::cli_abort(paste("{.arg standard} does not have values for case where", str_key))
    }
    nms_all <- names(ans)
    nms_val <- c("ex", "beta", "age", "lx", "ax")
    nms_key <- setdiff(nms_all, nms_val)
    has_beta <- "beta" %in% nms_all
    if (has_beta)
        nms_key <- c(nms_key, "beta")
    else
        ans$beta <- 1
    has_ax <- "ax" %in% nms_all
    if (!has_ax)
        ans$ax <- NA_real_
    vctrs::vec_split(ans[nms_val], ans[nms_key])
}


#' Calculate life tables, given processed inputs
#'
#' @param val Data frame with following columns:
#' - ex Numeric vector of life expectancies at birth
#' - beta Beta parameter in Brass logit model
#' - lx Standard lx for Brass logit model
#' - age Labels for age groups
#' - ax Average years lived in interval by people
#'      who die in interval. Numeric vector.
#' @param sex String or NULL.
#' @param methods Named character vectors with methods
#' for calculating life table
#' @param radix Radix for life table to be created
#' @param suffix Suffix added to columns of life table,
#' or NULL.
#'
#' @returns A data frame
#'
#' @noRd
ex_to_lifetab_brass_one <- function(val,
                                    sex,
                                    methods,
                                    radix,
                                    suffix) {
    l <- make_ex_beta_n_draw(ex = val$ex[[1L]],
                             beta = val$beta[[1L]])
    ex <- l$ex
    beta <- l$beta
    n_draw <- l$n_draw
    sex <- make_sex_ex_to_lifetab(sex = sex,
                                  methods = methods,
                                  nm_data = "standard")
    sex_rep <- if (is.null(n_draw)) sex else rep(sex, each = n_draw)
    lx_standard <- val$lx
    lx_standard <- lx_standard / lx_standard[[1L]]
    logit_lx_standard <- logit(lx_standard)
    age <- val$age
    age_group_categ <- age_group_categ(age)
    ax <- val$ax
    ## closure capturing 'logit_lx_standard',
    ## 'age_group_categ', 'ax', and 'methods'
    alpha_to_ex <- function(alpha, beta_i, sex_i) {
        logit_lx <- alpha + beta_i * logit_lx_standard
        lx <- invlogit_inner(logit_lx)
        qx <- lx_to_qx(lx)
        qx_to_ex(qx = qx,
                 age_group_categ = age_group_categ,
                 sex = sex_i,
                 ax = ax,
                 methods = methods)
    }
    n_val <- length(ex)
    lx_ans <- vector(mode = "list", length = n_val)
    for (i in seq_len(n_val)) {
        ex_i <- ex[[i]]
        beta_i <- beta[[i]]
        sex_i <- sex_rep[[i]]
        ## closure capturing 'beta_i', 'sex_i'
        abs_error <- function(alpha) {
            ex_derived <- alpha_to_ex(alpha = alpha,
                                      beta = beta_i,
                                      sex = sex_i)
            abs(ex_derived - ex_i)
        }
        val_optim <- stats::optimize(f = abs_error, interval = c(-10, 10))
        alpha_min_i <- val_optim$minimum
        logit_lx_i <- alpha_min_i + beta_i * logit_lx_standard
        lx_i <- invlogit(logit_lx_i)
        lx_ans[[i]] <- lx_i
    }
    has_draws <- !is.null(n_draw)
    if (has_draws) {
        n_by <- n_val %/% n_draw
        n_age <- length(age)
        lx_ans <- array(unlist(lx_ans), dim = c(n_age, n_by, n_draw))
        lx_ans <- apply(lx_ans, 2L, matrix, nrow = n_age, ncol = n_draw, simplify = FALSE)
    }
    ans <- vector(mode = "list", length = length(lx_ans))
    for (i in seq_along(ans)) {
        lx <- lx_ans[[i]]
        qx <- lx_to_qx(lx)
        lifetab <- qx_to_lifetab(qx = qx,
                                 age_group_categ = age_group_categ,
                                 sex = sex_rep[[i]],
                                 ax = ax,
                                 methods = methods,
                                 radix = radix,
                                 suffix = suffix)
        if (has_draws)
            lifetab <- lapply(lifetab, rvec::rvec_dbl)
        else
            lifetab <- lapply(lifetab, as.double)
        lifetab <- tibble::as_tibble(lifetab)
        lifetab <- tibble::tibble(age = age, lifetab)
        ans[[i]] <- lifetab
    }
    ans <- vctrs::vec_rbind(!!!ans)
    ans
}


## HAS_TESTS
#' Prepare 'ex', 'beta', and 'n_draw' arguments
#'
#' If one of 'ex' and 'beta' is an rvec, and the other is not
#' then the non-rvec vector is replicated
#'
#' If neither 'ex' nor 'beta' is an rvec, than 'n_draw' is NULL.
#'
#' @param ex Vector or rvec
#' @param beta Vector or rvec
#'
#' @returns A named list with 'ex', 'beta', and 'n_draw'.
#'
#' @noRd
make_ex_beta_n_draw <- function(ex, beta) {
    is_rv_ex <- rvec::is_rvec(ex)
    is_rv_beta <- rvec::is_rvec(beta)
    if (is_rv_ex && is_rv_beta) {
        n_draw_ex <- rvec::n_draw(ex)
        n_draw_beta <- rvec::n_draw(beta)
        if (n_draw_ex != n_draw_beta) {
            if (n_draw_ex == 1L) {
                ex <- as.numeric(ex)
                ex <- rep(ex, times = n_draw_beta)
                beta <- as.numeric(beta)
                n_draw <- n_draw_beta
            }
            else if (n_draw_beta == 1L) {
                ex <- as.numeric(ex)
                beta <- as.numeric(beta)
                beta <- rep(beta, times = n_draw_ex)
                n_draw <- n_draw_ex
            }
            else {
                cli::cli_abort(c("{.arg ex} and {.arg beta} have different numbers of draws.",
                                 i = "{.arg ex} has {n_draw_ex} draws.",
                                 i = "{.arg beta} has {n_draw_beta} draws."))
            }
        }
        else {
            ex <- as.numeric(ex)
            beta <- as.numeric(beta)
            n_draw <- n_draw_ex
        }
    }
    else if (!is_rv_ex && is_rv_beta) {
        n_draw <- rvec::n_draw(beta)
        ex <- rep(as.numeric(ex), times = n_draw)
        beta <- as.numeric(beta)
    }
    else if (is_rv_ex && !is_rv_beta) {
        n_draw <- rvec::n_draw(ex)
        ex <- as.numeric(ex)
        beta <- rep(as.numeric(beta), times = n_draw)
    }
    else {
        ex <- as.numeric(ex)
        beta <- as.numeric(beta)
        n_draw <- NULL
    }
    list(ex = ex,
         beta = beta,
         n_draw = n_draw)
}


## HAS_TESTS
#' Make sex variable for ex_to_lifetab function
#'
#' @param sex A character vector or NULL
#' @param A named character vector with
#' methods for calculating
#' life table quantities
#' @param nm_data Name of the input data to be
#' used in error messsages
#'
#' @returns A vector or NULL.
#'
#' @noRd
make_sex_ex_to_lifetab <- function(sex, methods, nm_data) {
    has_sex <- !is.null(sex)
    methods_need_sex <- get_methods_need_sex()
    is_needs_sex <- methods %in% methods_need_sex
    i_needs_sex <- match(TRUE, is_needs_sex, nomatch = 0L)
    if (i_needs_sex > 0L) {
        if (has_sex) {
            ans <- reformat_sex(sex, factor = FALSE)
        }
        else {
            nm_arg <- names(methods)[[i_needs_sex]]
            arg <- methods[[i_needs_sex]]
            val <- "sex"
            cli::cli_abort(c("{.arg {nm_data}} does not have a variable called {.val {val}}.",
                             i = paste("If {.arg {nm_arg}} is {.val {arg}}, then ",
                                       "{.arg {nm_data}} must have a variable called {.val {val}}.")))
        }
    }
    else
        ans <- NA_character_
    ans
}

