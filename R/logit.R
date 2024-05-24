
## HAS_TESTS
#' Logit and Inverse-Logit Functions
#'
#' Transform values to and from the logit scale.
#' `logit()` calculates
#'
#' \deqn{x = \log \left(\frac{p}{1 - p}\right)}
#'
#' and `invlogit()` calculates
#'
#' \deqn{p = \frac{e^x}{1 + e^x}}
#'
#' @details
#'
#' To avoid overflow, `invlogit()`
#' uses \eqn{p = \frac{1}{1 + e^{-x}}}
#' internally for \eqn{x} where \eqn{x > 0}.
#'
#' In some of the demographic literature,
#' the logit function is defined as
#'
#' \deqn{x = \frac{1}{2} \log \left(\frac{p}{1 - p}\right).}
#'
#' `logit()` and `invlogit()` follow the conventions
#' in statistics and machine learning, and omit the
#' \eqn{\frac{1}{2}}.
#'
#' @param p Values in the interval `[0, 1]`.
#' Can be an atomic vector, a matrix,
#' or an [rvec][rvec::rvec()].
#' @param x Values in the interval `(-Inf, Inf)`.
#' Can be an atomic vector, a matrix,
#' or an [rvec][rvec::rvec()].
#'
#' @returns
#' - A vector of doubles, if `p` or `x` is a vector.
#' - A matrix of doubles, if `p` or `x` is a matrix.
#' - An object of class `rvec_dbl`, if `p` or `x` is an rvec.
#'
#' @examples
#' p <- c(0.5, 1, 0.2)
#' logit(p)
#' invlogit(logit(p))
#' @export
logit <- function(p) {
  if (!is.numeric(p))
    cli::cli_abort(c("{.arg p} is not numeric.",
                     i = "{.arg p} has class {.cls {class(p)}}."))
  if (rvec::is_rvec(p)) {
    p <- as.matrix(p)
    ans <- Recall(p)
    ans <- rvec::rvec_dbl(ans)
  }
  else if (is.array(p)) {
    ans <- p
    ans[] <- logit_inner(p)
  }
  else if (is.atomic(p)) {
    ans <- logit_inner(p)
    names(ans) <- names(p)
  }
  else {
    cli::cli_abort("Can't handle class {.cls {class(p)}}.") ## nocov
  }
  ans
}

#' @export
#' @rdname logit
invlogit <- function(x) {
  if (!is.numeric(x))
    cli::cli_abort(c("{.arg x} is not numeric.",
                     i = "{.arg x} has class {.cls {class(x)}}."))
  if (rvec::is_rvec(x)) {
    x <- as.matrix(x)
    ans <- Recall(x)
    ans <- rvec::rvec_dbl(ans)
  }
  else if (is.array(x)) {
    ans <- x
    ans[] <- invlogit_inner(x)
  }
  else if (is.atomic(x)) {
    ans <- invlogit_inner(x)
    names(ans) <- names(x)
  }
  else {
    cli::cli_abort("Can't handle class {.cls {class(x)}}.") ## nocov
  }
  ans
}





