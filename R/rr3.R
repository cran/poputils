
## HAS_TESTS
#' Randomly Round A Vector of Integers to Base 3
#'
#' Apply the 'Random Round to Base 3' (RR3)
#' algorithm to a vector of integers
#' (or doubles where `round(x) == x`.)
#' 
#' The RR3 algorithm is used by statistical
#' agencies to confidentialize data. Under the
#' RR3 algorithm, an integer \eqn{n}
#' is randomly rounded as follows:
#'
#' - If \eqn{n} is divisible by 3, leave it unchanged
#' - If dividing \eqn{n} by 3 leaves a remainder of 1, then
#'   round down (subtract 1) with probability 2/3,
#'   and round up (add 2) with probability 1/3.
#' - If dividing \eqn{n} by 3 leaves a remainder of 1,
#'   then round down (subtract 2)
#'   with probability 1/3, and round up (add 1)
#'   with probability 2/3.
#'
#' RR3 has some nice properties:
#' - The randomly-rounded version of \eqn{n}
#'   has expected value \eqn{n}.
#' - If \eqn{n} non-negative, then the randomly
#'   rounded version of \eqn{n} is non-negative.
#' - If \eqn{n} is non-positive, then the randomly
#'   rounded version of \eqn{n} is non-positive.
#' 
#' @param x A vector of integers (in the
#' sense that `round(x) == x`.) Can be an
#' [rvec][rvec::rvec()].
#'
#' @returns A randomly-rounded version of `x`.
#'
#' @examples
#' x <- c(1, 5, 2, 0, -1, 3, NA)
#' rr3(x)
#' @export
rr3 <- function(x) {
  largest_int <- .Machine$integer.max
  is_rvec <- rvec::is_rvec(x)
  if (is_rvec) {
    n_draw <- rvec::n_draw(x)
    x <- as.matrix(x)
  }
  if (!is.numeric(x))
    cli::cli_abort("{.arg x} must be numeric or integer.")
  if (any(is.infinite(x)))
    cli::cli_abort("{.arg x} has non-finite values.")
  max_x <- suppressWarnings(max(x, na.rm = TRUE))
  if (max_x > largest_int) {
    cli::cli_abort(c(
      "Maximum value in {.arg x} greater than largest representable integer.",
      i = "Maximum value in {.arg x}: {.val {max_x}}.",
      i = "Largest integer: {.val {largest_int}}."
    ))
  }
  is_integerish <- is.integer(x) || all(round(x) == x, na.rm = TRUE)
  if (!is_integerish)
    cli::cli_abort("{.arg x} has non-integer values.")
  is_double <- is.double(x)
  x <- as.integer(x)
  ans <- .Call(`_poputils_rr3_inner`, x, is_rvec)
  if (is_double)
    ans <- as.double(ans)
  if (is_rvec) {
    ans <- matrix(ans, ncol = n_draw)
    ans <- rvec::rvec(ans)
  }
  ans
}
