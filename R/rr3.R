
## HAS_TESTS
#' Randomly Round A Vector of Integers to Base 3
#'
#' Apply the 'Random Round to Base 3' (RR3)
#' algorithm to a vector of integers
#' (or doubles where `round(x) == x`.
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
    x <- as.numeric(x)
  }
  is_x_integer <- is.integer(x)
  is_x_integerish <- is_x_integer || all(round(x) == x, na.rm = TRUE)
  if (!is_x_integerish)
    cli::cli_abort("{.arg x} has non-integer values.")
  if (max(x, na.rm = TRUE) > largest_int)
    cli::cli_abort(c(paste("Maximum value in {.arg x} greater than largest integer",
                           "that can be represented on this machine."),
                     i = "Maximum value in {.arg x}: {.val {max(x, na.rm = TRUE)}}.",
                     i = "Largest integer: {.val {largest_int}}."))
  x_mod_3 <- as.integer(x) %% 3L
  n <- length(x)
  p <- stats::runif(n = n)
  ## deal with NAs - leave untouched
  is_processed <- is.na(x)
  ## deal with values divisible by 3 - leave untouched
  is_remainder_0 <- !is_processed & (x_mod_3 == 0L)
  is_processed <- is_processed | is_remainder_0
  ## deal with mod 1, which has 2/3 chance of rounding down,
  ## 1/3 chance of rounding up
  is_remainder_1 <- !is_processed & (x_mod_3 == 1L)
  is_round_down <- is_remainder_1 & (p < 2/3)
  is_round_up <- is_remainder_1 & (p >= 2/3)
  x[is_round_down] <- x[is_round_down] - 1L
  x[is_round_up] <- x[is_round_up] + 2L
  is_processed <- is_processed | is_remainder_1
  ## deal with mod 2, which has 1/3 chance of rounding down,
  ## 2/3 chance of rounding up
  is_remainder_2 <- !is_processed
  is_round_down <- is_remainder_2 & (p < 1/3)
  is_round_up <- is_remainder_2 & (p >= 1/3)
  x[is_round_down] <- x[is_round_down] - 2L
  x[is_round_up] <- x[is_round_up] + 1L
  ## convert back to rvec if was originally rvec
  if (is_rvec) {
    x <- matrix(x, ncol = n_draw)
    x <- rvec::rvec(x)
  }
  ## return
  x
}
