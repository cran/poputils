
## HAS_TESTS
#' Trim Values So They Are Between 0 and 1
#'
#' Trim a vector so that all values are greater
#' than 0 and less than 1.
#'
#' If
#' - `min` is lowest element of `x`
#'   that is higher than 0, and
#' - `max` is the highest element of `x` that is
#'   lower than 1,
#' then `trim_01()`
#' - shifts all elements of `x` that are lower than `min`
#'   upwards, so that they equal `min`, and
#' - shifts all elements of `x` that are higher than `max`
#'   downwards, so that they equal `max`.
#'
#' @param x A numeric vector. Can be an
#' [rvec][rvec::rvec()].
#'
#' @returns A trimmed version of `x`
#'
#' @seealso
#' - [logit()], [invlogit()] Logit transformation
#'
#' @examples
#' x <- c(1, 0.98, -0.001, 0.5, 0.01)
#' trim_01(x)
#' @export
trim_01 <- function(x) {
  is_rvec <- rvec::is_rvec(x)
  if (is_rvec) {
    n_draw <- rvec::n_draw(x)
    ans <- as.matrix(x)
  }
  else
    ans <- x
  if (!is.numeric(ans))
    cli::cli_abort("{.arg x} is non-numeric.")
  is_obs <- !is.na(ans)
  if (!any(is_obs)) {
    return(x)
  }
  is_too_low <- ans <= 0
  is_too_high <- ans >= 1
  is_valid <- is_obs & !is_too_low & !is_too_high
  need_to_trunc_but_cannot <- any(is_obs) && !any(is_valid)
  if (need_to_trunc_but_cannot) {
    n_outside <- sum(is_too_low, is_too_high, na.rm = TRUE)
    cli::cli_abort(c("Unable to calculate truncated values.",
                     i = paste("{.arg x} has {n_outside} value{?s} outside the interval (0, 1),",
                               "but no values inside the interval.")))
  }
  valid <- ans[is_valid]
  min <- min(valid)
  max <- max(valid)
  is_increase <- is_obs & is_too_low
  is_reduce <- is_obs & is_too_high
  ans[is_increase] <- min
  ans[is_reduce] <- max
  if (is_rvec) {
    ans <- matrix(ans, ncol = n_draw)
    ans <- rvec::rvec_dbl(ans)
  }
  ans
}
