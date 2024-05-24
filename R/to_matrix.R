
## HAS_TESTS
#' Build a Matrix from Measure and ID Variables
#'
#' Build a matrix where the elements are values of
#' a measure variable, and the rows and columns
#' are formed by observed combinations of ID
#' variables. The ID variables picked
#' out by `rows` and `cols` must uniquely identify
#' cells. `to_matrix()`, unlike `stats::xtabs()`,
#' does not sum across multiple combinations of
#' ID variables.
#'
#' @param x A data frame.
#' @param rows The ID variable(s)
#' used to distinguish rows in the matrix.
#' @param cols The ID variable(s)
#' used to distinguish columns in the matrix.
#' @param measure The measure variable, eg
#' rates or counts.
#'
#' @returns A matrix
#'
#' @examples
#' x <- expand.grid(age = c(0, 1, 2),
#'                  sex = c("F", "M"),
#'                  region = c("A", "B"),
#'                  year = 2000:2001)
#' x$count <- 1:24
#' 
#' to_matrix(x,
#'           rows = c(age, sex),
#'           cols = c(region, year),
#'           measure = count)
#'
#' to_matrix(x,
#'           rows = c(age, sex, region),
#'           cols = year,
#'           measure = count)
#'
#' ## cells not uniquely identified
#' try(
#' to_matrix(x,
#'           rows = age,
#'           cols = sex,
#'           measure = count)
#' )
#' @export
to_matrix <- function(x, rows, cols, measure) {
    ## check 'x'
    if (!is.data.frame(x))
        cli::cli_abort(c("{.arg x} is not a data frame.",
                         i = "{.arg x} has class {.cls {class(x)}}."))
    if (ncol(x) < 3L)
        cli::cli_abort(c("{.arg x} has {ncol(x)} column{?s}.",
                         i = "{.arg x} must have at least 3 columns."))
    ## make 'i_measure'
    measure <- rlang::enquo(measure)
    i_measure <- tidyselect::eval_select(measure, data = x)
    n_measure <- length(i_measure)
    if (n_measure == 0L)
        stop(gettext("no measure variable supplied"),
             call. = FALSE)
    if (n_measure > 1L)
        stop(gettextf("attempt to select %d measure variables",
                      n_measure),
             call. = FALSE)
    ## make 'i_rows'
    rows <- rlang::enquo(rows)
    i_rows <- tidyselect::eval_select(rows, data = x)
    if (length(i_rows) == 0L)
        stop(gettextf("no value supplied for '%s'",
                      "rows"),
             call. = FALSE)
    if (i_measure %in% i_rows)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(i_measure),
                      "measure",
                      "rows"),
             call. = FALSE)
    ## make 'i_cols'
    cols <- rlang::enquo(cols)
    i_cols <- tidyselect::eval_select(cols, data = x)
    if (length(i_cols) == 0L)
        stop(gettextf("no value supplied for '%s'",
                      "cols"),
             call. = FALSE)
    if (i_measure %in% i_cols)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(i_measure),
                      "measure",
                      "cols"),
             call. = FALSE)
    i_rowcol <- intersect(i_rows, i_cols)
    if (length(i_rowcol) > 0L)
        stop(gettextf("same variable ['%s'] selected by '%s' and '%s'",
                      names(x)[[i_rowcol[1L]]],
                      "rows",
                      "cols"),
             call. = FALSE)
    ## check for duplicate combinations of ID variables
    i_id <- c(i_rows, i_cols)
    id_vars <- x[i_id]
    is_dup <- duplicated(id_vars)
    i_dup <- match(TRUE, is_dup, nomatch = 0L)
    if (i_dup > 0L) {
        nms_id <- names(id_vars)
        vals_dup <- id_vars[i_dup, ]
        vals_dup <- vapply(vals_dup, as.character, "")
        vals_dup <- sprintf("\"%s\"", vals_dup)
        nms_vals <- paste(nms_id, vals_dup, sep = "=")
        nms_vals <- paste(nms_vals, collapse = ", ")
        stop(gettextf("'%s' has two rows with values c(%s)",
                      "x",
                      nms_vals),
             call. = FALSE)
    }
    ## form interactions
    paste_dot <- function(...) paste(..., sep = ".")
    var_rows <- do.call(paste_dot, x[i_rows])
    var_cols <- do.call(paste_dot, x[i_cols])
    levels_rows <- unique(var_rows)
    levels_cols <- unique(var_cols)
    ## construct matrix and return
    ans <- matrix(NA,
                  nrow = length(levels_rows),
                  ncol = length(levels_cols),
                  dimnames = list(levels_rows, levels_cols))
    i <- match(var_rows, levels_rows)
    j <- match(var_cols, levels_cols)
    ans[cbind(i, j)] <- x[[i_measure]]
    ans
}
