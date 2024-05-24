

## HAS_TESTS
#' Get a named vector of column indices
#' for the grouping variables in a
#' grouped data frame
#'
#' Constructed a named vector of indices
#' equivalent to the vectors produced by
#' tidyselect::eval_select, but for the grouping
#' variables in an object of class
#' "grouped_df".
#'
#' If `data` is not grouped, then `groups_colnums`
#' returns a zero-length vector.
#' 
#' @param data A data frame.
#'
#' @returns A named integer vector.
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(x = 1:4,
#'                  g = c(1, 1, 2, 2))
#' groups_colnums(df)
#' df <- group_by(df, g)
#' groups_colnums(df)
#' @export
groups_colnums <- function(data) {
    if (!is.data.frame(data))
        cli::cli_abort(c("{.arg data} is not a data frame.",
                         i = "{.arg data} has class {.cls {class(data)}}."))
    attr <- attributes(data)
    has_groups <- "groups" %in% names(attr)
    if (has_groups) {
        nms_data <- attr$names
        groups <- attr$groups
        nms_groups <- setdiff(colnames(groups), ".rows")
        ans <- match(nms_groups, nms_data)
        names(ans) <- nms_groups
    }
    else {
        ans <- integer()
        names(ans) <- character()
    }
    ans
}


## HAS_TESTS
#' Turn a row from a data frame into a key=val string,
#' with mark-up expected by cli::cli_abort
#'
#' @param row A single row from a data frame
#'
#' @returns A string
#'
#' @noRd
make_str_key <- function(row) {
    key <- names(row)
    key <- sprintf("{.arg %s}", key)
    val <- vapply(row, format, "")
    val <- sprintf("{.val %s}", val)
    ans <- paste(key, val, sep = "=")
    ans <- paste(ans, collapse = ", ")
    ans
}
    

## HAS_TESTS
#' Turn a Matrix Into a List of Columns or Rows
#'
#' Given a matrix, create a list, each element of which contains
#' a column or row from the matrix.
#'
#' `matrix_to_list_of_cols()` and `matrix_to_list_of_rows() are
#' internal functions, for use by developers, and would not
#' normally be called directly by end users.
#' 
#' @param m A matrix
#'
#' @return
#' - `matrix_to_list_of_cols()` A list of vectors,
#'   each of which is a column from `x`.
#' - `matrix_to_list_of_rows()`, A list of vectors,
#'   each of which is a row from `x`.
#'
#' @examples
#' m <- matrix(1:12, nrow = 3)
#' matrix_to_list_of_cols(m)
#' matrix_to_list_of_rows(m)
#' @export
matrix_to_list_of_cols <- function(m) {
    if (!is.matrix(m))
        cli::cli_abort(c("{.arg m} is not a matrix.",
                         i = "{.arg m} has class {.cls {class(m)}}."))
    if (ncol(m) > 0L) {
        ans <-  apply(m,
                      MARGIN = 2L,
                      FUN = identity,
                      simplify = FALSE)
        names(ans) <- colnames(m)
    }
    else
        ans <- list()
    ans
}

#' @export
#' @rdname matrix_to_list_of_cols
matrix_to_list_of_rows <- function(m) {
    if (!is.matrix(m))
        cli::cli_abort(c("{.arg m} is not a matrix.",
                         i = "{.arg m} has class {.cls {class(m)}}."))
    if (nrow(m) > 0L) {
        ans <- apply(m,
                     MARGIN = 1L,
                     FUN = identity,
                     simplify = FALSE)
        names(ans) <- rownames(m)
    }
    else
        ans <- list()
    ans
}

