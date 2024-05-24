
#' Reformat a Binary Sex Variable
#'
#' Reformat a binary sex variable so
#' that it consists entirely of
#' values `"Female"`, `"Male"`,
#' and possibly `NA` and any values
#' included in `except`.
#'
#' When parsing labels, `reformat_sex()`
#' ignores case: `"FEMALE"` and `"fEmAlE"`
#' are equivalent.
#'
#' White space is removed from the beginning
#' and end of labels.
#'
#' `reformat_sex()` does not try to interpreting
#' numeric codes (eg `1`, `2`).
#'
#' @param x A vector.
#' @param except Values to exclude when reformatting.
#' @param factor Whether the return value
#' should be a factor.
#' 
#' @return If `factor` is `TRUE`,
#' then `reformat_age()` returns a factor;
#' otherwise it returns a character vector.
#'
#' @seealso [age_labels()], [reformat_age()]
#' 
#' @examples
#' reformat_sex(c("F", "female", NA, "MALES"))
#'
#' ## values supplied for 'except'
#' reformat_sex(c("Fem", "Other", "Male", "M"),
#'              except = c("Other", "Diverse"))
#'
#' ## return an ordinary character vector
#' reformat_sex(c("F", "female", NA, "MALES"),
#'              factor = FALSE)
#' @export
reformat_sex <- function(x, except = NULL, factor = TRUE) {
  synonyms_female <- c("females", "female", "fem", "fe", "f", "women", "girls")
  synonyms_male <- c("males", "male", "ma", "m", "men", "boys")
  has_except <- !is.null(except)
  if (has_except && !is.vector(except) && !is.factor(except))
    cli::cli_abort(c("{.arg except} is not a vector or a factor.",
                     i = "{.arg except} has class {.cls {class(except)}}."))
  except <- unique(except)
  check_flag(factor)
  is_na <- is.na(x)
  x_new <- gsub("\\s", "", x)
  x_new <- tolower(x_new)
  is_female <- x_new %in% synonyms_female
  is_male <- x_new %in% synonyms_male
  is_valid <- is_na | is_female | is_male
  if (has_except) {
    is_except <- x %in% except
    is_valid <- is_valid | is_except
  }
  i_invalid <- match(FALSE, is_valid, nomatch = 0L)
  if (i_invalid > 0L)
    cli::cli_abort("Can't parse label {.val {x[[i_invalid]]}}.")
  x <- as.character(x)
  x[is_female] <- "Female"
  x[is_male] <- "Male"
  x[is_na] <- NA
  if (factor) {
    levels <- c("Female", "Male")
    if (has_except)
      levels <- c(levels, except)
    if (any(is_na))
      levels <- c(levels, NA)
    factor(x,
           levels = levels,
           exclude = character())
  }
  else
    x
}    
    
    
