
## Functions to infer variable types or labels from vectors of names

## Find labels ----------------------------------------------------------------

## HAS_TESTS
#' Identify Sex or Gender Labels Referring to Females
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to females.
#' If no elements look like a label for females,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_label_male()], [find_var_sexgender()]
#' 
#' @examples
#' find_label_female(c("Female", "Male")) ## one valid
#' find_label_female(c("0-4", "5-9"))     ## none valid
#' find_label_female(c("F", "Fem"))       ## two valid
#' @export
find_label_female <- function(nms) {
    p_valid <- paste0("^female$|^f$|^fem$|^women$|^girl$|",
                      "^females$|^girls")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify Sex or Gender Labels Referring to Males
#'
#' Given labels for sex or gender, try to infer
#' which (if any) refer to males.
#' If no elements look like a label for males,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms` or `NULL`.
#'
#' @seealso [find_label_female()], [find_var_sexgender()]
#' 
#' @examples
#' find_label_male(c("Female", "Male")) ## one valid
#' find_label_male(c("0-4", "5-9"))     ## none valid
#' find_label_male(c("male", "m"))      ## two valid
#' @export
find_label_male <- function(nms) {
    p_valid <- paste0("^male$|^m$|^men$|^boy$|",
                      "^males$|^boys")
    find_inner(nms = nms, p_valid = p_valid)
}


## Find variables -------------------------------------------------------------

## HAS_TESTS
#' Identify an Age Variable
#'
#' Find the element of `nms` that looks like an age variable.
#' If no elements look like an age variable, or if
#' two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_time()], [find_var_sexgender()]
#' 
#' @examples
#' find_var_age(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_age(c("Sex", "Year"))                 ## none valid
#' find_var_age(c("age", "age.years"))            ## two valid
#' @export
find_var_age <- function(nms) {
    p_valid <- paste0("^age$|^agegroup$|^agegp$|^ageyear$|^ageinterval$|",
                      "^ages$|^agegroups$|^agegps$|^ageyears$|^ageintervals$")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify a Sex or Gender Variable
#'
#' Find the element of `nms` that looks like
#' a sex or gender variable.
#' If no elements look like a sex or gender variable,
#' or if two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_age()], [find_var_time()], [find_label_female()],
#' [find_label_male()]
#' 
#' @examples
#' find_var_sexgender(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_sexgender(c("Age", "Region"))               ## none valid
#' find_var_sexgender(c("sexgender", "sexes"))          ## two valid
#' @export
find_var_sexgender <- function(nms) {
    p_valid <- paste0("^sex$|^gender$|^sexgender$|",
                      "^sexes$|^genders$")
    find_inner(nms = nms, p_valid = p_valid)
}


## HAS_TESTS
#' Identify a Time Variable
#'
#' Find the element of `nms` that looks like an time variable.
#' If no elements look like a time variable, or if
#' two or more elements do,
#' then return `NULL`.
#'
#' @param nms A character vector
#'
#' @returns An element of `nms`, or `NULL`.
#'
#' @seealso [find_var_age()], [find_var_sexgender()]
#' 
#' @examples
#' find_var_time(c("Sex", "Year", "AgeGroup", NA)) ## one valid
#' find_var_time(c("Sex", "Region"))               ## none valid
#' find_var_time(c("time", "year"))                ## two valid
#' @export
find_var_time <- function(nms) {
    p_valid <- paste0("^time$|^period$|^year$|^month$|^quarter$|",
                      "^times$|^periods$|^years$|^months$|^quarters$|",
                      "^yearmonth$|^monthyear$|^yearquarter$|^quarteryear$")
    find_inner(nms = nms, p_valid = p_valid)
}



## Helper functions -----------------------------------------------------------

## HAS_TESTS
#' Helper function for find_* functions
#'
#' @param nms A character vector
#' @param p_valid A regular expression used
#' to identify valid members of 'nms'
#'
#' @returns An element of 'nms' or NULL.
#'
#' @noRd
find_inner <- function(nms, p_valid) {
    nms_cleaned <- tolower(nms)
    nms_cleaned <- gsub("[^a-z]", "", nms_cleaned)
    i <- grep(p_valid, nms_cleaned)
    if (identical(length(i), 1L))
        nms[[i]]
    else
        NULL
}
