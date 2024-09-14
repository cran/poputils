
#' Functions for working with demographic data
#'
#' Functions for common tasks in demographic
#' analyses. Some functions are aimed
#' at end-users, and others at developers.
#'
#' @section For end users:
#'
#' **Data manipulation**
#'
#' - [logit()],[invlogit()] Logistic transformation
#' - [trim_01()] Trim values to interval (0, 1)
#' - [rr3()] Randomly round to base 3
#'
#' **Labels**
#' 
#' - [age_labels()] Create age labels.
#' - [age_lower()], [age_mid()], [age_upper()] Limits and midpoints of age groups
#' - [combine_age()] Merge age group labels
#' - [reformat_age()] Reformat age group labels
#' - [reformat_sex()] Reformat sex labels
#' - [set_age_open()] Specify oldest age group
#'
#' **Life expectancy, life tables**
#' 
#' - `ex_to_lifetab_brass()` Brass logit model.
#' - [lifeexp()] Life expectancy
#' - [lifetab()] Full life table
#' - [q0_to_m0()] Infant mortality
#'
#' @section For developers:
#'
#' **Checking arguments**
#'
#' - [check_n()] Check an integer scalar
#' 
#' **Data manipulation**
#' 
#' - [check_no_overlap_colnums()] Checking for argument clash
#' - [groups_colnums()] Get column numbers for grouping variables
#' - [matrix_to_list_of_cols()], [matrix_to_list_of_rows()] Split matrix
#' - [to_matrix()] Convert data frame to matrix
#'
#' **Labels**
#'
#' - [age_group_type()] Infer type of age group label
#' - [check_age()] Validity checks for age group labels
#' - [find_label_female()], [find_label_male()] Identify sex or gender labels
#' - [find_var_age()], [find_var_sexgender()], [find_var_time()] Identify age, sex/gender, time variables
#'
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib poputils, .registration = TRUE
## usethis namespace: end
NULL
