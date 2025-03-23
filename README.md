
<!-- README.md is generated from README.Rmd. Please edit that file -->

# poputils <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bayesiandemography/poputils/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bayesiandemography/poputils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bayesiandemography/poputils?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/poputils)](https://CRAN.R-project.org/package=poputils)
<!-- badges: end -->

Manipulate and analyse demographic data.

## Installation

``` r
install.packages("poputils")
```

## For end users

### Data manipulation

- `logit()`, `invlogit()` Logistic transformation
- `trim_01()` Trim values to interval (0, 1)
- `rr3()` Randomly round to base 3

### Life expectancy, life tables

- `ex_to_lifetab_brass()` Use the Brass logit model to derive life
  tables with specified life expectancies
- `lifeexp()` Calculate life expectancy from mortality rates
- `lifetab()` Calculate life tables from mortality rates
- `q0_to_m0()` Infant mortality

### Fertility

- `tfr()` Calculate total fertility rates

### Labels

- `age_labels()` Create age labels
- `age_lower()`, `age_mid()`, `age_upper()` Limits and midpoints of age
  groups
- `combine_age()` Merge age group labels
- `reformat_age()` Reformat age group labels
- `reformat_sex()` Reformat sex labels
- `set_age_open()` Specify oldest age group

## For developers

### Checking arguments

- `check_n()` Check an integer scalar.

### Data manipulation

- `check_no_overlap_colnums()` Check for argument clashes
- `groups_colnums()` Get column numbers for grouping variables
- `matrix_to_list_of_cols()`, `matrix_to_list_of_rows()` Split matrix
- `to_matrix()` Convert data frame to matrix

### Labels

- `age_group_type()` Infer type of age group label
- `check_age()` Validity checks for age group labels
- `find_label_female()`, `find_label_male()` Identify sex or gender
  labels
- `find_var_age()`, `find_var_sexgender()`, `find_var_time()` Identify
  age, sex/gender, time variables
