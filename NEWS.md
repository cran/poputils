
# poputils 0.4.1

* Modify C++ function `qx_to_ex()` to avoid possible memory leekage
  found by valgrind.
  

# poputils 0.4.0

## Interface

* If `data` already contains columns called `"lx"`, `"Lx"`, `"dx"`,
  `"ex"`, then `lifetab()` overwrites these columns, with a message,
  rather than creating new versions.
* `at` argument to `lifeexp()` can now be a vector with length
  > 1. In this case, `lifeexp()` calculates life expectancy for each
  value (within each combination of the 'by' variables, if present.)
* Added `n_core` argument to `lifetab()` and `lifeexp()`. Setting
  `n_core` to a value greater than 1 triggers parallel processing.
* Added `closed` argument to `check_age()`, to check whether the
  oldest age group is closed.
* Added function `tfr()` for calculating total fertility rates.
* Extended vignette.


## Bug fixes

* `combine_age()` previously not working properly if `"to"` is
  `"five"`, and lower limit of youngest age group in `x` not divisible
  by 5.
* `ex_to_lifetab_brass()` previously assumed, without checking that
  age groups were correctly ordered. `ex_to_lifetab_brass()` now
  automatically reorders them.


# poputils 0.3.3

## Bug fixes

* Previously calling `lifetab()` with value supplied for `qx` created
  a duplicate `qx` column in the result. The extra column has been
  removed.
  

## Interface

* Added `at` argument to `lifeexp()`.
* Removed `mx` column from output of `lifetab()` when calculated from
  mx (to be consistent with behaviour of `lifetab()` when calculated
  from qx.)


## New functions

* Added function `check_n()`.
* Added function `q0_to_m0()`.
* Added function `rr3()`.
* Added function `trim_01()`.


## Internal

* Removed `stop(gettextf(` style error messages, except in functions
  involving age (which will be superceded by package **agetime**.


# poputils 0.3.2

## Internal 

* Removed all uses of `rvec::rvec.is.numeric()`, in preparation for
  removing from **rvec** package.
  
## Bug fixes

* Fixed bug in `combine_age()`. Previously giving wrong answers when
  last age group was closed.

# poputils 0.3.1

## Bug fixes

* Fixed "lifetab.cpp:455:16: runtime error: inf is outside the
  range of representable values of type 'int'" - changed nx from int
  to double

# poputils 0.3.0

* Released on to CRAN

# poputils 0.2.0

* Coverage of tests 100 percent

