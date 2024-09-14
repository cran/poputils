
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

