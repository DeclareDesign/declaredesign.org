# Changelog

## estimatr 1.0.4

CRAN release: 2024-03-31

- Test suite changes for M1 mac stay current on CRAN.

## estimatr 1.0.2

CRAN release: 2024-01-17

- Minor documentation changes to stay current on CRAN.

## estimatr 1.0.0

CRAN release: 2022-07-04

- Version bump to coincide with DeclareDesign package version 1.0.0
- Tests edited

## estimatr 0.30.6

CRAN release: 2022-01-31

- Fix tests to address CRAN failures

## estimatr 0.30.4

CRAN release: 2021-11-07

- Bug fix of tidy handling of conf.level
- Bug fix of lh_robust tidy

## estimatr 0.30.2

CRAN release: 2021-01-17

- Remove lfe from tests

## estimatr 0.30.0

CRAN release: 2021-01-07

- Test suite changes (skip if not installed for checking against other
  packages)

## estimatr 0.28.0

CRAN release: 2020-11-19

- Test suite changes

## estimatr 0.26.0

CRAN release: 2020-09-07

- Test suite changes

## estimatr 0.24.0

CRAN release: 2020-09-02

- tidy: rename nobs, nclusters, nblocks
- tidy: new arguments conf.int, conf.level
- Added `update.iv_robust()`
- Bug fix regarding fixed effects with large numbers

## estimatr 0.22.0

CRAN release: 2020-03-19

- Bug fixes

## estimatr 0.20.0

CRAN release: 2019-09-09

- Added support for `emmeans` (thanks
  [@rvlenth](https://github.com/rvlenth))!
- Fixed bug when estimating `diagnostics` in
  [`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md)
  without explicitly specifying `se_type` (issue
  [\#310](https://github.com/DeclareDesign/estimatr/issues/310))
- Support for `rlang` 0.4.0

## estimatr 0.18.0

CRAN release: 2019-05-26

- Fixed bug where collinear covariates caused fixed effects estimator to
  crash (issue
  [\#294](https://github.com/DeclareDesign/estimatr/issues/294))
- Added
  [`glance.lh_robust()`](https://declaredesign.org/r/estimatr/reference/estimatr_glancers.md)
  and fixed some issues with printing and summarizing
  [`lh_robust()`](https://declaredesign.org/r/estimatr/reference/lh_robust.md)
  objects (issues
  [\#295](https://github.com/DeclareDesign/estimatr/issues/295) and
  [\#296](https://github.com/DeclareDesign/estimatr/issues/296))
- Fixes CRAN errors in testing with new `clubSandwich` package

## estimatr 0.16.0

- Add `diagnostics` to
  [`iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.md)
- Add [`glance()`](https://generics.r-lib.org/reference/glance.html)
  methods for all estimators
- Add
  [`lh_robust()`](https://declaredesign.org/r/estimatr/reference/lh_robust.md)
  for easy interface to
  [`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html)
- Fixed minor bug with a formula such as `is.na(var)` in the
  `covariates` formula in
  [`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
  (issue [\#283](https://github.com/DeclareDesign/estimatr/issues/283))

## estimatr 0.14.0

- Removes `broom` hack for `tidy` method and instead relies on importing
  `generics`

## estimatr 0.12.0

- Fixed ambiguity about how interacted covariates were centered in
  `lm_lin`
- A series of fixes for bugs that occurred with multiple outcomes
  (multivariate regression):
  - Fixed bug pointed out by James Pustejovsky via the `sandwich`
    version 2.5-0 and off-diagonal blocks of multivariate regression
    vcov matrices
  - Fixed bugs in `lm_lin` preventing multivariate regression
  - Fixed bug that truncated degrees of freedom with “CR2” standard
    errors
  - Fixed bug that returned incorrect R-squared for the second or later
    outcomes
- Fixed bug preventing integration with latest version of `margins`
- Fixed bug with `difference_in_means` when using `condition1` and
  `condition2` to subset a treatment vector with more than two treatment
  conditions. Previous estimates and standard errors were incorrect.

## estimatr 0.10.0

CRAN release: 2018-07-12

- Changed names of confidence interval columns in tidied data from
  `ci.lower` and `ci.upper` to `conf.low` and `conf.high` to be in line
  with other tidy methods
- Added support for `fixed_effects` that are just one block
- Added support for specifing `condition_prs` in
  [`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)
  as a single number
- Added t- and z-statistics to output
- Limit unnecessary messaging in
  [`horvitz_thompson()`](https://declaredesign.org/r/estimatr/reference/horvitz_thompson.md)

## estimatr 0.8.0

CRAN release: 2018-06-02

- Added support for absorbing fixed effects in `lm_robust` and
  `iv_robust`
- Added `commarobust` and `starprep` for stargazer integration
- Added `texreg` support for 2SLS IV models
- Fixed bugs for incorrect F-statistics with robust standard errors
- Refactor of main fitting engine for linear models

## estimatr 0.6.0

CRAN release: 2018-03-28

- Added support for multivariate linear models
- Added support for instrumental variables regression
- Major change to name of object output elements to mostly match with
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html)
  - old -\> new
  - `coefficient_names` -\> `term`
  - `se` -\> `std.error`
  - `p` -\> `p.values`
  - `ci_lower` -\> `ci.lower`
  - `ci_upper` -\> `ci.upper`
  - All of the above changes are also made to the column names on the
    output of `tidy`; furthermore for `tidy` objects one further name
    change from `coefficients` -\> `estimate` has been made
- Fixed bug that caused variances, standard errors, and p-values to be
  wrong for weighted “CR2” variance estimation
- Fixed incorrect estimates when both weights and blocks were passed to
  `difference_in_means`
- Rewrite NSE handling to be done by `rlang`
- Rewrite `na.omit` handler in R
- Major refactor of C++ underlying regression estimators

## estimatr 0.4.0

CRAN release: 2018-02-15

- Changed suffix added to centered variables in
  [`lm_lin()`](https://declaredesign.org/r/estimatr/reference/lm_lin.md)
  from `_bar` to `_c`
- Added all vignettes to `.Rbuildignore`, only available on website now
- Fixed `lm_robust_helper.cpp` algorithm to not catch own exception and
  to deal with `valgrind` memory errors
- Bugfix where passing a formula as an object within a function would
  fail
- Simplified some tests for various CRAN test platforms

## estimatr 0.2.0

CRAN release: 2018-01-29

- First **CRAN** upload
