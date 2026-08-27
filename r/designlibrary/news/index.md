# Changelog

## DesignLibrary 0.2

- model -\> .method
- cleaner two arm
- compatibility with declaredesign updates

## DesignLibrary 0.1.10

CRAN release: 2021-10-18

- Further updates for syntax for DeclareDesign v. 0.28.0

## DesignLibrary 0.1.8

CRAN release: 2021-09-02

- Update syntax for DeclareDesign v. 0.28.0

## DesignLibrary 0.1.6

CRAN release: 2021-03-02

- Update syntax: declare_estimands -\> declare_inquiries

## DesignLibrary 0.1.5.5

CRAN release: 2020-12-01

- Removed broken URL in package documentation

## DesignLibrary 0.1.5

- fixed bug in randomized_response_designer diagnosands

- deprecated
  [`label_estimator()`](https://declaredesign.org/r/declaredesign/reference/declare_estimator.html)

## DesignLibrary 0.1.4

CRAN release: 2019-06-17

- updated tests to be compatible with rlang 0.3.0

- fixed a bug in designers that resulted from holding all args fixed

## DesignLibrary 0.1.3

CRAN release: 2019-05-02

- updated code to be compatible to new DeclareDesign version on CRAN
  (fixes a few bugs that were breaking CRAN checks)

- added dependency on `glue` for better and less `rlang`- heavy handling
  of strings

- [`construct_design_code()`](https://declaredesign.org/r/designlibrary/reference/construct_design_code.md)
  vastly improved

- changed `fixed` to `args_to_fix` and made it possible for every
  designer

- added definitions and other attributes to all designers to integrate
  better with external shiny app

- added some new error handling for cases that weren’t handled before

- improved correlation handling in
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md),
  [`mediation_analysis_designer()`](https://declaredesign.org/r/designlibrary/reference/mediation_analysis_designer.md),
  and
  [`pretest_posttest_designer()`](https://declaredesign.org/r/designlibrary/reference/pretest_posttest_designer.md)

- updated estimands in
  [`binary_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/binary_iv_designer.md)

## DesignLibrary 0.1.2

CRAN release: 2018-11-12

- Renamed and deprecated `simple_*` designers:

  - `simple_two_arm_designer() -> two_arm_designer()`,
  - `simple_spillover_designer() -> spillover_designer()`,
  - `simple_iv_designer() -> binary_iv_designer()`,
  - `simple_factorial_designer() -> two_by_two_designer()`

- Added tests for new `DeclareDesign` functionality

- Updated names of `DeclareDesign` helpers (`draw_estimates`, etc.)

- Enabled specification of block-level assignment probabilities in
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)

- Fixed a bug in
  [`factorial_designer()`](https://declaredesign.org/r/designlibrary/reference/factorial_designer.md)
  and
  [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md)
  that was breaking substitution when `fixed =` argument was used

- Small cosmetic edits to
  [`pretest_posttest_designer()`](https://declaredesign.org/r/designlibrary/reference/pretest_posttest_designer.md)

- Fixed estimator labels in
  [`multi_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/multi_arm_designer.md)

- Small updates to warning and error messages in:

  - [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
  - [`cluster_sampling_designer()`](https://declaredesign.org/r/designlibrary/reference/cluster_sampling_designer.md)

- Added PR template for contributing designers

- Improved handling of variance in
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md),
  including verbose messaging

- Imported `tidy` from `generics` following update to estimatr (\>=
  0.14.0)

## DesignLibrary 0.1.1

CRAN release: 2018-08-25

- Added designer for process-tracing designs
  ([`process_tracing_designer()`](https://declaredesign.org/r/designlibrary/reference/process_tracing_designer.md))
- Added designer for simple instrumental variables designs
  ([`simple_iv_designer()`](https://declaredesign.org/r/designlibrary/reference/binary_iv_designer.md))
- Added new arguments to
  [`regression_discontinuity_designer()`](https://declaredesign.org/r/designlibrary/reference/regression_discontinuity_designer.md)
  to allow for setting variance and shape of potential outcomes
  functions
- Added blocking to
  [`cluster_sampling_designer()`](https://declaredesign.org/r/designlibrary/reference/cluster_sampling_designer.md)
- Added more flexible ways to specify `N` in
  [`block_cluster_two_arm_designer()`](https://declaredesign.org/r/designlibrary/reference/block_cluster_two_arm_designer.md)
- Cleaned up various inconsistencies in documentation
- Cleaned up dependencies in DESCRIPTION
- Cleaner method for handling global variables in DesignLibrary.R
- Used `importFrom` to fix notes and errors in
  `check_results_DesignLibrary.html`

## DesignLibrary 0.1.0

CRAN release: 2018-08-09

- First CRAN version
