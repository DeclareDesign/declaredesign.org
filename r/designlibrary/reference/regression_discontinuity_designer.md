# Create a regression discontinuity design

Builds a design with sample from population of size `N`. The average
treatment effect local to the cutpoint is equal to `tau`. It allows for
specification of the order of the polynomial regression
(`poly_reg_order`), cutoff value on the running variable (`cutoff`), and
size of bandwidth around the cutoff (`bandwidth`). By providing a vector
of numbers to `control_coefs` and `treatment_coefs`, users can also
specify polynomial regression coefficients that generate the expected
control and treatment potential outcomes given the running variable.

## Usage

``` r
regression_discontinuity_designer(
  N = 1000,
  tau = 0.15,
  outcome_sd = 0.1,
  cutoff = 0.5,
  bandwidth = 0.5,
  control_coefs = c(0.5, 0.5),
  treatment_coefs = c(-5, 1),
  poly_reg_order = 4,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of population to sample from.

- tau:

  A number. Difference in potential outcomes functions at the threshold.

- outcome_sd:

  A positive number. The standard deviation of the outcome.

- cutoff:

  A number in (0,1). Threshold on running variable beyond which units
  are treated.

- bandwidth:

  A number. The value of the bandwidth on both sides of the threshold
  from which to include units.

- control_coefs:

  A vector of numbers. Coefficients for polynomial regression function
  that generates control potential outcomes. Order of polynomial is
  equal to length.

- treatment_coefs:

  A vector of numbers. Coefficients for polynomial regression function
  that generates treatment potential outcomes. Order of polynomial is
  equal to length.

- poly_reg_order:

  Integer greater than or equal to 1. Order of the polynomial regression
  used to estimate the jump at the cutoff.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A regression discontinuity design.

## Details

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/regression_discontinuity.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a regression discontinuity design using default arguments:
regression_discontinuity_design <- regression_discontinuity_designer()
```
