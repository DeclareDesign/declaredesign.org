# Create a pretest-posttest design

Produces a design in which an outcome Y is observed pre- and
post-treatment. The design allows for individual post-treatment outcomes
to be correlated with pre-treatment outcomes and for at-random
missingness in the observation of post-treatment outcomes.

## Usage

``` r
pretest_posttest_designer(
  N = 100,
  ate = 0.25,
  sd_1 = 1,
  sd_2 = 1,
  rho = 0.5,
  attrition_rate = 0.1,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- ate:

  A number. Average treatment effect.

- sd_1:

  Nonnegative number. Standard deviation of period 1 shocks.

- sd_2:

  Nonnegative number. Standard deviation of period 2 shocks.

- rho:

  A number in \[-1,1\]. Correlation in outcomes between pre- and
  post-test.

- attrition_rate:

  A number in \[0,1\]. Proportion of respondents in pre-test data that
  appear in post-test data.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A pretest-posttest design.

## Details

See [vignette
online](https://declaredesign.org/r/designlibrary/articles/pretest_posttest.html).

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a pre-test post-test design using default arguments:
pretest_posttest_design <- pretest_posttest_designer()
```
