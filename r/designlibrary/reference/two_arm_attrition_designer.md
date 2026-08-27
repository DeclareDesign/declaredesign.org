# Create design with risk of attrition or post treatment conditioning

Creates a two-arm design with application for when estimand of interest
is conditional on a post-treatment outcome (the effect on Y given R) or
data is conditionally observed (Y given R). See \`Details\` for more
information on the data generating process.

## Usage

``` r
two_arm_attrition_designer(
  N = 100,
  a_R = 0,
  b_R = 1,
  a_Y = 0,
  b_Y = 1,
  rho = 0,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Size of sample.

- a_R:

  A number. Constant in equation relating treatment to responses.

- b_R:

  A number. Slope coefficient in equation relating treatment to
  responses.

- a_Y:

  A number. Constant in equation relating treatment to outcome.

- b_Y:

  A number. Slope coefficient in equation relating treatment to outcome.

- rho:

  A number in \[0,1\]. Correlation between shocks in equations for R and
  Y.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A post-treatment design.

## Details

The data generating process is of the form:

`R ~ (a_R + b_R*Z > u_R)`

`Y ~ (a_Y + b_Y*Z > u_Y)`

where `u_R` and `u_Y` are joint normally distributed with correlation
`rho`.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# To make a design using default argument (missing completely at random):
two_arm_attrition_design <- two_arm_attrition_designer()
if (FALSE) { # \dontrun{
diagnose_design(two_arm_attrition_design)
} # }
# Attrition can produce bias even for unconditional ATE even when not
# associated with treatment
if (FALSE) { # \dontrun{
diagnose_design(two_arm_attrition_designer(b_R = 0, rho = .3))
} # }
# Here the linear estimate using R=1 data is unbiased for
# "ATE on Y (Given R)" with b_R = 0 but not when  b_R = 1   
if (FALSE) { # \dontrun{
diagnose_design(redesign(two_arm_attrition_design, b_R = 0:1, rho = .2))
} # }
```
