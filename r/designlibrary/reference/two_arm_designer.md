# Create a one-level two-arm design

Builds a design with one treatment and one control arm. Treatment
effects can be specified either by providing `control_mean` and
`treatment_mean` or by specifying a `control_mean` and `ate`.

## Usage

``` r
two_arm_designer(
  N = 100,
  assignment_prob = 0.5,
  control_mean = 0,
  control_sd = 1,
  ate = 1,
  treatment_mean = control_mean + ate,
  treatment_sd = control_sd,
  rho = 1,
  args_to_fix = NULL
)
```

## Arguments

- N:

  An integer. Sample size.

- assignment_prob:

  A number in \[0,1\]. Probability of assignment to treatment.

- control_mean:

  A number. Average outcome in control.

- control_sd:

  A positive number. Standard deviation in control.

- ate:

  A number. Average treatment effect.

- treatment_mean:

  A number. Average outcome in treatment. Overrides `ate` if both
  specified.

- treatment_sd:

  A nonnegative number. Standard deviation in treatment. By default
  equals `control_sd`.

- rho:

  A number in \[-1,1\]. Correlation between treatment and control
  outcomes.

- args_to_fix:

  A character vector. Names of arguments to be args_to_fix in design.

## Value

A simple two-arm design.

## Details

Units are assigned to treatment using complete random assignment.
Potential outcomes are normally distributed according to the mean and sd
arguments.

## Author

[DeclareDesign Team](https://declaredesign.org/)

## Examples

``` r
# Generate a simple two-arm design using default arguments
two_arm_design <- two_arm_designer()
two_arm_design <- two_arm_designer(args_to_fix = c("N", "assignment_prob"))
```
